;;;; the instruction set definition for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;; Various helpers
(defun register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'registers)))

(defun vfp-register-p (thing)
  (and (tn-p thing)
       (eq (sb-name (sc-sb (tn-sc thing))) 'float-registers)))


;; 
;; Printers for the various instruction arguments

;; 'Conditional' field of the instructions. Print the symbolic name of the
;; condition. Do not print :AL and :XX to keep the output clean.
(defparameter *cond-values-alist*
  '((:eq .  0) (:ne .  1) (:cs .  2) (:cc .  3)
    (:mi .  4) (:pl .  5) (:vs .  6) (:vc .  7)
    (:hi .  8) (:ls .  9) (:ge . 10) (:lt . 11)
    (:gt . 12) (:le . 13) (:al . 14) (:xx . 15)))

(defun cond-encoding (condition)
  (cdr (assoc condition *cond-values-alist* :test #'eq)))

(sb!disassem:define-arg-type cnd
    :sign-extend nil
    :printer #'(lambda (value stream dstate)
                 (declare (ignore dstate)
                          (type stream stream) (type fixnum value))
                 (unless (member value '(14 15))
                   (princ (or (car (rassoc value *cond-values-alist*)) value)
                          stream))))

;; Register fields of the instructions. Print the name of the register (e.g. R0)
;; and maybe some associated storage reference information (e.g. R0 = X) as a
;; comment at the end of line.
(sb!disassem:define-arg-type reg
    :printer #'(lambda (value stream dstate)
                 (declare (type stream stream) (fixnum value))
                 (let ((regname (aref *register-names* value)))
                   (princ regname stream)
                   (sb!disassem:maybe-note-associated-storage-ref
                    value 'registers regname dstate))))

(defun reg-tn-encoding (tn)
  (declare (type tn tn))
  (aver (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
  (tn-offset tn))

(defun freg-tn-encoding (tn)
  (declare (type tn tn))
  (aver (eq (sb-name (sc-sb (tn-sc tn))) 'float-registers))
  (tn-offset tn))

;; 12-bit modified immediate constants, see Section A5.2.4
(defun immed12-encoding (immed)
  (macrolet ((frob (const-mask rot-mask rot-bits)
               `(when (= immed (logand immed ,const-mask))
                  (return-from immed12-encoding
                    (logior ,rot-mask
                            (logior
                             (ldb (byte 8 0) (ash immed ,rot-bits))
                             (ldb (byte 8 0) (ash immed ,(- rot-bits 32))))))))
             )
    (frob #x000000ff #x000  0)
    (frob #xc000003f #x100  2)
    (frob #xf000000f #x200  4)
    (frob #xfc000003 #x300  6)
    (frob #xff000000 #x400  8)
    (frob #x3fc00000 #x500 10)
    (frob #x0ff00000 #x600 12)
    (frob #x03fc0000 #x700 14)
    (frob #x00ff0000 #x800 16)
    (frob #x003fc000 #x900 18)
    (frob #x000ff000 #xa00 20)
    (frob #x0003fc00 #xb00 22)
    (frob #x0000ff00 #xc00 24)
    (frob #x00003fc0 #xd00 26)
    (frob #x00000ff0 #xe00 28)
    (frob #x000003fc #xf00 30)))

(defun ror-immed12 (immed12)
  "Perform the calculation as stated in A5.2.4. Basically, rotate right
ROTATION*2 times the value IMMED8 into a 32-bit integer."
  (let* ((rotation (ldb (byte 4 8) immed12))
         (immed8 (ldb (byte 8 0) immed12))
         (count (* rotation -2))
         (bytespec (byte 32 0)))
    (logior (ldb bytespec (ash immed8 count))
            (ldb bytespec (ash immed8 (+ count 32))))))

(sb!disassem:define-arg-type immed12
    :printer #'(lambda (value stream dstate)
                 (declare (ignore dstate)
                          (type stream stream) (fixnum value))
                 (princ (format nil "#~d" (ror-immed12 value)) stream)))

;; Shift types. However, there is a catch: 0 is either no shift or LSL,
;; depending on whether the immed5 field is provided; also, 3 is either ROR or
;; RRX based on the same thing. This is disambiguated using the immed5 prefilter
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *shift-types-alist*
    '((:lsl . 0) (:lsr . 1) (:asr . 2) (:ror 3)))
  (defparameter *shift-types-alist2* (append *shift-types-alist* '((:rrx 3)))))

(defun shift-op-encoding (shift-op)
  (cdr (assoc shift-op *shift-types-alist2* :test #'eq)))

(sb!disassem:define-arg-type shift-op
    :printer #'(lambda (value stream dstate)
                 (declare (type stream stream) (type fixnum value))
                 (let ((zero-immed5
                        (sb!disassem:dstate-get-inst-prop dstate 'zero-immed5)))
                   (cond
                     ((and (= value 0) zero-immed5)) ;; noop
                     ((and (= value 3) zero-immed5) (princ ", RRX" stream))
                     (t (princ
                         (format nil ", ~a"
                                 (or (car (rassoc value *shift-types-alist*))
                                     value))
                         stream))))))

;; 5-bit immediate field. Print #<value>
;; The prefilter is used in order to signify to the shift-type printer that the
;; immed5 value is zero. This will disambiguate some of the printer cases.
(sb!disassem:define-arg-type immed5
    :prefilter #'(lambda (value dstate)
                   (when (= value 0)
                     (sb!disassem:dstate-put-inst-prop dstate 'zero-immed5)))
    :printer #'(lambda (value stream dstate)
                 (declare (ignore dstate)
                          (type stream stream) (fixnum value))
                 (unless (= value 0)
                   (princ (format nil "#~d" value) stream))))

(defparameter *vfp-mov-immed-alist*
  '((2.0 . 0) (2.125 . 1) (2.25 . 2) (2.375 . 3) (2.5 . 4) (2.625 . 5)
    (2.75 . 6) (2.875 . 7) (3.0 . 8) (3.125 . 9) (3.25 . 10) (3.375 . 11)
    (3.5 . 12) (3.625 . 13) (3.75 . 14) (3.875 . 15) (4.0 . 16) (4.25 . 17)
    (4.5 . 18) (4.75 . 19) (5.0 . 20) (5.25 . 21) (5.5 . 22) (5.75 . 23)
    (6.0 . 24) (6.25 . 25) (6.5 . 26) (6.75 . 27) (7.0 . 28) (7.25 . 29)
    (7.5 . 30) (7.75 . 31) (8.0 . 32) (8.5 . 33) (9.0 . 34) (9.5 . 35)
    (10.0 . 36) (10.5 . 37) (11.0 . 38) (11.5 . 39) (12.0 . 40) (12.5 . 41)
    (13.0 . 42) (13.5 . 43) (14.0 . 44) (14.5 . 45) (15.0 . 46) (15.5 . 47)
    (16.0 . 48) (17.0 . 49) (18.0 . 50) (19.0 . 51) (20.0 . 52) (21.0 . 53)
    (22.0 . 54) (23.0 . 55) (24.0 . 56) (25.0 . 57) (26.0 . 58) (27.0 . 59)
    (28.0 . 60) (29.0 . 61) (30.0 . 62) (31.0 . 63) (0.125 . 64)
    (0.1328125 . 65) (0.140625 . 66) (0.1484375 . 67) (0.15625 . 68)
    (0.1640625 . 69) (0.171875 . 70) (0.1796875 . 71) (0.1875 . 72)
    (0.1953125 . 73) (0.203125 . 74) (0.2109375 . 75) (0.21875 . 76)
    (0.2265625 . 77) (0.234375 . 78) (0.2421875 . 79) (0.25 . 80)
    (0.265625 . 81) (0.28125 . 82) (0.296875 . 83) (0.3125 . 84)
    (0.328125 . 85) (0.34375 . 86) (0.359375 . 87) (0.375 . 88)
    (0.390625 . 89) (0.40625 . 90) (0.421875 . 91) (0.4375 . 92)
    (0.453125 . 93) (0.46875 . 94) (0.484375 . 95) (0.5 . 96) (0.53125 . 97)
    (0.5625 . 98) (0.59375 . 99) (0.625 . 100) (0.65625 . 101) (0.6875 . 102)
    (0.71875 . 103) (0.75 . 104) (0.78125 . 105) (0.8125 . 106) (0.84375 . 107)
    (0.875 . 108) (0.90625 . 109) (0.9375 . 110) (0.96875 . 111) (1.0 . 112)
    (1.0625 . 113) (1.125 . 114) (1.1875 . 115) (1.25 . 116) (1.3125 . 117)
    (1.375 . 118) (1.4375 . 119) (1.5 . 120) (1.5625 . 121) (1.625 . 122)
    (1.6875 . 123) (1.75 . 124) (1.8125 . 125) (1.875 . 126) (1.9375 . 127)
    (-2.0 . 128) (-2.125 . 129) (-2.25 . 130) (-2.375 . 131) (-2.5 . 132)
    (-2.625 . 133) (-2.75 . 134) (-2.875 . 135) (-3.0 . 136) (-3.125 . 137)
    (-3.25 . 138) (-3.375 . 139) (-3.5 . 140) (-3.625 . 141) (-3.75 . 142)
    (-3.875 . 143) (-4.0 . 144) (-4.25 . 145) (-4.5 . 146) (-4.75 . 147)
    (-5.0 . 148) (-5.25 . 149) (-5.5 . 150) (-5.75 . 151) (-6.0 . 152)
    (-6.25 . 153) (-6.5 . 154) (-6.75 . 155) (-7.0 . 156) (-7.25 . 157)
    (-7.5 . 158) (-7.75 . 159) (-8.0 . 160) (-8.5 . 161) (-9.0 . 162)
    (-9.5 . 163) (-10.0 . 164) (-10.5 . 165) (-11.0 . 166) (-11.5 . 167)
    (-12.0 . 168) (-12.5 . 169) (-13.0 . 170) (-13.5 . 171) (-14.0 . 172)
    (-14.5 . 173) (-15.0 . 174) (-15.5 . 175) (-16.0 . 176) (-17.0 . 177)
    (-18.0 . 178) (-19.0 . 179) (-20.0 . 180) (-21.0 . 181) (-22.0 . 182)
    (-23.0 . 183) (-24.0 . 184) (-25.0 . 185) (-26.0 . 186) (-27.0 . 187)
    (-28.0 . 188) (-29.0 . 189) (-30.0 . 190) (-31.0 . 191) (-0.125 . 192)
    (-0.1328125 . 193) (-0.140625 . 194) (-0.1484375 . 195) (-0.15625 . 196)
    (-0.1640625 . 197) (-0.171875 . 198) (-0.1796875 . 199) (-0.1875 . 200)
    (-0.1953125 . 201) (-0.203125 . 202) (-0.2109375 . 203) (-0.21875 . 204)
    (-0.2265625 . 205) (-0.234375 . 206) (-0.2421875 . 207) (-0.25 . 208)
    (-0.265625 . 209) (-0.28125 . 210) (-0.296875 . 211) (-0.3125 . 212)
    (-0.328125 . 213) (-0.34375 . 214) (-0.359375 . 215) (-0.375 . 216)
    (-0.390625 . 217) (-0.40625 . 218) (-0.421875 . 219) (-0.4375 . 220)
    (-0.453125 . 221) (-0.46875 . 222) (-0.484375 . 223) (-0.5 . 224)
    (-0.53125 . 225) (-0.5625 . 226) (-0.59375 . 227) (-0.625 . 228)
    (-0.65625 . 229) (-0.6875 . 230) (-0.71875 . 231) (-0.75 . 232)
    (-0.78125 . 233) (-0.8125 . 234) (-0.84375 . 235) (-0.875 . 236)
    (-0.90625 . 237) (-0.9375 . 238) (-0.96875 . 239) (-1.0 . 240)
    (-1.0625 . 241) (-1.125 . 242) (-1.1875 . 243) (-1.25 . 244)
    (-1.3125 . 245) (-1.375 . 246) (-1.4375 . 247) (-1.5 . 248)
    (-1.5625 . 249) (-1.625 . 250) (-1.6875 . 251) (-1.75 . 252)
    (-1.8125 . 253) (-1.875 . 254) (-1.9375 . 255)))

(defun vfp-mov-immed-encoding (immed)
  (let ((imm8 (cdr (assoc immed *vfp-mov-immed-alist* :test #'eq))))
    (unless imm8 (error "vmov immediate out of range!"))
    (values (ldb (byte 4 4) imm8)
	    (ldb (byte 4 0) imm8))))



;;;; Emitters

;;; Primitive emitters.
(define-bitfield-emitter emit-word 32
  (byte 32 0))

;;; A5.2.1 Data processing (register) instruction emitter
(define-bitfield-emitter emit-dp-reg-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 5 7) (byte 2 5) (byte 1 4) (byte 4 0))

;;; A5.2.2 Data processing (register-shifter register) instruction emitter
(define-bitfield-emitter emit-dp-rsr-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 1 7) (byte 2 5) (byte 1 4) (byte 4 0))

;;; A5.2.3 Data processing (immediate) instruction emitter
;;; A5.2.11 MSR immediate and hints
(define-bitfield-emitter emit-dp-imm-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12) (byte 12 0))

;;; A5.2.5 Multiply and Multiply-Accumulate instruction emitter
;;; A5.2.6 Saturating addition and subtraction
;;; A5.2.7 Halfword multiply and multiply-accumulate
;;; A5.2.8 Extra load/store instructions
;;; A5.2.9 Extra load/store instructions (unpriviledged)
;;; A5.2.10 Synchronization primitives
;;; A5.2.11 MSR immediate and hints
(define-bitfield-emitter emit-mma-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 4 4) (byte 4 0))

;;; CCCC|00010010|imm12.......|0111|imm4| -- BKPT #imm16
(define-bitfield-emitter emit-misc-inst 32
  (byte 4 28) (byte 8 20) (byte 12 8) (byte 4 4) (byte 4 0))






(define-bitfield-emitter emit-a523-inst 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16)
  (byte 4 12) (byte 12 0))

;;; 3-register VFP data-processing instructions
;;; CCCC|11100D00|Vn  |Vd  |101sNoM0|Vm  | VMLA
(define-bitfield-emitter emit-vfp-3rdp-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12) (byte 8 4) (byte 4 0))

;;; CCCC|1101UD01|Rn  |Vd  |1010|imm8    | -- VLDR Sd, Rn +/-#imm8
(define-bitfield-emitter emit-xrls-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12) (byte 4 8) (byte 8 0))



;; 
;;;; Instruction definitions


;;;=============================================================================
;;; A5.2.1. Data Processing (register)
;;; A5.2.2. Data Processing (register-shifted register)
;;; A5.2.3. Data Processing (immediate)
;;;
;;; CCCC|0000001|S|Rn  |Rd  |imm5  |tp|0|Rm  | EOR Rd, Rn, Rm, sh-op #shift
;;; CCCC|0000001|S|Rn  |Rd  |Rs  |0|tp|1|Rm  | EOR Rd, Rn, Rm, sh-op Rs
;;; CCCC|0010001|S|Rn  |Rd  |imm12           | EOR Rd, Rn, #const
(macrolet ((define-dp-instruction (name op)
             `(define-instruction ,name (segment dst src1 src2 &key (cnd :al)
                                                 shift shift-op)
                (:delay 1)
                (:cost 1)
                (:dependencies (reads src1) (reads src2) (writes dst))
                (:emitter
                 (let ((fld ,op))
                   (if shift-op
                       (cond
                         ;; first form (register)
                         ((integerp shift)
                          (emit-dp-reg-inst segment
                                            (cond-encoding cnd) fld
                                            (reg-tn-encoding src1)
                                            (reg-tn-encoding dst)
                                            shift
                                            (shift-op-encoding shift-op) #b0
                                            (reg-tn-encoding src2)))
                         ;; second form (register-shifted register)
                         ((register-p shift)
                          (emit-dp-rsr-inst segment
                                            (cond-encoding cnd) fld
                                            (reg-tn-encoding src1)
                                            (reg-tn-encoding dst)
                                            (reg-tn-encoding shift)
                                            #b0 (shift-op-encoding shift-op) #b1
                                            (reg-tn-encoding src2)))
                         (t
                          (error "Bad shift operand in ~a!" ',name)))
                       (if (register-p src2)
                           ;; first form (register, special case of 0 shift)
                           (emit-dp-reg-inst segment
                                             (cond-encoding cnd) fld
                                             (reg-tn-encoding src1)
                                             (reg-tn-encoding dst)
                                             #b00000 #b00 #b0
                                             (reg-tn-encoding src2))
                           ;; third form (immediate)
                           (emit-dp-imm-inst segment
                                             (cond-encoding cnd)
                                             (logior #b100000 fld)
                                             (reg-tn-encoding src1)
                                             (reg-tn-encoding dst)
                                             (immed12-encoding src2)))))))))
  (define-dp-instruction and   #b00000000)
  (define-dp-instruction ands  #b00000001)
  (define-dp-instruction eor   #b00000010)
  (define-dp-instruction eors  #b00000011)
  (define-dp-instruction sub   #b00000100)
  (define-dp-instruction subs  #b00000101)
  (define-dp-instruction rsb   #b00000110)
  (define-dp-instruction rsbs  #b00000111)
  (define-dp-instruction add   #b00001000)
  (define-dp-instruction adds  #b00001001)
  (define-dp-instruction adc   #b00001010)
  (define-dp-instruction adcs  #b00001011)
  (define-dp-instruction sbc   #b00001100)
  (define-dp-instruction sbcs  #b00001101)
  (define-dp-instruction rsc   #b00001110)
  (define-dp-instruction rscs  #b00001111)
  ;; --- these don't have non-S variants, since all they do is set flags
  (define-dp-instruction tst   #b00010001)
  (define-dp-instruction teq   #b00010011)
  (define-dp-instruction cmp   #b00010101)
  (define-dp-instruction cmn   #b00010111)
  ;; ---
  (define-dp-instruction orr   #b00011000)
  (define-dp-instruction orrs  #b00011001)
  (define-dp-instruction %mov  #b00011010)
  (define-dp-instruction %movs #b00011011)
  (define-dp-instruction bic   #b00011100)
  (define-dp-instruction bics  #b00011101)
  (define-dp-instruction mvn   #b00011110)
  (define-dp-instruction mvns  #b00011111)
  )


;; Instructions based on the %MOV/%MOVS templates with special values for he
;; shifter and operands.
(macrolet ((mov-frob (name base)
             `(define-instruction-macro ,name (dst src &key (cnd :al))
                `(inst ,,base ,dst a0-tn ,src :cnd ,cnd)))
           (shift-frob (name base shift-op)
             `(define-instruction-macro ,name (dst src shift &key (cnd :al))
                `(inst ,,base ,dst a0-tn ,src :cnd ,cnd
                       :shift-op ,,shift-op
                       :shift ,shift))))
  (mov-frob   mov  '%mov)
  (mov-frob   movs '%movs)
  (shift-frob lsl  '%mov  :lsl)
  (shift-frob lsls '%movs :lsl)
  (shift-frob lsr  '%mov  :lsr)
  (shift-frob lsrs '%movs :lsr)
  (shift-frob asr  '%mov  :asr)
  (shift-frob asrs '%movs :asr)
  (shift-frob ror  '%mov  :ror)
  (shift-frob rors '%movs :ror))

(define-instruction-macro rrx (dst src &key (cnd :al))
  `(inst ror ,dst ,src 0 :cnd ,cnd))

(define-instruction-macro rrxs (dst src &key (cnd :al))
  `(inst rors ,dst ,src 0 :cnd ,cnd))

(macrolet ((frob (name fx op)
             `(define-instruction ,name (segment dst src &key (cnd :al))
                (:declare (type (or fixup (unsigned-byte 16)
                                    (signed-byte 16)) src))
                (:delay 1)
                (:cost 1)
                (:dependencies (writes dst))
                (:emitter
                 (when (typep src 'fixup)
                   (note-fixup segment ,fx src)
                   (setq src (or (fixup-offset src) 0)))
                 (let ((imm4 (ldb (byte 4 12) src))
                       (imm12 (ldb (byte 12 0) src)))
                   (emit-a523-inst segment (cond-encoding cnd)
                                   #b001 ,op imm4
                                   (reg-tn-encoding dst) imm12))))))
  (frob movw :l #b10000)
  (frob movt :ha #b10100))

;;;
;;; End of A5.2.1, A5.2.2 and A5.2.3
;;;=============================================================================



;;;=============================================================================
;;; A5.2.4 Modified immediate constants in ARM instructions
;;;
;;; no instructions in this section
;;;
;;; End of A5.2.4
;;;=============================================================================



;;;=============================================================================
;;; A5.2.5 Multiply and multiply-accumulate
;;;
(macrolet ((define-mma-instruction (name op subtype)
	     ;; note that in the :long instructions arguments semantics is:
	     ;; dest is dest/high (RdHi)
	     ;; src3 is dest/low (RdLo)
	     ;; src1 is src2 (Rm)
	     ;; src2 is src1 (Rn)
             `(define-instruction ,name (segment cnd dest src1 src2 src3)
                (:delay 1)
                (:cost 1)
		,(ecase subtype
			(:regular
			 '(:dependencies (reads src1) (reads src2) (reads src3)
			   (writes dest)))
			(:long
			 '(:dependencies (reads dest) (reads src1) (reads src2)
			   (reads src3) (writes dest) (writes src1))))
                (:emitter (emit-mma-inst segment
					 (cond-encoding cnd)
					 ,op
					 (reg-tn-encoding dest)
					 (reg-tn-encoding src3)
					 (reg-tn-encoding src2)
					 #b1001
					 (reg-tn-encoding src1))))))
  (define-mma-instruction %mul   #b00000000 :regular)
  (define-mma-instruction %muls  #b00000001 :regular)
  (define-mma-instruction mla    #b00000010 :regular)
  (define-mma-instruction mlas   #b00000011 :regular)
  (define-mma-instruction umaal  #b00000100 :long)
  (define-mma-instruction mls    #b00000110 :regular)
  (define-mma-instruction umull  #b00001000 :long)
  (define-mma-instruction umulls #b00001001 :long)
  (define-mma-instruction umlal  #b00001010 :long)
  (define-mma-instruction umlals #b00001011 :long)
  (define-mma-instruction smull  #b00001100 :long)
  (define-mma-instruction smulls #b00001101 :long)
  (define-mma-instruction smlal  #b00001110 :long)
  (define-mma-instruction smlals #b00001111 :long))

(define-instruction-macro mul (dst src1 src2 &key (cnd :al))
  `(inst %mul ,dst ,src1 ,src2 a0-tn :cnd ,cnd))

(define-instruction-macro muls (dst src1 src2 &key (cnd :al))
  `(inst %muls ,dst ,src1 ,src2 a0-tn :cnd ,cnd))
;;;
;;; End of A5.2.5
;;;=============================================================================



;;;=============================================================================
;;; A5.2.6 Saturating addition and subtraction
(macrolet ((define-sas-instruction (name op)
             `(define-instruction ,name (segment cnd dest src1 src2)
                (:delay 1)
                (:cost 1)
		(:dependencies (reads src1) (reads src2) (writes dest))
                (:emitter
		 (emit-mma-inst segment
				(cond-encoding cnd)
				,op
				(reg-tn-encoding src2)
				(reg-tn-encoding dest)
				#b0000
				#b0101
				(reg-tn-encoding src1))))))
  (define-sas-instruction qadd  #b00010000)
  (define-sas-instruction qsub  #b00010010)
  (define-sas-instruction qdadd #b00010100)
  (define-sas-instruction qdsub #b00010110))
;;;
;;; End of A5.2.6
;;;=============================================================================



;;;=============================================================================
;;; A5.2.7 Halfword multiply and multiply-accumulate
;;;
(macrolet ((define-hwmma-instruction (name op1 op2)
             `(define-instruction ,name (segment cnd dest src1 src2 src3)
                (:delay 1)
                (:cost 1)
		(:dependencies (reads src1) (reads src2) (reads src3)
			       (writes dest))
                (:emitter (emit-mma-inst segment
					 (cond-encoding cnd)
					 ,op1
					 (reg-tn-encoding dest)
					 (reg-tn-encoding src3)
					 (reg-tn-encoding src2)
					 ,op2
					 (reg-tn-encoding src1))))))
  (define-hwmma-instruction smlabb #b00010000 #b1000)
  (define-hwmma-instruction smlabt #b00010000 #b1100)
  (define-hwmma-instruction smlatb #b00010000 #b1010)
  (define-hwmma-instruction smlatt #b00010000 #b1110)

  (define-hwmma-instruction smlawb #b00010010 #b1000)
  (define-hwmma-instruction smlawt #b00010010 #b1100)

  (define-hwmma-instruction %smulwb #b00010010 #b1010)
  (define-hwmma-instruction %smulwt #b00010010 #b1110)

  (define-hwmma-instruction %smulbb #b00010110 #b1000)
  (define-hwmma-instruction %smulbt #b00010110 #b1100)
  (define-hwmma-instruction %smultb #b00010110 #b1010)
  (define-hwmma-instruction %smultt #b00010110 #b1110))

(define-instruction-macro smulwb (dst src1 src2 &key (cnd :al))
  `(inst %smulwb ,dst ,src1 ,src2 a0-tn :cnd ,cnd))

(define-instruction-macro smulwt (dst src1 src2 &key (cnd :al))
  `(inst %smulwt ,dst ,src1 ,src2 a0-tn :cnd ,cnd))

(define-instruction-macro smulbb (dst src1 src2 &key (cnd :al))
  `(inst %smulbb ,dst ,src1 ,src2 a0-tn :cnd ,cnd))

(define-instruction-macro smulbt (dst src1 src2 &key (cnd :al))
  `(inst %smulbt ,dst ,src1 ,src2 a0-tn :cnd ,cnd))

(define-instruction-macro smultb (dst src1 src2 &key (cnd :al))
  `(inst %smultb ,dst ,src1 ,src2 a0-tn :cnd ,cnd))

(define-instruction-macro smultt (dst src1 src2 &key (cnd :al))
  `(inst %smultt ,dst ,src1 ,src2 a0-tn :cnd ,cnd))
;;;
;;; End of A5.2.7
;;;=============================================================================



;;;=============================================================================
;;; A5.2.8 Extra load/store instructions
;;;
(macrolet ((define-xls-instruction (name op1 op2 dir)
             `(define-instruction ,name (segment src base offset &key (cnd :al)
						 (index t) wback)
                (:delay 1)
                (:cost 1)
                ,(ecase dir
                        (:load '(:dependencies (writes src) (reads base)
				 (reads :memory)))
                        (:store '(:dependencies (reads src) (reads base)
				  (writes :memory))))
                (:emitter
                 (let ((fld ,op1))
                   (when index (setf fld (logior fld #b00010000)))
                   (when wback (setf fld (logior fld #b00000010)))
                   (cond
		     ;; first form (register)
		     ((register-p offset)
                      (emit-mma-inst segment (cond-encoding cnd) fld
				     (reg-tn-encoding base)
				     (reg-tn-encoding src)
				     #b0000
				     ,op2
				     (reg-tn-encoding offset)))
                     ;; second form (immediate)
                     ((integerp offset)
		      (setf fld (logior fld #b00000100))
		      (if (>= offset 0)
			  (setf fld (logior fld #b00001000))
			  (setf offset (- offset)))
                      (emit-mma-inst segment (cond-encoding cnd) fld
				     (reg-tn-encoding base)
				     (reg-tn-encoding src)
				     (ldb (byte 4 4) (ash offset -24))
				     ,op2
				     (ldb (byte 4 0) (ash offset -24))))
                     (t (error "Bad offset operand in ~a!" ',name))))))))
  (define-xls-instruction strh   #b00000000 #b1011 :store)
  (define-xls-instruction ldrh   #b00000001 #b1011 :load)
  (define-xls-instruction ldrd   #b00000000 #b1101 :load)
  (define-xls-instruction ldrsb  #b00000001 #b1101 :load)
  (define-xls-instruction strd   #b00000000 #b1111 :store)
  (define-xls-instruction ldrsh  #b00000001 #b1111 :load))
;;;
;;; End of A5.2.8
;;;=============================================================================



;;;=============================================================================
;;; A5.2.9 Extra load/store instructions (unprivileged)
;;;
(macrolet ((define-xlsu-instruction (name op1 op2 dir)
             `(define-instruction ,name (segment src base offset &key (cnd :al))
                (:delay 1)
                (:cost 1)
                ,(ecase dir
                        (:load '(:dependencies (writes src) (reads base)
				 (reads :memory)))
                        (:store '(:dependencies (reads src) (reads base)
				  (writes :memory))))
                (:emitter
                 (let ((fld ,op1))
                   (cond
		     ;; first form (register)
		     ((register-p offset)
                      (emit-mma-inst segment (cond-encoding cnd) fld
				     (reg-tn-encoding base)
				     (reg-tn-encoding src)
				     #b0000
				     ,op2
				     (reg-tn-encoding offset)))
                     ;; second form (immediate)
                     ((integerp offset)
		      (setf fld (logior fld #b00000100))
		      (if (>= offset 0)
			  (setf fld (logior fld #b00001000))
			  (setf offset (- offset)))
                      (emit-mma-inst segment (cond-encoding cnd) fld
				     (reg-tn-encoding base)
				     (reg-tn-encoding src)
				     (ldb (byte 4 4) (ash offset -24))
				     ,op2
				     (ldb (byte 4 0) (ash offset -24))))
                     (t (error "Bad offset operand in ~a!" ',name))))))))
  (define-xlsu-instruction strht  #b00000010 #b1011 :store)
  (define-xlsu-instruction ldrht  #b00000011 #b1011 :load)
  (define-xlsu-instruction ldrsbt #b00000011 #b1101 :load)
  (define-xlsu-instruction ldrsht #b00000011 #b1111 :load))
;;;
;;; End of A5.2.9
;;;=============================================================================



;;;=============================================================================
;;; A5.2.10 Synchronization primitives
;;;
(macrolet ((define-sp-instruction (name op1 op2)
             `(define-instruction ,name (segment r1 r2 r3 &key (cnd :al))
                (:delay 1)
                (:cost 1)
		(:dependencies (writes r1) (reads r2) (reads :memory))
                (:emitter
		 (emit-mma-inst segment (cond-encoding cnd) ,op1
				(reg-tn-encoding r3)
				(reg-tn-encoding r1)
				,op2
				#b1001
				(reg-tn-encoding r2))))))
  (define-sp-instruction swp     #b00010000 #b0000)
  (define-sp-instruction swpb    #b00010100 #b0000)
  (define-sp-instruction strex   #b00011000 #b1111)
  (define-sp-instruction %ldrex  #b00011001 #b1111)
  (define-sp-instruction strexd  #b00011010 #b1111)
  (define-sp-instruction %ldrexd #b00011011 #b1111)
  (define-sp-instruction strexb  #b00011100 #b1111)
  (define-sp-instruction %ldrexb #b00011101 #b1111)
  (define-sp-instruction strexh  #b00011110 #b1111)
  (define-sp-instruction %ldrexh #b00011111 #b1111)
)

(define-instruction-macro ldrex (r3 r1 &key (cnd :al))
  `(inst %ldrex ,r1 a0-tn ,r3 :cnd ,cnd))

(define-instruction-macro ldrexd (r3 r1 &key (cnd :al))
  `(inst %ldrexd ,r1 a0-tn ,r3 :cnd ,cnd))

(define-instruction-macro ldrexb (r3 r1 &key (cnd :al))
  `(inst %ldrexb ,r1 a0-tn ,r3 :cnd ,cnd))

(define-instruction-macro ldrexh (r3 r1 &key (cnd :al))
  `(inst %ldrexh ,r1 a0-tn ,r3 :cnd ,cnd))
;;;
;;; End of A5.2.10
;;;=============================================================================



;;;=============================================================================
;;; A5.2.11 MSR (immediate) and hints
;;;
(macrolet ((define-hint-instruction (name op)
             `(define-instruction ,name (segment option &key (cnd :al))
                (:delay 1)
                (:cost 1)
                (:emitter
		 (emit-mma-inst segment (cond-encoding cnd)
				#b00110010 #b0000
				#b1111 #b0000
				,op option)))))
  (define-hint-instruction %nop   #b0000)
  (define-hint-instruction %yield #b0000)
  (define-hint-instruction %wfe   #b0000)
  (define-hint-instruction %wfi   #b0000)
  (define-hint-instruction %sev   #b0000)
  (define-hint-instruction dbg    #b1111))

(define-instruction-macro nop (&key (cnd :al))
  `(inst %nop #b0000 :cnd ,cnd))

(define-instruction-macro yield (&key (cnd :al))
  `(inst %yield #b0001 :cnd ,cnd))

(define-instruction-macro wfe (&key (cnd :al))
  `(inst %wfe #b0010 :cnd ,cnd))

(define-instruction-macro wfi (&key (cnd :al))
  `(inst %wfi #b0011 :cnd ,cnd))

(define-instruction-macro sev (&key (cnd :al))
  `(inst %sev #b0100 :cnd ,cnd))

(define-instruction msr (segment rn-or-imm mask &key (cnd :al))
  (:delay 1)
  (:cost 1)
  (:emitter
   (cond
     ((register-p rn-or-imm)
      ;; A5.2.12 Miscelaneous instructions MSR (register)
      (emit-dp-reg-inst segment (cond-encoding cnd)
		    #b00010010
		    (ash mask 2)
		    #b1111  #b00000 #b00 #b0
		    (reg-tn-encoding rn-or-imm)))
     ((integerp rn-or-imm)
      (emit-dp-imm-inst segment (cond-encoding cnd)
			#b00110010
			(ash mask 2)
			#b1111
			(immed12-encoding rn-or-imm))))))
;;;
;;; End of A5.2.11
;;;=============================================================================



;;;=============================================================================
;;; A5.2.12 Miscellaneous instructions
;;;
(define-instruction mrs (segment rd &key (cnd :al))
  (:delay 1)
  (:cost 1)
  (:emitter
   (emit-dp-imm-inst segment (cond-encoding cnd)
		     #b00010000
		     #b1111
		     (reg-tn-encoding rd)
		     #b000000000000)))

(macrolet ((define-misc-instruction (name op1 op2)
             `(define-instruction ,name (segment rm &key (cnd :al))
		(:delay 1)
		(:cost 1)
                (:emitter
                 (emit-dp-reg-inst segment (cond-encoding cnd)
				   #b00010010 #b1111 #b1111 #b11110
				   ,op1 ,op2 (reg-tn-encoding rm)))))
           )
  (define-misc-instruction bx  #b00 #b1)
  (define-misc-instruction bxj #b01 #b0)
  (define-misc-instruction blx #b01 #b1))

(define-instruction clz (segment cnd rd rm)
  (:delay 1)
  (:cost 1)
  (:emitter
   (emit-mma-inst segment (cond-encoding cnd)
		  #b00010110 #b1111 (reg-tn-encoding rd)
		  #b1111 #b0001 (reg-tn-encoding rm))))

(define-instruction bkpt (segment imm16)
  (:delay 1)
  (:cost 1)
  (:emitter
   (let ((imm12 (ldb (byte 12 4) imm16))
	 (imm4 (ldb (byte 4 0) imm16)))
     (emit-misc-inst segment #b1110 #b00010010 imm12 #b0111 imm4))))

;;;
;;; End of A5.2.12
;;;=============================================================================



;;;=============================================================================
;;; A5.3 Load/store word and unsigned byte
;;; CCCC|01A|op1  |Rn  |...........  |B|....| - template
;;; CCCC|010|PU0W0|Rn  |Rt  |immed12.....   | - STR Rt, [Rn, #immed]
;;; CCCC|011|PU0W0|Rn  |Rt  |imm5.|tp|0|Rm..| - STR Rt, [Rn, Rm, {,shift}]!
;;; CCCC|010|1U001|1111|Rt  |immed12.....   | - LDR Rt, <label> (sp. of form1)
(macrolet ((define-ls-instruction (name op dir)
             `(define-instruction ,name (segment src base offset &key (cnd :al)
						 (index t) wback
						 shift shift-op)
                (:declare (type (or (integer -4096 4096) tn) offset))
                (:delay 1)
                (:cost 1)
                ,(ecase dir
                        (:load '(:dependencies (writes src) (reads base)
					      (reads :memory)))
                        (:store '(:dependencies (reads src) (reads base)
					       (writes :memory))))
                (:emitter
                 (let ((fld ,op))
                   (when index (setf fld (logior fld #b00010000)))
                   (when wback (setf fld (logior fld #b00000010)))
                   (cond
                     ;; first form (immediate)
                     ((integerp offset)
		      (if (>= offset 0)
			  (setf fld (logior fld #b00001000))
			  (setf offset (- offset)))
                      (emit-dp-imm-inst segment (cond-encoding cnd) fld
                                        (reg-tn-encoding base)
                                        (reg-tn-encoding src)
                                        (ash offset -2)))
                     ;; second form (register)
                     ((register-p offset)
                      (setf fld (logior fld #b00100000))
                      (emit-dp-reg-inst segment (cond-encoding cnd) fld
                                        (reg-tn-encoding base)
                                        (reg-tn-encoding src)
                                        shift
                                        (shift-op-encoding shift-op) #b0
                                        (reg-tn-encoding offset)))
                     (t (error "Bad offset operand in ~a!" ',name))))))))
  (define-ls-instruction str  #b01000000 :store)
  (define-ls-instruction strb #b01000100 :store)
  (define-ls-instruction ldr  #b01000001 :load)
  (define-ls-instruction ldrb #b01000101 :load))

;;; End of A5.3
;;;=============================================================================



;;;;============================================================================
;;;; A5.5 Branch, branch with link and block data transfer

(define-instruction b (segment target &key (cnd :al))
  (:emitter
   ;; FIXME: the target may be > 32 MB away, what do we do then?
   (etypecase target
     (fixup
      (note-fixup segment :b target)
      (emit-word segment
		 (dpb (cond-encoding cnd) (byte 4 28)
		      (dpb #b1010 (byte 4 24) 0))))
     (label
      (emit-back-patch
       segment 4
       #'(lambda (segment posn)
	   (emit-word
	    segment
	    (dpb (cond-encoding cnd) (byte 4 28)
		 (dpb #b1010 (byte 4 24)
		      (ash (- (label-position target) posn) -2))))))))))

;;;; End of A5.5
;;;;============================================================================



;;;;============================================================================
;;;; A7.5 VFP Data Processing Instructions

;;; Table A7-16 Three-register  VFP data-processing instructions
;;;
;;; CCCC|11100D00|Vn  |Vd  |101sNoM0|Vm  | VMLA
(macrolet ((define-vfp-3rdp-instruction (name op1 op2 precision)
	     `(define-instruction ,name (segment dest src1 src2
						 &key (cnd :al))
		(:delay 1)
		(:cost 1)
		(:dependencies (reads src1) (reads src2) (writes dest))
		(:emitter
		 (let ((fld1 ,op1)
		       (fld2 ,op2)
		       (freg-n (freg-tn-encoding src1))
		       (freg-d (freg-tn-encoding dest))
		       (freg-m (freg-tn-encoding src2))
		       vn n vd d vm m)
		   ,(ecase precision
			   (:single '(setf
				      vn (ldb (byte 4 1) freg-n)
				      vd (ldb (byte 4 1) freg-d)
				      vm (ldb (byte 4 1) freg-m)
				      n (ldb (byte 1 0) freg-n)
				      d (ldb (byte 1 0) freg-d)
				      m (ldb (byte 1 0) freg-m)))
			   (:double '(setf
				      vn (ldb (byte 4 0) freg-n)
				      vd (ldb (byte 4 0) freg-d)
				      vm (ldb (byte 4 0) freg-m)
				      n (ldb (byte 1 0) freg-n)
				      d (ldb (byte 1 0) freg-d)
				      m (ldb (byte 1 0) freg-m)
				      fld2 (logior fld2 #b00010000))))
		   (setf fld2 (logior fld2 (ash n 3)))
		   (setf fld1 (logior fld1 (ash d 2)))
		   (setf fld2 (logior fld2 (ash m 1)))
		   (emit-vfp-3rdp-inst segment (cond-encoding cnd)
				       fld1 vn vd fld2 vm))))))
  ;; single
  (define-vfp-3rdp-instruction vmla.f32  #b11100000 #b10100000 :single)
  (define-vfp-3rdp-instruction vmls.f32  #b11100000 #b10100100 :single)
  (define-vfp-3rdp-instruction vnmla.f32 #b11100001 #b10100100 :single)
  (define-vfp-3rdp-instruction vnmls.f32 #b11100001 #b10100000 :single)
  (define-vfp-3rdp-instruction vnmul.f32 #b11100010 #b10100100 :single)
  (define-vfp-3rdp-instruction vmul.f32  #b11100010 #b10100000 :single)
  (define-vfp-3rdp-instruction vadd.f32  #b11100011 #b10100000 :single)
  (define-vfp-3rdp-instruction vsub.f32  #b11100011 #b10100100 :single)
  (define-vfp-3rdp-instruction vdiv.f32  #b11101000 #b10100000 :single)
  ;; double
  (define-vfp-3rdp-instruction vmla.f64  #b11100000 #b10100000 :double)
  (define-vfp-3rdp-instruction vmls.f64  #b11100000 #b10100100 :double)
  (define-vfp-3rdp-instruction vnmls.f64 #b11100001 #b10100000 :double)
  (define-vfp-3rdp-instruction vnmla.f64 #b11100001 #b10100100 :double)
  (define-vfp-3rdp-instruction vnmul.f64 #b11100010 #b10100100 :double)
  (define-vfp-3rdp-instruction vmul.f64  #b11100010 #b10100000 :double)
  (define-vfp-3rdp-instruction vadd.f64  #b11100011 #b10100000 :double)
  (define-vfp-3rdp-instruction vsub.f64  #b11100011 #b10100100 :double)
  (define-vfp-3rdp-instruction vdiv.f64  #b11101000 #b10100000 :double)
  )

;;; Table A7-17 Other VFP data-processing instructions, bar:
;;; - vcvtb, vcvtt (not supported on the Pi)
;;; - vcvt between floating-point and fixed-point (no fixed-point in sbcl)
;;; - vmov (it has two encodings, immediate and register)
;;; - vcmp and vcmpe with zero (other encoding)
;;; the latter 2 cases are implemented further down
;;;
;;; CCCC|11101D11|0000|Vd  |101s01M0|Vm  | VMOV
(macrolet ((define-vfp-odp-instruction (name op1 op2 op3 precision)
	     `(define-instruction ,name (segment dest src &key (cnd :al))
		(:delay 1)
		(:cost 1)
		(:dependencies (reads src) (writes dest))
		(:emitter
		 (let ((fld1 ,op1)
		       (fld2 ,op3)
		       (freg-d (freg-tn-encoding dest))
		       (freg-m (freg-tn-encoding src))
		       vd d vm m)
		   ,(ecase precision
			   (:single '(setf
				      vd (ldb (byte 4 1) freg-d)
				      vm (ldb (byte 4 1) freg-m)
				      d (ldb (byte 1 0) freg-d)
				      m (ldb (byte 1 0) freg-m)))
			   (:double '(setf
				      vd (ldb (byte 4 0) freg-d)
				      vm (ldb (byte 4 0) freg-m)
				      d (ldb (byte 1 0) freg-d)
				      m (ldb (byte 1 0) freg-m)
				      fld2 (logior fld2 #b00010000))))
		   (setf fld1 (logior fld1 (ash d 2)))
		   (setf fld2 (logior fld2 (ash m 1)))
		   (emit-vfp-3rdp-inst segment (cond-encoding cnd)
				       fld1 ,op2 vd fld2 vm))))))
  ;; single
  (define-vfp-odp-instruction vabs.f32     #b11101011 #b0000 #b10101100 :single)
  (define-vfp-odp-instruction vneg.f32     #b11101011 #b0001 #b10100100 :single)
  (define-vfp-odp-instruction vsqrt.f32    #b11101011 #b0001 #b10101100 :single)
  (define-vfp-odp-instruction vcmp.f32     #b11101011 #b0100 #b10100100 :single)
  (define-vfp-odp-instruction vcmpe.f32    #b11101011 #b0100 #b10101100 :single)
  (define-vfp-odp-instruction vcvt.f64.f32 #b11101011 #b0111 #b10101100 :single)
  (define-vfp-odp-instruction vcvtr.u32.f32 #b11101011 #b1100 #b10100100
			      :single)
  (define-vfp-odp-instruction vcvtr.s32.f32 #b11101011 #b1101 #b10100100
			      :single)
  (define-vfp-odp-instruction vcvt.u32.f32 #b11101011 #b1100 #b10101100 :single)
  (define-vfp-odp-instruction vcvt.s32.f32 #b11101011 #b1101 #b10101100 :single)
  (define-vfp-odp-instruction vcvt.f32.u32 #b11101011 #b1000 #b10100100 :single)
  (define-vfp-odp-instruction vcvt.f32.s32 #b11101011 #b1000 #b10101100 :single)
  ;; double
  (define-vfp-odp-instruction vabs.f64     #b11101011 #b0000 #b10101100 :double)
  (define-vfp-odp-instruction vneg.f64     #b11101011 #b0001 #b10100100 :double)
  (define-vfp-odp-instruction vsqrt.f64    #b11101011 #b0001 #b10101100 :double)
  (define-vfp-odp-instruction vcmp.f64     #b11101011 #b0100 #b10100100 :double)
  (define-vfp-odp-instruction vcmpe.f64    #b11101011 #b0100 #b10101100 :double)
  (define-vfp-odp-instruction vcvt.f32.f64 #b11101011 #b0111 #b10101100 :double)
  (define-vfp-odp-instruction vcvtr.u32.f64 #b11101011 #b1100 #b10100100
			      :double)
  (define-vfp-odp-instruction vcvtr.s32.f64 #b11101011 #b1101 #b10100100
			      :double)
  (define-vfp-odp-instruction vcvt.u32.f64 #b11101011 #b1100 #b10101100 :double)
  (define-vfp-odp-instruction vcvt.s32.f64 #b11101011 #b1101 #b10101100 :double)
  (define-vfp-odp-instruction vcvt.f64.u32 #b11101011 #b1000 #b10100100 :double)
  (define-vfp-odp-instruction vcvt.f64.s32 #b11101011 #b1000 #b10101100 :double)
  )

;;; VMOV has two encodings: immediate-to-register and register-to-register
;;; CCCC|11101D11|im4H|Vd  |101s0000|im4L| VMOV.F32 Sd, #imm8
;;; CCCC|11101D11|0000|Vd  |101s01M0|Vm  | VMOV.F32 Sd, Sm
(macrolet ((define-vfp-mov-instruction (name precision)
	     `(define-instruction ,name (segment dest src &key (cnd :al))
		(:delay 1)
		(:cost 1)
		(:dependencies (reads src) (writes dest))
		(:emitter
		 (let ((fld1 #b11101011)
		       (fld2 #b10100000)
		       (freg-d (freg-tn-encoding dest))
		       freg-m vd d vm m)
		   ,(ecase precision
		      (:single '(setf
				 vd (ldb (byte 4 1) freg-d)
				 d (ldb (byte 1 0) freg-d)))
		      (:double '(setf
				 vd (ldb (byte 4 0) freg-d)
				 d (ldb (byte 1 0) freg-d)
				 fld2 (logior fld2 #b00010000))))
		   (setf fld1 (logior fld1 (ash d 2)))
		   (cond
                     ;; first form (immediate)
                     ((floatp src)
		      (multiple-value-bind (imm4H imm4L)
			  (vfp-mov-immed-encoding src)
			(emit-vfp-3rdp-inst segment (cond-encoding cnd)
					    fld1 imm4H vd fld2 imm4L)))
                     ;; second form (register)
                     ((register-p src)
		      (setf freg-m (freg-tn-encoding src))
		      ,(ecase precision
			      (:single '(setf
					 vm (ldb (byte 4 1) freg-m)
					 m (ldb (byte 1 0) freg-m)))
			      (:double '(setf
					 vm (ldb (byte 4 0) freg-m)
					 m (ldb (byte 1 0) freg-m))))
		      (setf fld2 (logior fld2 (ash m 1)))
		      (emit-vfp-3rdp-inst segment (cond-encoding cnd)
					  fld1 #b0000 vd fld2 vm))))))))
  ;; single
  (define-vfp-mov-instruction vmov.f32 :single)
  ;; double
  (define-vfp-mov-instruction vmov.f64 :double)
  )

;;; VCMP/VCMPE with zero has somewhat special encoding, not suitable for 
;;; inclusion in the general case above.
;;; In order to differentiate between the vcmp register-to-register and
;;; vcmp register with 0.0, we give a different name to this inst.
;;;
;;; CCCC|11101D11|0101|Vd  |101sE100|0000| VCMP{E}<c>.F32 <Sd>, #0.0
(macrolet ((define-vfp-vcmpz-instruction (name op1 precision)
	     `(define-instruction ,name (segment dest &key (cnd :al))
		(:delay 1)
		(:cost 1)
		(:dependencies (reads dest))
		(:emitter
		 (let ((fld1 #b11101011)
		       (fld2 ,op1)
		       (freg-d (freg-tn-encoding dest))
		       vd d)
		   ,(ecase precision
		      (:single '(setf
				 vd (ldb (byte 4 1) freg-d)
				 d (ldb (byte 1 0) freg-d)))
		      (:double '(setf
				 vd (ldb (byte 4 0) freg-d)
				 d (ldb (byte 1 0) freg-d)
				 fld2 (logior fld2 #b00010000))))
		   (setf fld1 (logior fld1 (ash d 2)))
		   (emit-vfp-3rdp-inst segment (cond-encoding cnd)
				       fld1 #b0101 vd fld2 #b0000))))))
  ;; single
  (define-vfp-vcmpz-instruction %vcmp.f32  #b10100100 :single)
  (define-vfp-vcmpz-instruction %vcmpe.f32 #b10101100 :single)
  ;; double
  (define-vfp-vcmpz-instruction %vcmp.f64  #b10100100 :double)
  (define-vfp-vcmpz-instruction %vcmpe.f64 #b10101100 :double))
;;;; End of A7.5
;;;;============================================================================



;;;=============================================================================
;;; A7.6 Extension register load/store instructions
;;; CCCC|1101UD01|Rn  |Vd  |1010|imm8    | -- VLDR Sd, Rn +/-#imm8
;;;
(macrolet ((define-xrls-instruction (name op op2 dir)
             `(define-instruction ,name (segment src base offset
						 &key (cnd :al))
                (:declare (type (or (integer -4096 4096) tn) offset))
                (:delay 1)
                (:cost 1)
                ,(ecase dir
                        (:load '(:dependencies (writes src) (reads base)
					      (reads :memory)))
                        (:store '(:dependencies (reads src) (reads base)
					       (writes :memory))))
                (:emitter
                 (let ((fld ,op)
		       (freg (freg-tn-encoding src))
		       vd d fld2)
		   ,(ecase op2
			   (:single '(setf vd (ldb (byte 4 1) freg)
				      d (ldb (byte 1 0) freg)
				      fld2 #b1010))
			   (:double '(setf vd (ldb (byte 4 0) freg)
				      d (ldb (byte 1 4) freg)
				      fld2 #b1011)))
		   (if (>= offset 0)
		       (setf fld (logior fld #b00001000))
		       (setf offset (- offset)))
		   (setf fld (logior fld (ash d 2)))
		   (emit-xrls-inst segment (cond-encoding cnd) fld
				  (reg-tn-encoding base)
				  vd fld2 (ash offset -2)))))))
  (define-xrls-instruction vstr.32  #b11010000 :single :store)
  (define-xrls-instruction vldr.32  #b11010001 :single :load)
  (define-xrls-instruction vstr.64  #b11010000 :double :store)
  (define-xrls-instruction vldr.64  #b11010001 :double :load))
;;; End of A7.6
;;;=============================================================================



;;;=============================================================================
;;; A7.8 8,16, and 32-bit transfer between ARM core and extension registers
;;;
;;; TODO: implement VMOV from ARM to 1/2 of double-precision register and vv
;;;
(define-instruction vmov (segment dest src &key (cnd :al))
  (:delay 1)
  (:cost 1)
  (:dependencies (reads src) (writes dest))
  (:emitter
   (let (op vn n rt freg
	    (fld1 #b11100000)
	    (fld2 #b10100001))
     (cond
       ((and (register-p src) (vfp-register-p dest))
	(setf op 0
	      rt (reg-tn-encoding src)
	      freg (freg-tn-encoding dest)))
       ((and (register-p dest) (vfp-register-p src))
	(setf op 1
	      rt (reg-tn-encoding dest)
	      freg (freg-tn-encoding src)))
       (t (error "Wrong arguments to VMOV!")))
     (setf vn (ldb (byte 4 1) freg) n (ldb (byte 1 0) freg)
	   fld1 (logior fld1 op)
	   fld2 (logior fld2 (ash n 2)))
     (emit-vfp-3rdp-inst segment (cond-encoding cnd)
			 fld1 vn rt fld2 #b0000))))

(define-instruction vmsr (segment src &key (cnd :al))
  (:delay 1)
  (:cost 1)
  (:dependencies (reads src))
  (:emitter
   (let ((rt (reg-tn-encoding src)))
     (emit-vfp-3rdp-inst segment (cond-encoding cnd)
			 #b11101110 #b0001 rt #b10100001 #b0000))))

(define-instruction vmrs (segment dest &key (cnd :al))
  (:delay 1)
  (:cost 1)
  (:dependencies (writes dest))
  (:emitter
   (let ((rt (reg-tn-encoding dest)))
     (emit-vfp-3rdp-inst segment (cond-encoding cnd)
			 #b11101111 #b0001 rt #b10100001 #b0000))))

;;; End of A7.8
;;;=============================================================================



;;; Some more macros

(defun %lr (reg value)
  (etypecase value
    ((or (signed-byte 16) (unsigned-byte 16)) (inst movw reg value))
    ((or (signed-byte 32) (unsigned-byte 32))
     (let* ((high-half (ldb (byte 16 16) value))
            (low-half (ldb (byte 16 0) value)))
       (declare (type (unsigned-byte 16) high-half low-half))
       (inst movw reg low-half)
       (inst movt reg high-half)))
    (fixup
     (inst movw reg value)
     (inst movt reg value))))

(define-instruction-macro lr (reg value)
  `(%lr ,reg ,value))



;;;; Instructions for dumping data and header objects.

(define-instruction word (segment word)
  (:declare (type (or (unsigned-byte 32) (signed-byte 32)) word))
  :pinned
  (:delay 0)
  (:emitter
   (emit-word segment word)))

(define-instruction byte (segment byte)
  (:declare (type (or (unsigned-byte 8) (signed-byte 8)) byte))
  :pinned
  (:delay 0)
  (:emitter
   (emit-byte segment byte)))

(define-bitfield-emitter emit-header-object 32
  (byte 24 8) (byte 8 0))

(defun emit-header-data (segment type)
  (emit-back-patch
   segment 4
   #'(lambda (segment posn)
       (emit-word segment
                  (logior type
                          (ash (+ posn (component-header-length))
                               (- n-widetag-bits word-shift)))))))

(define-instruction simple-fun-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment simple-fun-header-widetag)))

(define-instruction lra-header-word (segment)
  :pinned
  (:delay 0)
  (:emitter
   (emit-header-data segment return-pc-header-widetag)))



;;;; Instructions for converting between code objects, functions, and lras.
(defun emit-compute-inst (segment vop dst src label temp calc)
  (emit-chooser
   ;; We emit either 12 or 4 bytes, so we maintain 8 byte alignments.
   segment 12 3
   #'(lambda (segment posn delta-if-after)
       (let ((delta (funcall calc label posn delta-if-after)))
         (when (<= (- (ash 1 15)) delta (1- (ash 1 15)))
           (emit-back-patch
	    segment 4
	    #'(lambda (segment posn)
		(assemble (segment vop)
			  (inst add dst src (funcall calc label posn 0)))))
           t)))
   #'(lambda (segment posn)
       (let ((delta (funcall calc label posn 0)))
         (assemble (segment vop)
		   (inst lr temp delta)
                   (inst add dst src temp))))))

;; code = lip - header - label-offset + other-pointer-tag
(define-instruction compute-code-from-lip (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (- other-pointer-lowtag
                             ;;function-pointer-type
                             (label-position label posn delta-if-after)
                             (component-header-length))))))

;; code = lra - other-pointer-tag - header - label-offset + other-pointer-tag
;;      = lra - (header + label-offset)
(define-instruction compute-code-from-lra (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (- (+ (label-position label posn delta-if-after)
                                (component-header-length)))))))

;; lra = code + other-pointer-tag + header + label-offset - other-pointer-tag
;;     = code + header + label-offset
(define-instruction compute-lra-from-code (segment dst src label temp)
  (:declare (type tn dst src temp) (type label label))
  (:attributes variable-length)
  (:dependencies (reads src) (writes dst) (writes temp))
  (:delay 0)
  (:vop-var vop)
  (:emitter
   (emit-compute-inst segment vop dst src label temp
                      #'(lambda (label posn delta-if-after)
                          (+ (label-position label posn delta-if-after)
                             (component-header-length))))))


;; (defknown %bahor (fixnum fixnum) fixnum (always-translatable))

;; (define-vop (fast-bahor/fixnum=>fixnum)
;;   (:policy :fast-safe)
;;   (:effects)
;;   (:affected)
;;   (:args (x :target r :scs (any-reg))
;;          (y :target r :scs (any-reg)))
;;   (:arg-types fixnum fixnum)
;;   (:results (r :scs (any-reg)))
;;   (:result-types fixnum)
;;   (:note "inline fixnum arithmetic")
;;   (:translate %bahor)
;;   (:generator 2

;;            ;; (move r x)
;;            ;; (inst and r y)
;;            (inst sbcs :le r x y :lsl 2)
;;            ))

;; (defun xxx (a b)
;;   (declare (fixnum a b))
;;   (sb-vm::%bahor a b))
