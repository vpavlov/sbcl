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

;;;; Emitters

;;; Primitive emitters.
(define-bitfield-emitter emit-word 32
  (byte 32 0))


;;; Data processing (immediate) instruction emitter
;;; Template: 
;;; CCCC|0010001|S|Rn  |Rd  |imm12           | -- EOR Rd, Rn, #const
(define-bitfield-emitter emit-dp-imm-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12) (byte 12 0))


;;; Data processing (register) instruction emitter 
;;; Template:
;;; CCCC|0000001|S|Rn  |Rd  |imm5  |tp|0|Rm  | -- EOR Rd, Rn, Rm, sh-op #shift
(define-bitfield-emitter emit-dp-reg-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 5 7) (byte 2 5) (byte 1 4) (byte 4 0))


;;; Data processing (register) instruction emitter 
;;; Template:
;;; CCCC|0000001|S|Rn  |Rd  |Rs  |0|tp|1|Rm  | -- EOR Rd, Rn, Rm, sh-op Rs
(define-bitfield-emitter emit-dp-rsr-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 1 7) (byte 2 5) (byte 1 4) (byte 4 0))


(define-bitfield-emitter emit-a523-inst 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16)
  (byte 4 12) (byte 12 0))
;; 
;;;; Instruction definitions


;;;=============================================================================
;;; A5.2.1. Data Processing (register)
;;; A5.2.2. Data Processing (register-shifted register)
;;; A5.2.3. Data Processing (immediate)
;;;
;;; CCCC|0000001|S|Rn  |Rd  |imm5  |tp|0|Rm  | -- EOR Rd, Rn, Rm, sh-op #shift
;;; CCCC|0000001|S|Rn  |Rd  |Rs  |0|tp|1|Rm  | -- EOR Rd, Rn, Rm, sh-op Rs
;;; CCCC|0010001|S|Rn  |Rd  |imm12           | -- EOR Rd, Rn, #const
(macrolet ((define-dp-instruction (name op)
	     `(define-instruction ,name (segment dst src &key (cnd :al)
						 src2 shift shift-op)
		(:delay 1)
		(:cost 1)
		(:dependencies (reads src) (writes dst))
		(:emitter
		 (let ((fld ,op)
		       src1)
		   (if src2
		       (setf src1 src)
		       (setf src1 dst src2 src))
		   (if shift-op
		       (cond
			 ;; first form (register)
			 ((integerp shift)
			  (emit-dp-reg-inst segment
					    (cond-encoding cnd) ,fld
					    (reg-tn-encoding src1)
					    (reg-tn-encoding dst)
					    shift
					    (shift-op-encoding shift-op) #b0
					    (reg-tn-encoding src2)))
			 ;; second form (register-shifted register)
			 ((register-p shift)
			  (emit-dp-rsr-inst segment
					    (cond-encoding cnd) ,fld
					    (reg-tn-encoding src1)
					    (reg-tn-encoding dst)
					    (reg-tn-encoding shift)
					    #b0 (shift-op-encoding shift-op) #b1
					    (reg-tn-encoding src2)))
			 (t
			  (error "Bad shift operand in ~a!" ,name)))
		       (if (register-p src2)
			   ;; first form (register, special case of 0 shift)
			   (emit-dp-reg-inst segment
					     (cond-encoding cnd) ,fld
					     (reg-tn-encoding src1)
					     (reg-tn-encoding dst)
					     #b00000 #b00 #b0
					     (reg-tn-encoding src2))
			   ;; third form (immediate)
			   (emit-dp-imm-inst segment
					     (cond-encoding cnd)
					     (logior #b100000 ,fld)
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
		`(inst ,,base ,dst (make-random-tn :kind :normal
						   :sc (sc-or-lose 'any-reg)
						   :offset 0)
		       :cnd ,cnd
		       :src2 ,src)))
	   (shift-frob (name base shift-op)
	     `(define-instruction-macro ,name (dst src shift &key (cnd :al))
		`(inst ,,base ,dst (make-random-tn :kind :normal
						   :sc (sc-or-lose 'any-reg)
						   :offset 0)
		       :cnd ,cnd
		       :src2 ,src
		       :shift-op ,,shift-op
		       :shift ,shift))))
  (mov-frob   mov  %mov)
  (mov-frob   movs %movs)
  (shift-frob lsl  %mov  :lsl)
  (shift-frob lsls %movs :lsl)
  (shift-frob lsr  %mov  :lsr)
  (shift-frob lsrs %movs :lsr)
  (shift-frob asr  %mov  :asr)
  (shift-frob asrs %movs :asr)
  (shift-frob ror  %mov  :ror)
  (shift-frob rors %movs :ror))

(define-instruction-macro rrx (dst src &key (cnd :al))
  `(inst ror ,dst ,src 0 :cnd ,cnd))

(define-instruction-macro rrxs (dst src &key (cnd :al))
  `(inst rors ,dst ,src 0 :cnd ,cnd))

;;;
;;; End of A5.2.1, A5.2.2 and A5.2.3
;;;=============================================================================


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

;; A5.2.12
(macrolet (
           (define-a5212-instruction (name op1 op2)
             `(define-instruction ,name (segment rm &key (cnd :al))
                (:emitter
                 (emit-dp-reg-inst segment (cond-encoding cnd)
				 #b00010010 #b1111 #b1111 #b11110
				 ,op1 ,op2 (reg-tn-encoding rm)))))
           )
  (define-a5212-instruction bx  #b00 #b1)
  (define-a5212-instruction bxj #b01 #b0)
  (define-a5212-instruction blx #b01 #b1))

;; A5.5 Branch, branch with link and block data transfer
(define-instruction b (segment where &key (cnd :al))
  (:emitter
   (emit-chooser
    segment 4 0
    (lambda (segment posn delta-if-after)
      (declare (ignore segment posn delta-if-after))
    (lambda (segment posn)
      ;; @@@FIXME: this calculation maybe wrong, but I don't know yet, I
      ;;           just produced it from the top of my head, following
      ;;           whatever leads I got, but am not 100% sure that its fine
      ;; --vnp 2013-05-17
      (let ((disp (ash (- (label-position where) (+ posn 4)) -2)))
	(emit-word segment
		   (dpb (cond-encoding cnd)
			(byte 4 28)
			(dpb #b1010 (byte 4 24) disp)))))))))

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





;; 
;; ;;; Instruction formats and corresponding emitters


;; (define-bitfield-emitter emit-a525-inst 32
;;   (byte 4 28) (byte 4 24) (byte 4 20) (byte 4 16) (byte 4 12)
;;   (byte 4 8) (byte 4 4) (byte 4 0))

;; (define-bitfield-emitter emit-a526-inst 32
;;   (byte 4 28) (byte 5 23) (byte 2 21) (byte 1 20)
;;   (byte 4 16) (byte 4 12) (byte 8 4) (byte 4 0))

;; (define-bitfield-emitter emit-a527-inst 32
;;   (byte 4 28) (byte 5 23) (byte 2 21) (byte 1 20)
;;   (byte 4 16) (byte 4 12) (byte 4 8) (byte 4 4) (byte 4 0))

;; (define-bitfield-emitter emit-a528-inst 32
;;   (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
;;   (byte 4 8) (byte 4 4) (byte 4 0))

;; (define-bitfield-emitter emit-a5211-inst 32
;;   (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12) (byte 12 0))

;; (define-bitfield-emitter emit-a5212-inst 32
;;   (byte 4 28) (byte 8 20) (byte 12 8) (byte 4 4) (byte 4 0))
 


;; ;; A5.2.5 Multiply and multiply-accumulate -- 14 insts
;; (macrolet (
;;            ;; MUL Rd, Rn, Rm
;;            (define-a525-subtype1-instruction (name op)
;;              `(define-instruction ,name (segment cnd rd rn rm)
;;                 (:emitter (emit-a525-inst segment (conditional-opcode cnd)
;;                                           #b0000 ,op 
;;                                           (reg-encode rd) #b0000
;;                                           (reg-encode rm)
;;                                           #b1001
;;                                           (reg-encode rn)))))

;;            ;; instructions like:
;;            ;; MLA Rd, Rn, Rm, Ra
;;            (define-a525-subtype2-instruction (name op)
;;              `(define-instruction ,name (segment cnd rd rn rm ra)
;;                 (:emitter (emit-a525-inst segment (conditional-opcode cnd)
;;                                           #b0000 ,op
;;                                           (reg-encode rd)
;;                                           (reg-encode ra)
;;                                           (reg-encode rm)
;;                                           #b1001
;;                                           (reg-encode rn)))))

;;            ;; instructions like:
;;            ;; UMAAL RdLo, RdHi, Rn, Rm
;;            (define-a525-subtype3-instruction (name op)
;;              `(define-instruction ,name (segment cnd rdlo rdhi rn rm)
;;                 (:emitter (emit-a525-inst segment (conditional-opcode cnd)
;;                                           #b0000 ,op
;;                                           (reg-encode rdhi)
;;                                           (reg-encode rdlo)
;;                                           (reg-encode rm)
;;                                           #b1001
;;                                           (reg-encode rn)))))
;;            )
;;   (define-a525-subtype1-instruction mul    #b0000)
;;   (define-a525-subtype1-instruction muls   #b0001)
;;   (define-a525-subtype2-instruction mla    #b0010)
;;   (define-a525-subtype2-instruction mlas   #b0011)
;;   (define-a525-subtype3-instruction umaal  #b0100)
;;                                         ;; #b0101  undefined
;;   (define-a525-subtype2-instruction mls    #b0110)
;;                                         ;; #b0111  undefined
;;   (define-a525-subtype3-instruction umull  #b1000)
;;   (define-a525-subtype3-instruction umulls #b1001)
;;   (define-a525-subtype3-instruction umlal  #b1010)
;;   (define-a525-subtype3-instruction umlals #b1011)
;;   (define-a525-subtype3-instruction smull  #b1100)
;;   (define-a525-subtype3-instruction smulls #b1101)
;;   (define-a525-subtype3-instruction smlal  #b1110)
;;   (define-a525-subtype3-instruction smlals #b1111))


;; ;; A5.2.6 Saturating addition and substraction -- 4 insts
;; (macrolet (
;;            ;; QADD Rd, Rm, Rn
;;            (define-a526-instruction (name op)
;;              `(define-instruction ,name (segment cnd rd rn rm)
;;                 (:emitter (emit-a526-inst segment (conditional-opcode cnd)
;;                                           #b00010 ,op #b0
;;                                           (reg-encode rn)
;;                                           (reg-encode rd)
;;                                           #b00000101
;;                                           (reg-encode rm)))))
;;            )
;;   (define-a526-instruction qadd #b00)
;;   (define-a526-instruction qsub #b01)
;;   (define-a526-instruction qdadd #b10)
;;   (define-a526-instruction qdsub #b11))


;; ;; A5.2.7 Halfword multiply and multiply-accumulate -- 16 insts
;; (macrolet (
;;            ;; SMLABB Rd, Rn, Rm, Ra
;;            ;; SMLAWB Rd, Rn, Rm, Ra
;;            (define-a527-subtype1-instruction (name op1 op2)
;;              `(define-instruction ,name (segment cnd rd rn rm ra)
;;                 (:emitter (emit-a527-inst segment (conditional-opcode cnd)
;;                                           #b00010 ,op1 #b0
;;                                           (reg-encode rd)
;;                                           (reg-encode ra)
;;                                           (reg-encode rm)
;;                                           ,op2
;;                                           (reg-encode rn)))))

;;            ;; SMULWB Rd, Rn, Rm
;;            ;; SMULBB Rd, Rn, Rm
;;            (define-a527-subtype2-instruction (name op1 op2)
;;              `(define-instruction ,name (segment cnd rd rn rm)
;;                 (:emitter (emit-a527-inst segment (conditional-opcode cnd)
;;                                           #b00010 ,op1 #b0
;;                                           (reg-encode rd)
;;                                           #b0000
;;                                           (reg-encode rm)
;;                                           ,op2
;;                                           (reg-encode rn)))))

;;            ;; SMLALBB RdLo, RdHi, Rn, Rm
;;            (define-a527-subtype3-instruction (name op1 op2)
;;              `(define-instruction ,name (segment cnd rdlo rdhi rn rm)
;;                 (:emitter (emit-a527-inst segment (conditional-opcode cnd)
;;                                           #b00010 ,op1 #b0
;;                                           (reg-encode rdhi)
;;                                           (reg-encode rdlo)
;;                                           (reg-encode rm)
;;                                           ,op2
;;                                           (reg-encode rn)))))


;;            )
;;   (define-a527-subtype1-instruction smlabb  #b00 #b1000)
;;   (define-a527-subtype1-instruction smlatb  #b00 #b1010)
;;   (define-a527-subtype1-instruction smlabt  #b00 #b1100)
;;   (define-a527-subtype1-instruction smlatt  #b00 #b1110)
;;   (define-a527-subtype1-instruction smlawb  #b01 #b1000)
;;   (define-a527-subtype2-instruction smulwb  #b01 #b1010)
;;   (define-a527-subtype1-instruction smlawt  #b01 #b1100)
;;   (define-a527-subtype2-instruction smulwt  #b01 #b1110)
;;   (define-a527-subtype3-instruction smlalbb #b10 #b1000)
;;   (define-a527-subtype3-instruction smlaltb #b10 #b1010)
;;   (define-a527-subtype3-instruction smlalbt #b10 #b1100)
;;   (define-a527-subtype3-instruction smlaltt #b10 #b1110)
;;   (define-a527-subtype2-instruction smulbb  #b11 #b1110)
;;   (define-a527-subtype2-instruction smultb  #b11 #b1110)
;;   (define-a527-subtype2-instruction smulbt  #b11 #b1110)
;;   (define-a527-subtype2-instruction smultt  #b11 #b1110)
;; )

;; ;; TODO: implement the [] syntax printing in disassembly
;; (macrolet (
;;            ;; STRH Rt, [Rn, -Rm]!
;;            (define-a528-instruction (name op1 op2)
;;              `(define-instruction ,name (segment cnd rt rn rm-or-imm4l
;;                                                  &key add wback index (imm4h 0))
;;                 (:emitter
;;                  (let ((fld #b00000000))
;;                    (setf fld (logior fld
;;                                      ;; TODO: unbox 0 1 and immed values
;;                                      (ash (if (typep rm-or-imm4l 'tn) 0 1) 2)
;;                                      ,op1))
;;                    (when index (setf fld (logior fld #b00010000)))
;;                    (when add   (setf fld (logior fld #b00001000)))
;;                    (when wback (setf fld (logior fld #b00000010)))
;;                    (emit-a528-inst segment (conditional-opcode cnd) fld
;;                                    (reg-encode rn)
;;                                    (reg-encode rt)
;;                                    (if (typep rm-or-imm4l 'tn) #b0000 imm4h)
;;                                    ,op2
;;                                    (if (typep rm-or-imm4l 'tn)
;;                                        (reg-encode rm-or-imm4l)
;;                                        rm-or-imm4l))))))
;;            )
;;   (define-a528-instruction strh  0 #b1011)
;;   (define-a528-instruction ldrh  1 #b1011)
;;   (define-a528-instruction ldrd  0 #b1101)
;;   (define-a528-instruction ldrsb 1 #b1101)
;;   (define-a528-instruction strd  0 #b1111)
;;   (define-a528-instruction ldrsh 1 #b1111)
;; )

;; ;; TODO: implement the [] syntax printing in disassembly
;; (macrolet (
;;            ;; STRH Rt, [Rn, -Rm]!
;;            (define-a529-instruction (name op1 op2)
;;              `(define-instruction ,name (segment cnd rt rn rm-or-imm4l
;;                                                  &key add (imm4h 0))
;;                 (:emitter
;;                  (let ((fld #b00000010))
;;                    (setf fld (logior fld
;;                                      ;; TODO: unbox 0 1 and immed values
;;                                      (ash (if (typep rm-or-imm4l 'tn) 0 1) 2)
;;                                      ,op1))
;;                    (when add (setf fld (logior fld #b00001000)))
;;                    (emit-a528-inst segment (conditional-opcode cnd) fld
;;                                    (reg-encode rn)
;;                                    (reg-encode rt)
;;                                    (if (typep rm-or-imm4l 'tn) #b0000 imm4h)
;;                                    ,op2
;;                                    (if (typep rm-or-imm4l 'tn)
;;                                        (reg-encode rm-or-imm4l)
;;                                        rm-or-imm4l))))))
;;            )
;;   (define-a529-instruction strht  0 #b1011)
;;   (define-a529-instruction ldrht  1 #b1011)
;;   (define-a529-instruction ldrsbt 1 #b1101)
;;   (define-a529-instruction ldrsht 1 #b1111)
;;   )

;; (macrolet (
;;            ;; SWP, SWPB
;;            (define-a5210-subtype1-instruction (name fld)
;;              `(define-instruction ,name (segment cnd rt rt2 rn)
;;                 (:emitter
;;                  (emit-a528-inst segment (conditional-opcode cnd)
;;                                  ,fld
;;                                  (reg-encode rn)
;;                                  (reg-encode rt)
;;                                  #b0000 #b1001
;;                                  (reg-encode rt2)))))

;;            ;; LDREX, LDREXD, LDREXB, LDREXH
;;            (define-a5210-subtype2-instruction (name fld)
;;              `(define-instruction ,name (segment cnd rd rt rn)
;;                 (:emitter
;;                  (emit-a528-inst segment (conditional-opcode cnd)
;;                                  ,fld
;;                                  (reg-encode rn)
;;                                  (reg-encode rd)
;;                                  #b1111 #b1001
;;                                  (reg-encode rt)))))

;;            ;; STREX, STREXD, STREXB, STREXH
;;            (define-a5210-subtype3-instruction (name fld)
;;              `(define-instruction ,name (segment cnd rt rn)
;;                 (:emitter
;;                  (emit-a528-inst segment (conditional-opcode cnd)
;;                                  ,fld
;;                                  (reg-encode rn)
;;                                  (reg-encode rt)
;;                                  #b1111 #b1001 #b1111))))
;;            )
;;   (define-a5210-subtype1-instruction swp    #b00010000)
;;   (define-a5210-subtype1-instruction swpb   #b00010100)
;;   (define-a5210-subtype2-instruction strex  #b00011000)
;;   (define-a5210-subtype3-instruction ldrex  #b00011001)
;;   (define-a5210-subtype2-instruction strexd #b00011010)
;;   (define-a5210-subtype3-instruction ldrexd #b00011011)
;;   (define-a5210-subtype2-instruction strexb #b00011100)
;;   (define-a5210-subtype3-instruction ldrexb #b00011101)
;;   (define-a5210-subtype2-instruction strexh #b00011110)
;;   (define-a5210-subtype3-instruction ldrexh #b00011111)
;;   )

;; (macrolet (
;;            ;; NOP and the other hints
;;            (define-a5211-subtype1-instruction (name op21 op22)
;;              `(define-instruction ,name (segment cnd)
;;                 (:emitter
;;                  (emit-a528-inst segment (conditional-opcode cnd)
;;                                   #b00110010
;;                                   #b0000 #b1111 #b0000
;;                                   ,op21 ,op22))))

;;            (define-a5211-subtype2-instruction (name op21)
;;              `(define-instruction ,name (segment cnd option)
;;                 (:emitter
;;                  (emit-a528-inst segment (conditional-opcode cnd)
;;                                  #b00110010
;;                                  #b0000 #b1111 #b0000
;;                                  ,op21 option))))

;;            (define-a5211-subtype3-instruction (name)
;;              `(define-instruction ,name (segment cnd spec-reg mask
;;                                                  rn-or-immed12)
;;                 (:emitter
;;                  (if (typep rn-or-immed12 'tn)
;;                      (let ((msk (ash mask 2)))
;;                        (emit-a525-inst segment (conditional-opcode cnd)
;;                                        #b0001 #b0010 msk
;;                                        #b1111 #b0000 #b0000
;;                                        (reg-encode rn-or-immed12)))
;;                      (let ((fld (cond
;;                                   ((eq spec-reg :cpsr) #b00110010)
;;                                   ((eq spec-reg :spsr) #b00110110)
;;                                   (t (error "unknown spec-reg in a5211!")))))
;;                        (emit-a5211-inst segment (conditional-opcode cnd)
;;                                         fld
;;                                         mask
;;                                         #b1111
;;                                         (ror-immed12 rn-or-immed12)))))))
;;            )
;;   (define-a5211-subtype1-instruction nop   #b0000 #b0000)
;;   (define-a5211-subtype1-instruction yield #b0000 #b0001)
;;   (define-a5211-subtype1-instruction wfe   #b0000 #b0010)
;;   (define-a5211-subtype1-instruction wfi   #b0000 #b0011)
;;   (define-a5211-subtype1-instruction sev   #b0000 #b0100)
;;   (define-a5211-subtype2-instruction dbg   #b1111)
;;   (define-a5211-subtype3-instruction msr)
;;   )


;; (macrolet (
;;            (define-a5212-subtype1-instruction (name op1)
;;              `(define-instruction ,name (segment cnd rm)
;;                 (:emitter
;;                  (emit-a525-inst segment (conditional-opcode cnd) #b0001 #b0010
;;                                  #b1111 #b1111 #b1111 ,op1 (reg-encode rm)))))
;;            )

;;   (define-a5212-subtype1-instruction bx  #b0001)
;;   (define-a5212-subtype1-instruction bxj #b0010)
;;   (define-a5212-subtype1-instruction blx #b0011)

;;   (define-instruction mrs (segment cnd rd)
;;     (:emitter
;;      (emit-a5211-inst segment (conditional-opcode cnd) #b00010000 #b1111
;;                       (reg-encode rd) #b000000000000)))

;;   (define-instruction clz (segment cnd rd rm)
;;     (:emitter
;;      (emit-a525-inst segment (conditional-opcode cnd) #b0001 #b0110 #b1111
;;                      (reg-encode rd) #b1111 #b0001 (reg-encode rm))))

;;   (define-instruction bkpt (segment immed4 &optional (immed12 0))
;;     (:emitter
;;      (emit-a5212-inst segment #b1110 #b00010010 immed12 #b0111 immed4)))

;;   (define-instruction smc (segment cnd immed4)
;;     (:emitter
;;      (emit-a5212-inst segment (conditional-opcode cnd) #b00010110
;;                       #b000000000000 #b0111 immed4)))
;; )


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