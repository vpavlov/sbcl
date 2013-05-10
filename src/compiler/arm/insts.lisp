;;;; the instruction set definition for the ARMv6/VFPv2

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

;;;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;;;; BIG TODO: handle fixups, immeds, etc.
;;;;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

(in-package "SB!VM")

;;; Printers for the various instruction arguments


;; 'Conditional' field of the instructions. Print the symbolic name of the
;; condition. Do not print :AL and :XX to keep the output clean.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *cond-values-alist*
    '((:eq .  0) (:ne .  1) (:cs .  2) (:cc .  3)
      (:mi .  4) (:pl .  5) (:vs .  6) (:vc .  7)
      (:hi .  8) (:ls .  9) (:ge . 10) (:lt . 11)
      (:gt . 12) (:le . 13) (:al . 14) (:xx . 15))))

(defun conditional-opcode (condition)
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
                 (let ((regname (aref *core-register-names* value)))
                   (princ regname stream)
                   (sb!disassem:maybe-note-associated-storage-ref
                    value 'registers regname dstate))))

(defun reg-encoding (tn)
  (declare (type tn tn))
  (aver (eq (sb-name (sc-sb (tn-sc tn))) 'registers))
  (tn-offset tn))


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

;; 12-bit modified immediate constants, see Section A5.2.4
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

(defun type-opcode (type)
  (cdr (assoc type *shift-types-alist2* :test #'eq)))

(sb!disassem:define-arg-type shift-type
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

;;; Instruction formats and corresponding emitters

(define-bitfield-emitter emit-a521-inst 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16)
  (byte 4 12) (byte 5 7) (byte 2 5) (byte 1 4) (byte 4 0))

(define-bitfield-emitter emit-a522-inst 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16)
  (byte 4 12) (byte 4 8) (byte 1 7) (byte 2 5) (byte 1 4) (byte 4 0))

(define-bitfield-emitter emit-a523-inst 32
  (byte 4 28) (byte 3 25) (byte 5 20) (byte 4 16) (byte 4 12) (byte 12 0))

(define-bitfield-emitter emit-a525-inst 32
  (byte 4 28) (byte 4 24) (byte 4 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 4 4) (byte 4 0))

(define-bitfield-emitter emit-a526-inst 32
  (byte 4 28) (byte 5 23) (byte 2 21) (byte 1 20)
  (byte 4 16) (byte 4 12) (byte 8 4) (byte 4 0))

(define-bitfield-emitter emit-a527-inst 32
  (byte 4 28) (byte 5 23) (byte 2 21) (byte 1 20)
  (byte 4 16) (byte 4 12) (byte 4 8) (byte 4 4) (byte 4 0))

(define-bitfield-emitter emit-a528-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12)
  (byte 4 8) (byte 4 4) (byte 4 0))

(define-bitfield-emitter emit-a5211-inst 32
  (byte 4 28) (byte 8 20) (byte 4 16) (byte 4 12) (byte 12 0))

(define-bitfield-emitter emit-a5212-inst 32
  (byte 4 28) (byte 8 20) (byte 12 8) (byte 4 4) (byte 4 0))

;;; Instruction definitions


;; A5.2.1, A5.2.2 and A5.2.3 Data processing (register, register-shifter
;; register and immediate) -- 38 insts
(macrolet (
           ;; instructions like:
           ;; AND Rd, Rn, Rm LSL #5 (A5.2.1)
           ;; AND Rd, Rn, Rm LSL Rs (A5.2.2)
           ;; AND Rd, Rn, #const    (A5.2.3)
           (define-a521/2/3-instruction (name op1)
             `(define-instruction ,name (segment cnd rd rn rm-or-imm12
                                                 &optional (type 0)
                                                 (rs-or-imm5 0))
                (:emitter
                 ;; TODO: unbox immed values
                 (if (typep rm-or-imm12 tn)
                     (if (typep rs-or-imm5 tn)
                         (emit-a522-inst segment (conditional-opcode cnd)
                                         #b000 ,op1 (reg-encoding rn)
                                         (reg-encoding rd)
                                         (reg-encoding rs-or-imm5) #b0
                                         (type-opcode type) #b1
                                         (reg-encoding rm-or-imm12))
                         (emit-a521-inst segment (conditional-opcode cnd)
                                         #b000 ,op1 (reg-encoding rn)
                                         (reg-encoding rd) rs-or-imm5
                                         (type-opcode type) #b0
                                         (reg-encoding rm-or-imm12)))
                     (emit-a523-inst segment (conditional-opcode cnd)
                                     #b001 ,op1 (reg-encoding rn)
                                     (reg-encoding rd)
                                     rm-or-imm12)))))

           ;; MOV Rd, Rm
           ;; RRX Rd, Rm
           (define-a521-subtype2-instruction (name op1 op3)
             `(define-instruction ,name (segment cnd rd rm)
                (:emitter (emit-a521-inst segment (conditional-opcode cnd)
                                          #b000 ,op1 #b0000
                                          (reg-encoding rd) #b00000
                                          ,op3 #b0
                                          (reg-encoding rm)))))

           ;; instructions like:
           ;; LSL Rd, Rm, #const (A5.2.1)
           ;; LSL Rd, Rm, Rs     (A5.2.2)
           (define-a521/2-subtype3-instruction (name op1 op3)
             `(define-instruction ,name (segment cnd rd rm rs-or-imm5)
                (:emitter
                 ;; TODO: unbox immed values
                 (if (typep rs-or-imm5 tn)
                     (emit-a522-inst segment (conditional-opcode cnd)
                                     #b000 ,op1 #b0000
                                     (reg-encoding rd)
                                     (reg-encoding rs-or-imm5) #b0
                                     ,op3 #b1
                                     (reg-encoding rm))
                     (emit-a521-inst segment (conditional-opcode cnd)
                                     #b000 ,op1 #b0000
                                     (reg-encoding rd) rs-or-imm5
                                     ,op3 #b0
                                     (reg-encoding rm))))))
           )
  (define-a521/2/3-instruction        and  #b00000)
  (define-a521/2/3-instruction        ands #b00001)
  (define-a521/2/3-instruction        eor  #b00010)
  (define-a521/2/3-instruction        eors #b00011)
  (define-a521/2/3-instruction        sub  #b00100)
  (define-a521/2/3-instruction        subs #b00101)
  (define-a521/2/3-instruction        rsb  #b00110)
  (define-a521/2/3-instruction        rsbs #b00111)
  (define-a521/2/3-instruction        add  #b01000)
  (define-a521/2/3-instruction        adds #b01001)
  (define-a521/2/3-instruction        adc  #b01010)
  (define-a521/2/3-instruction        adcs #b01011)
  (define-a521/2/3-instruction        sbc  #b01100)
  (define-a521/2/3-instruction        sbcs #b01101)
  (define-a521/2/3-instruction        rsc  #b01110)
  (define-a521/2/3-instruction        rscs #b01111)
  (define-a521/2/3-instruction        tst  #b10001)
  (define-a521/2/3-instruction        teq  #b10011)
  (define-a521/2/3-instruction        cmp  #b10101)
  (define-a521/2/3-instruction        cmn  #b10111)
  (define-a521/2/3-instruction        orr  #b11000)
  (define-a521/2/3-instruction        orrs #b11001)
  (define-a521-subtype2-instruction   mov  #b11010 #b00)
  (define-a521-subtype2-instruction   movs #b11011 #b00)
  (define-a521/2-subtype3-instruction lsl  #b11010 #b00)
  (define-a521/2-subtype3-instruction lsls #b11011 #b00)
  (define-a521/2-subtype3-instruction lsr  #b11010 #b01)
  (define-a521/2-subtype3-instruction lsrs #b11011 #b01)
  (define-a521/2-subtype3-instruction asr  #b11010 #b10)
  (define-a521/2-subtype3-instruction asrs #b11011 #b10)
  (define-a521-subtype2-instruction   rrx  #b11010 #b11)
  (define-a521-subtype2-instruction   rrxs #b11011 #b11)
  (define-a521/2-subtype3-instruction ror  #b11010 #b11)
  (define-a521/2-subtype3-instruction rors #b11011 #b11)
  (define-a521/2/3-instruction        bic  #b11100)
  (define-a521/2/3-instruction        bics #b11101)
  (define-a521/2/3-instruction        mvn  #b11110)
  (define-a521/2/3-instruction        mvns #b11111))


;; A5.2.5 Multiply and multiply-accumulate -- 14 insts
(macrolet (
           ;; MUL Rd, Rn, Rm
           (define-a525-subtype1-instruction (name op)
             `(define-instruction ,name (segment cnd rd rn rm)
                (:emitter (emit-a525-inst segment (conditional-opcode cnd)
                                          #b0000 ,op 
                                          (reg-encoding rd) #b0000
                                          (reg-encoding rm)
                                          #b1001
                                          (reg-encoding rn)))))

           ;; instructions like:
           ;; MLA Rd, Rn, Rm, Ra
           (define-a525-subtype2-instruction (name op)
             `(define-instruction ,name (segment cnd rd rn rm ra)
                (:emitter (emit-a525-inst segment (conditional-opcode cnd)
                                          #b0000 ,op
                                          (reg-encoding rd)
                                          (reg-encoding ra)
                                          (reg-encoding rm)
                                          #b1001
                                          (reg-encoding rn)))))

           ;; instructions like:
           ;; UMAAL RdLo, RdHi, Rn, Rm
           (define-a525-subtype3-instruction (name op)
             `(define-instruction ,name (segment cnd rdlo rdhi rn rm)
                (:emitter (emit-a525-inst segment (conditional-opcode cnd)
                                          #b0000 ,op
                                          (reg-encoding rdhi)
                                          (reg-encoding rdlo)
                                          (reg-encoding rm)
                                          #b1001
                                          (reg-encoding rn)))))
           )
  (define-a525-subtype1-instruction mul    #b0000)
  (define-a525-subtype1-instruction muls   #b0001)
  (define-a525-subtype2-instruction mla    #b0010)
  (define-a525-subtype2-instruction mlas   #b0011)
  (define-a525-subtype3-instruction umaal  #b0100)
                                        ;; #b0101  undefined
  (define-a525-subtype2-instruction mls    #b0110)
                                        ;; #b0111  undefined
  (define-a525-subtype3-instruction umull  #b1000)
  (define-a525-subtype3-instruction umulls #b1001)
  (define-a525-subtype3-instruction umlal  #b1010)
  (define-a525-subtype3-instruction umlals #b1011)
  (define-a525-subtype3-instruction smull  #b1100)
  (define-a525-subtype3-instruction smulls #b1101)
  (define-a525-subtype3-instruction smlal  #b1110)
  (define-a525-subtype3-instruction smlals #b1111))


;; A5.2.6 Saturating addition and substraction -- 4 insts
(macrolet (
           ;; QADD Rd, Rm, Rn
           (define-a526-instruction (name op)
             `(define-instruction ,name (segment cnd rd rn rm)
                (:emitter (emit-a526-inst segment (conditional-opcode cnd)
                                          #b00010 ,op #b0
                                          (reg-encoding rn)
                                          (reg-encoding rd)
                                          #b00000101
                                          (reg-encoding rm)))))
           )
  (define-a526-instruction qadd #b00)
  (define-a526-instruction qsub #b01)
  (define-a526-instruction qdadd #b10)
  (define-a526-instruction qdsub #b11))


;; A5.2.7 Halfword multiply and multiply-accumulate -- 16 insts
(macrolet (
           ;; SMLABB Rd, Rn, Rm, Ra
           ;; SMLAWB Rd, Rn, Rm, Ra
           (define-a527-subtype1-instruction (name op1 op2)
             `(define-instruction ,name (segment cnd rd rn rm ra)
                (:emitter (emit-a527-inst segment (conditional-opcode cnd)
                                          #b00010 ,op1 #b0
                                          (reg-encoding rd)
                                          (reg-encoding ra)
                                          (reg-encoding rm)
                                          ,op2
                                          (reg-encoding rn)))))

           ;; SMULWB Rd, Rn, Rm
           ;; SMULBB Rd, Rn, Rm
           (define-a527-subtype2-instruction (name op1 op2)
             `(define-instruction ,name (segment cnd rd rn rm)
                (:emitter (emit-a527-inst segment (conditional-opcode cnd)
                                          #b00010 ,op1 #b0
                                          (reg-encoding rd)
                                          #b0000
                                          (reg-encoding rm)
                                          ,op2
                                          (reg-encoding rn)))))

           ;; SMLALBB RdLo, RdHi, Rn, Rm
           (define-a527-subtype3-instruction (name op1 op2)
             `(define-instruction ,name (segment cnd rdlo rdhi rn rm)
                (:emitter (emit-a527-inst segment (conditional-opcode cnd)
                                          #b00010 ,op1 #b0
                                          (reg-encoding rdhi)
                                          (reg-encoding rdlo)
                                          (reg-encoding rm)
                                          ,op2
                                          (reg-encoding rn)))))


           )
  (define-a527-subtype1-instruction smlabb  #b00 #b1000)
  (define-a527-subtype1-instruction smlatb  #b00 #b1010)
  (define-a527-subtype1-instruction smlabt  #b00 #b1100)
  (define-a527-subtype1-instruction smlatt  #b00 #b1110)
  (define-a527-subtype1-instruction smlawb  #b01 #b1000)
  (define-a527-subtype2-instruction smulwb  #b01 #b1010)
  (define-a527-subtype1-instruction smlawt  #b01 #b1100)
  (define-a527-subtype2-instruction smulwt  #b01 #b1110)
  (define-a527-subtype3-instruction smlalbb #b10 #b1000)
  (define-a527-subtype3-instruction smlaltb #b10 #b1010)
  (define-a527-subtype3-instruction smlalbt #b10 #b1100)
  (define-a527-subtype3-instruction smlaltt #b10 #b1110)
  (define-a527-subtype2-instruction smulbb  #b11 #b1110)
  (define-a527-subtype2-instruction smultb  #b11 #b1110)
  (define-a527-subtype2-instruction smulbt  #b11 #b1110)
  (define-a527-subtype2-instruction smultt  #b11 #b1110)
)

;; TODO: implement the [] syntax printing in disassembly
(macrolet (
           ;; STRH Rt, [Rn, -Rm]!
           (define-a528-instruction (name op1 op2)
             `(define-instruction ,name (segment cnd rt rn rm-or-imm4l
                                                 &key add wback index (imm4h 0))
                (:emitter
                 (let ((fld #b00000000))
                   (setf fld (logior fld
                                     ;; TODO: unbox 0 1 and immed values
                                     (ash (if (typep rm-or-imm4l tn) 0 1) 2)
                                     ,op1))
                   (when index (setf fld (logior fld #b00010000)))
                   (when add   (setf fld (logior fld #b00001000)))
                   (when wback (setf fld (logior fld #b00000010)))
                   (emit-a528-inst segment (conditional-opcode cnd) fld
                                   (reg-encoding rn)
                                   (reg-encoding rt)
                                   (if (typep rm-or-imm4l tn) #b0000 imm4h)
                                   ,op2
                                   (if (typep rm-or-imm4l tn)
                                       (reg-encoding rm-or-imm4l)
                                       rm-or-imm4l))))))
           )
  (define-a528-instruction strh  0 #b1011)
  (define-a528-instruction ldrh  1 #b1011)
  (define-a528-instruction ldrd  0 #b1101)
  (define-a528-instruction ldrsb 1 #b1101)
  (define-a528-instruction strd  0 #b1111)
  (define-a528-instruction ldrsh 1 #b1111)
)

;; TODO: implement the [] syntax printing in disassembly
(macrolet (
           ;; STRH Rt, [Rn, -Rm]!
           (define-a529-instruction (name op1 op2)
             `(define-instruction ,name (segment cnd rt rn rm-or-imm4l
                                                 &key add (imm4h 0))
                (:emitter
                 (let ((fld #b00000010))
                   (setf fld (logior fld
                                     ;; TODO: unbox 0 1 and immed values
                                     (ash (if (typep rm-or-imm4l tn) 0 1) 2)
                                     ,op1))
                   (when add (setf fld (logior fld #b00001000)))
                   (emit-a528-inst segment (conditional-opcode cnd) fld
                                   (reg-encoding rn)
                                   (reg-encoding rt)
                                   (if (typep rm-or-imm4l tn) #b0000 imm4h)
                                   ,op2
                                   (if (typep rm-or-imm4l tn)
                                       (reg-encoding rm-or-imm4l)
                                       rm-or-imm4l))))))
           )
  (define-a529-instruction strht  0 #b1011)
  (define-a529-instruction ldrht  1 #b1011)
  (define-a529-instruction ldrsbt 1 #b1101)
  (define-a529-instruction ldrsht 1 #b1111)
  )

(macrolet (
           ;; SWP, SWPB
           (define-a5210-subtype1-instruction (name fld)
             `(define-instruction ,name (segment cnd rt rt2 rn)
                (:emitter
                 (emit-a528-inst segment (conditional-opcode cnd)
                                 ,fld
                                 (reg-encoding rn)
                                 (reg-encoding rt)
                                 #b0000 #b1001
                                 (reg-encoding rt2)))))

           ;; LDREX, LDREXD, LDREXB, LDREXH
           (define-a5210-subtype2-instruction (name fld)
             `(define-instruction ,name (segment cnd rd rt rn)
                (:emitter
                 (emit-a528-inst segment (conditional-opcode cnd)
                                 ,fld
                                 (reg-encoding rn)
                                 (reg-encoding rd)
                                 #b1111 #b1001
                                 (reg-encoding rt)))))

           ;; STREX, STREXD, STREXB, STREXH
           (define-a5210-subtype3-instruction (name fld)
             `(define-instruction ,name (segment cnd rt rn)
                (:emitter
                 (emit-a528-inst segment (conditional-opcode cnd)
                                 ,fld
                                 (reg-encoding rn)
                                 (reg-encoding rt)
                                 #b1111 #b1001 #b1111))))
           )
  (define-a5210-subtype1-instruction swp    #b00010000)
  (define-a5210-subtype1-instruction swpb   #b00010100)
  (define-a5210-subtype2-instruction strex  #b00011000)
  (define-a5210-subtype3-instruction ldrex  #b00011001)
  (define-a5210-subtype2-instruction strexd #b00011010)
  (define-a5210-subtype3-instruction ldrexd #b00011011)
  (define-a5210-subtype2-instruction strexb #b00011100)
  (define-a5210-subtype3-instruction ldrexb #b00011101)
  (define-a5210-subtype2-instruction strexh #b00011110)
  (define-a5210-subtype3-instruction ldrexh #b00011111)
  )

(macrolet (
           ;; NOP and the other hints
           (define-a5211-subtype1-instruction (name op21 op22)
             `(define-instruction ,name (segment cnd)
                (:emitter
                 (emit-a528-inst segment (conditional-opcode cnd)
                                  #b00110010
                                  #b0000 #b1111 #b0000
                                  ,op21 ,op22))))

           (define-a5211-subtype2-instruction (name op21)
             `(define-instruction ,name (segment cnd option)
                (:emitter
                 (emit-a528-inst segment (conditional-opcode cnd)
                                 #b00110010
                                 #b0000 #b1111 #b0000
                                 ,op21 option))))

           (define-a5211-subtype3-instruction (name)
             `(define-instruction ,name (segment cnd spec-reg mask
                                                 rn-or-immed12)
                (:emitter
                 (if (typep rn-or-immed12 tn)
                     (let ((msk (ash mask 2)))
                       (emit-a525-inst segment (conditional-opcode cnd)
                                       #b0001 #b0010 msk
                                       #b1111 #b0000 #b0000
                                       (reg-encode rn-or-immed12)))
                     (let ((fld (cond
                                  ((eq spec-reg :cpsr) #b00110010)
                                  ((eq spec-reg :spsr) #b00110110)
                                  (t (error "unknown spec-reg in a5211!")))))
                       (emit-a5211-inst segment (conditional-opcode cnd)
                                        fld
                                        mask
                                        #b1111
                                        (ror-immed12 rn-or-immed12)))))))
           )
  (define-a5211-subtype1-instruction nop   #b0000 #b0000)
  (define-a5211-subtype1-instruction yield #b0000 #b0001)
  (define-a5211-subtype1-instruction wfe   #b0000 #b0010)
  (define-a5211-subtype1-instruction wfi   #b0000 #b0011)
  (define-a5211-subtype1-instruction sev   #b0000 #b0100)
  (define-a5211-subtype2-instruction dbg   #b1111)
  (define-a5211-subtype3-instruction msr)
  )


(macrolet (
           (define-a5212-subtype1-instruction (name op1)
             `(define-instruction ,name (segment cnd rm)
                (:emitter
                 (emit-a525-inst segment (conditional-opcode cnd) #b0001 #b0010
                                 #b1111 #b1111 #b1111 ,op1 (reg-encode rm)))))
           )

  (define-a5212-subtype1-instruction bx  #b0001)
  (define-a5212-subtype1-instruction bxj #b0010)
  (define-a5212-subtype1-instruction blx #b0011)

  (define-instruction mrs (segment cnd rd)
    (:emitter
     (emit-a5211-inst segment (conditional-opcode cnd) #b00010000 #b1111
                      (reg-encode rd) #b000000000000)))

  (define-instruction clz (segment cnd rd rm)
    (:emitter
     (emit-a525-inst segment (conditional-opcode cnd) #b0001 #b0110 #b1111
                     (reg-encode rd) #b1111 #b0001 (reg-encode rm))))

  (define-instruction bkpt (segment immed4 &optional (immed12 0))
    (:emitter
     (emit-a5212-inst segment #b1110 #b00010010 immed12 #b0111 immed4)))

  (define-instruction smc (segment cnd immed4)
    (:emitter
     (emit-a5212-inst segment (conditional-opcode cnd) #b00010110
                      #b000000000000 #b0111 immed4)))
)


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