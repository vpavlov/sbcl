;;;; miscellaneous VM definition noise for the ARMv6/VFPv2

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; the size of an INTEGER representation of a SYSTEM-AREA-POINTER, i.e.
;;; size of a native memory address
(deftype sap-int () '(unsigned-byte 32))

;;;; register specs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *core-register-names* (make-array 16 :initial-element nil))
  (defvar *vfpv2-register-names* (make-array 16 :initial-element nil)))

(macrolet ((defreg (name offset nmspace)
             (let ((offset-sym (symbolicate name "-OFFSET"))
                   (names-vector (symbolicate "*" nmspace "-REGISTER-NAMES*")))
               `(progn
                  (eval-when (:compile-toplevel :load-toplevel :execute)
                    ;; EVAL-WHEN is necessary because stuff like #.R0-OFFSET
                    ;; (in the same file) depends on compile-time evaluation
                    ;; of the DEFCONSTANT. -- AL 20010224
                    (def!constant ,offset-sym ,offset))
                  (setf (svref ,names-vector ,offset-sym)
                        ,(symbol-name name)))))
           (defregset (name &rest regs)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (defparameter ,name
                  (list ,@(mapcar #'(lambda (name)
                                      (symbolicate name "-OFFSET")) regs))))))
  ;; core registers
  (defreg a1 0 :core)
  (defreg a2 1 :core)
  (defreg a3 2 :core)
  (defreg a4 3 :core)
  (defreg v1 4 :core)
  (defreg v2 5 :core)
  (defreg v3 6 :core)
  (defreg v4 7 :core)
  (defreg v5 8 :core)
  (defreg thread 9 :core)
  (defreg v7 10 :core)
  (defreg v8 11 :core)
  (defreg ip 12 :core)
  (defreg sp 13 :core)
  (defreg lr 14 :core)
  (defreg pc 15 :core)
  (defregset *core-regs* a1 a2 a3 a4 v1 v2 v3 v4 v5 thread v7 v8 ip sp lr pc)

  ;; VFPv2, 16 doubleword registers view
  ;; TODO: figure out how to encode the other view, 32 singleword registers
  ;;       for now we use the double-word registers as single-word
  (defreg d0 0 :vfpv2)
  (defreg d1 1 :vfpv2)
  (defreg d2 2 :vfpv2)
  (defreg d3 3 :vfpv2)
  (defreg d4 4 :vfpv2)
  (defreg d5 5 :vfpv2)
  (defreg d6 6 :vfpv2)
  (defreg d7 7 :vfpv2)
  (defreg d8 8 :vfpv2)
  (defreg d9 9 :vfpv2)
  (defreg d10 10 :vfpv2)
  (defreg d11 11 :vfpv2)
  (defreg d12 12 :vfpv2)
  (defreg d13 13 :vfpv2)
  (defreg d14 14 :vfpv2)
  (defreg d15 15 :vfpv2)
  (defregset *vfpv2-regs* d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11 d12 d13 d14 d15)

  ;; registers used to pass arguments
  ;;
  ;; the number of arguments/return values passed in registers
  (def!constant register-arg-count 4)
  ;; names and offsets for registers used to pass arguments
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defparameter *register-arg-names* '(a1 a2 a3 a4)))
  (defregset *register-arg-offsets*  a1 a2 a3 a4))

;;;; SB definitions

(define-storage-base registers :finite :size 16)
(define-storage-base vfpv2-registers :finite :size 16)
(define-storage-base stack :unbounded :size 8)
(define-storage-base constant :non-packed)
(define-storage-base immediate-constant :non-packed)
(define-storage-base noise :unbounded :size 2)  ;; WTF ?

;;;; SC definitions

;;; a handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class
;;;
(defmacro !define-storage-classes (&rest classes)
  (collect ((forms))
    (let ((index 0))
      (dolist (class classes)
        (let* ((sc-name (car class))
               (constant-name (symbolicate sc-name "-SC-NUMBER")))
          (forms `(define-storage-class ,sc-name ,index
                    ,@(cdr class)))
          (forms `(def!constant ,constant-name ,index))
          (incf index))))
    `(progn
       ,@(forms))))

;;; The DEFINE-STORAGE-CLASS call for CATCH-BLOCK refers to the size
;;; of CATCH-BLOCK. The size of CATCH-BLOCK isn't calculated until
;;; later in the build process, and the calculation is entangled with
;;; code which has lots of predependencies, including dependencies on
;;; the prior call of DEFINE-STORAGE-CLASS. The proper way to
;;; unscramble this would be to untangle the code, so that the code
;;; which calculates the size of CATCH-BLOCK can be separated from the
;;; other lots-of-dependencies code, so that the code which calculates
;;; the size of CATCH-BLOCK can be executed early, so that this value
;;; is known properly at this point in compilation. However, that
;;; would be a lot of editing of code that I (WHN 19990131) can't test
;;; until the project is complete. So instead, I set the correct value
;;; by hand here (a sort of nondeterministic guess of the right
;;; answer:-) and add an assertion later, after the value is
;;; calculated, that the original guess was correct.
;;;
;;; (What a KLUDGE! Anyone who wants to come in and clean up this mess
;;; has my gratitude.) (FIXME: Maybe this should be me..)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (def!constant kludge-nondeterministic-catch-block-size 6))

(!define-storage-classes

  ;; non-immediate constants in the constant pool
  (constant constant)

  ;; some FP constants can be generated in the i387 sillicon
  ;; so, what about the ARM VFPv2 silicon? We'll see...
  ;; --vnp 2013-04-25
  (fp-constant immediate-constant)
  (fp-single-immediate immediate-constant)
  (fp-double-immediate immediate-constant)
  (immediate immediate-constant)

  ;;
  ;; the stacks
  ;;

  ;; the control stack
  (control-stack stack)                 ; may be pointers, scanned by GC

  ;; the non-descriptor stacks
  (signed-stack stack)                  ; (signed-byte 32)
  (unsigned-stack stack)                ; (unsigned-byte 32)
  (character-stack stack)               ; non-descriptor characters.
  (sap-stack stack)                     ; System area pointers.
  (single-stack stack)                  ; single-floats
  (double-stack stack :element-size 2)  ; double-floats.
  (complex-single-stack stack :element-size 2)  ; complex-single-floats
  (complex-double-stack stack :element-size 4)  ; complex-double-floats

  ;;
  ;; magic SCs
  ;;

  (ignore-me noise)

  ;;
  ;; things that can go in the integer registers
  ;;

  ;; On the ARM, we don't have to distinguish between descriptor and
  ;; non-descriptor registers, because of the conservative GC.
  ;; Therefore, we use different scs only to distinguish between
  ;; descriptor and non-descriptor values and to specify size.

  ;; immediate descriptor objects. Don't have to be seen by GC, but nothing
  ;; bad will happen if they are. (fixnums, characters, header values, etc).
  (any-reg registers
           :locations #.*core-regs*
           :constant-scs (immediate)
           :save-p t
           :alternate-scs (control-stack))

  ;; pointer descriptor objects -- must be seen by GC
  (descriptor-reg registers
                  :locations #.*core-regs*
                  :constant-scs (constant immediate)
                  :save-p t
                  :alternate-scs (control-stack))

  ;; non-descriptor characters
  (character-reg registers
                 :locations #.*core-regs*
                 :constant-scs (immediate)
                 :save-p t
                 :alternate-scs (character-stack))

  ;; non-descriptor SAPs (arbitrary pointers into address space)
  (sap-reg registers
           :locations #.*core-regs*
           :constant-scs (immediate)
           :save-p t
           :alternate-scs (sap-stack))

  ;; non-descriptor (signed or unsigned) numbers
  (signed-reg registers
              :locations #.*core-regs*
              :constant-scs (immediate)
              :save-p t
              :alternate-scs (signed-stack))

  (unsigned-reg registers
                :locations #.*core-regs*
                :constant-scs (immediate)
                :save-p t
                :alternate-scs (unsigned-stack))

  ;; non-descriptor SINGLE-FLOATs
  (single-reg vfpv2-registers
              :locations (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
              :constant-scs (fp-constant fp-single-immediate)
              :save-p t
              :alternate-scs (single-stack))

  ;; non-descriptor DOUBLE-FLOATs
  (double-reg vfpv2-registers
              :locations (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
              :constant-scs (fp-constant fp-double-immediate)
              :save-p t
              :alternate-scs (double-stack))

  (complex-single-reg vfpv2-registers
                      :locations (0 2 4 6 8 10 12 14)
                      :element-size 2
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-single-stack))

  (complex-double-reg vfpv2-registers
                      :locations (0 2 4 6 8 10 12 14)
                      :element-size 2
                      :constant-scs ()
                      :save-p t
                      :alternate-scs (complex-double-stack))

  ;; a catch or unwind block
  (catch-block stack :element-size kludge-nondeterministic-catch-block-size))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *core-sc-names*
    '(any-reg descriptor-reg sap-reg signed-reg unsigned-reg control-stack
      signed-stack unsigned-stack sap-stack single-stack
      character-reg character-stack constant))
;;; added by jrd. I guess the right thing to do is to treat floats
;;; as a separate size...
;;;
;;; These are used to (at least) determine operand size.
  (defparameter *float-sc-names* '(single-reg))
  (defparameter *double-sc-names* '(double-reg double-stack))
  ) ; EVAL-WHEN


;;;; miscellaneous TNs for the various registers

(macrolet ((def-misc-reg-tns (sc-name &rest reg-names)
             (collect ((forms))
               (dolist (reg-name reg-names)
                 (let ((tn-name (symbolicate reg-name "-TN"))
                       (offset-name (symbolicate reg-name "-OFFSET")))
                   ;; FIXME: It'd be good to have the special
                   ;; variables here be named with the *FOO*
                   ;; convention.
                   (forms `(defparameter ,tn-name
                             (make-random-tn :kind :normal
                                             :sc (sc-or-lose ',sc-name)
                                             :offset ,offset-name)))))
               `(progn ,@(forms)))))
  (def-misc-reg-tns unsigned-reg a1 a2 a3 a4 v1 v2 v3 v4 v5 v7 v8)
  (def-misc-reg-tns single-reg d0 d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 d11
		    d12 d13 d14 d15))

;;; TNs for registers used to pass arguments
(defparameter *register-arg-tns*
  (mapcar (lambda (register-arg-name)
            (symbol-value (symbolicate register-arg-name "-TN")))
          *register-arg-names*))

;;; If value can be represented as an immediate constant, then return
;;; the appropriate SC number, otherwise return NIL.
(!def-vm-support-routine
 immediate-constant-sc (value)
 (typecase value
   ((or (integer #.sb!xc:most-negative-fixnum #.sb!xc:most-positive-fixnum)
        character)
    (sc-number-or-lose 'immediate))
   (symbol
    (when (static-symbol-p value)
      (sc-number-or-lose 'immediate)))
   (single-float
    (case value
      ((0f0 1f0) (sc-number-or-lose 'fp-constant))
      (t (sc-number-or-lose 'fp-single-immediate))))
   (double-float
    (case value
      ((0d0 1d0) (sc-number-or-lose 'fp-constant))
      (t (sc-number-or-lose 'fp-double-immediate))))))

(!def-vm-support-routine
 boxed-immediate-sc-p (sc)
 (eql sc (sc-number-or-lose 'immediate)))

;; For an immediate TN, return its value encoded for use as a literal.
;; For any other TN, return the TN.  Only works for FIXNUMs,
;; STATIC-SYMBOLs, and CHARACTERS (FLOATs and SAPs are handled
;; elsewhere).
(defun encode-value-if-immediate (tn)
  (if (sc-is tn immediate)
      (let ((val (tn-value tn)))
        (etypecase val
          (integer (fixnumize val))
          (symbol (+ nil-value (static-symbol-offset val)))
          (character (logior (ash (char-code val) n-widetag-bits)
                             character-widetag))))
      tn))

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location. It returns a thing that can be printed with PRINC.
(!def-vm-support-routine
 location-print-name (tn)
 (declare (type tn tn))
 (let* ((sc (tn-sc tn))
        (sb (sb-name (sc-sb sc)))
        (offset (tn-offset tn)))
   (ecase sb
     (registers
      (let* ((sc-name (sc-name sc)))
        (or (and (< -1 offset (length *core-register-names*))
                 (svref *core-register-names* offset))
            ;; FIXME: Shouldn't this be an ERROR?
            (format nil "<unknown reg: off=~W, sc=~A>" offset sc-name))))
     (vfpv2-registers (format nil "D~D" offset))
     (stack (format nil "S~D" offset))
     (constant (format nil "Const~D" offset))
     (immediate-constant "Immed")
     (noise (symbol-name (sc-name sc))))))
;;; FIXME: Could this, and everything that uses it, be made #!+SB-SHOW?

(!def-vm-support-routine
 combination-implementation-style (node)
 (declare (type sb!c::combination node))
 (flet ((valid-funtype (args result)
          (sb!c::valid-fun-use node
                               (sb!c::specifier-type
                                `(function ,args ,result)))))
   (case (sb!c::combination-fun-source-name node)
     (logtest
      (cond
        ((valid-funtype '(fixnum fixnum) '*)
         (values :direct nil))
        ((valid-funtype '((signed-byte 32) (signed-byte 32)) '*)
         (values :direct nil))
        ((valid-funtype '((unsigned-byte 32) (unsigned-byte 32)) '*)
         (values :direct nil))
        (t (values :default nil))))
     (logbitp
      (cond
        ((and (valid-funtype '((integer 0 29) fixnum) '*)
              (sb!c::constant-lvar-p
               (first (sb!c::basic-combination-args node))))
         (values :transform '(lambda (index integer)
                              (%logbitp integer index))))
        ((valid-funtype '((integer 0 31) (signed-byte 32)) '*)
         (values :transform '(lambda (index integer)
                              (%logbitp integer index))))
        ((valid-funtype '((integer 0 31) (unsigned-byte 32)) '*)
         (values :transform '(lambda (index integer)
                              (%logbitp integer index))))
        (t (values :default nil))))
     (t (values :default nil)))))
