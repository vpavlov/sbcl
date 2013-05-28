;;;; miscellaneous VM definition noise for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; register specs

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* (make-array 16 :initial-element nil)))

(macrolet ((defreg (name offset)
             (let ((offset-sym (symbolicate name "-OFFSET")))
               `(eval-when (:compile-toplevel :load-toplevel :execute)
                  (def!constant ,offset-sym ,offset)
                  (setf (svref *register-names* ,offset-sym)
                        ,(symbol-name name)))))
           (defregset (name &rest regs)
             `(eval-when (:compile-toplevel :load-toplevel :execute)
                (defparameter ,name
                  (list ,@(mapcar #'(lambda (name)
                                      (symbolicate name "-OFFSET")) regs))))))
  (defreg a1 0)
  (defreg a2 1)
  (defreg a3 2)
  (defreg a4 3)
  (defreg v1 4)
  (defreg v2 5)
  (defreg v3 6)
  (defreg v4 7)
  (defreg v5 8)
  (defreg thread 9)
  (defreg v7 10)
  (defreg null 11)
  (defreg lip 12)
  (defreg sp 13)
  (defreg lr 14)
  (defreg pc 15)
  (defregset descriptor-regs a1 a2 a3 a4 v1 v2 v3 v4 v5 v7)

  (defregset *register-arg-offsets* a1 a2 a3 a4)
  (defparameter register-arg-names '(a1 a2 a3 a4)))

;;;; SB definitions

(define-storage-base registers :finite :size 16)
(define-storage-base float-registers :finite :size 32)
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

  ;; NIL is in register
  (null immediate-constant)

  ;; Anything else that can be an immediate.
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
  (double-stack stack
                :element-size 2 :alignment 2)  ; double-floats.
  (complex-single-stack stack :element-size 2)  ; complex-single-floats
  (complex-double-stack stack
                        :element-size 4 :alignment 2)  ; complex-double-floats

  ;;
  ;; things that can go in the integer registers
  ;;

  ;; On the ARM, we don't have to distinguish between descriptor and
  ;; non-descriptor registers, because of the conservative GC.
  ;; Therefore, we use different scs only to distinguish between
  ;; descriptor and non-descriptor values and to specify size.

  ;; immediate descriptor objects. Don't have to be seen by GC, but nothing
  ;; bad will happen if they are. (fixnums, characters, header values, etc).
  (any-reg
   registers
   :locations #.descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; pointer descriptor objects -- must be seen by GC
  (descriptor-reg
   registers
   :locations #.descriptor-regs
   :constant-scs (constant null immediate)
   :save-p t
   :alternate-scs (control-stack))

  ;; non-descriptor characters
  (character-reg
   registers
   :locations #.descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (character-stack))

  ;; non-descriptor SAPs (arbitrary pointers into address space)
  (sap-reg
   registers
   :locations #.descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (sap-stack))

  ;; non-descriptor (signed or unsigned) numbers
  (signed-reg
   registers
   :locations #.descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (signed-stack))

  (unsigned-reg
   registers
   :locations #.descriptor-regs
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Pointers to the interior of objects.  Used only as a temporary.
  (interior-reg registers
   :locations (#.lip-offset))

  ;; non-descriptor SINGLE-FLOATs
  (single-reg
   float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   ;; ### Note: We really should have every location listed, but then we
   ;; would have to make load-tns work with element-sizes other than 1.
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))

  ;; non-descriptor DOUBLE-FLOATs
  (double-reg
   float-registers
   :locations #.(loop for i from 0 to 31 collect i)
   ;; ### Note: load-tns don't work with an element-size other than 1.
   ;; :element-size 2 :alignment 2
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  (complex-single-reg
   float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg
   float-registers
   :locations #.(loop for i from 0 to 30 by 2 collect i)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-stack))

  ;; a catch or unwind block
  (catch-block stack :element-size kludge-nondeterministic-catch-block-size))

;;;; Make some random tns for important registers.

(macrolet ((defregtn (name sc)
               (let ((offset-sym (symbolicate name "-OFFSET"))
                     (tn-sym (symbolicate name "-TN")))
                 `(defparameter ,tn-sym
                   (make-random-tn :kind :normal
                    :sc (sc-or-lose ',sc)
                    :offset ,offset-sym)))))
  (defregtn lip interior-reg)
  (defregtn null descriptor-reg)
  (defregtn sp any-reg))

;;; If value can be represented as an immediate constant, then return
;;; the appropriate SC number, otherwise return NIL.
(!def-vm-support-routine
 immediate-constant-sc (value)
 (typecase value
   (null
    (sc-number-or-lose 'null))
   ((or (integer #.sb!xc:most-negative-fixnum #.sb!xc:most-positive-fixnum)
        character)
    (sc-number-or-lose 'immediate))
   (symbol
    (when (static-symbol-p value)
      (sc-number-or-lose 'immediate)))))

(!def-vm-support-routine
 boxed-immediate-sc-p (sc)
 (or (eql sc (sc-number-or-lose 'null))
     (eql sc (sc-number-or-lose 'immediate))))

;;;; function call parameters

;;; the SC numbers for register and stack arguments/return values
(def!constant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(def!constant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(def!constant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))

(eval-when (:compile-toplevel :load-toplevel :execute)

;;; offsets of special stack frame locations
  (def!constant ocfp-save-offset 0)
  (def!constant lra-save-offset 1)
  (def!constant nfp-save-offset 2)

;;; the number of arguments/return values passed in registers
  (def!constant register-arg-count 4)

;;; names to use for the argument registers
) ; EVAL-WHEN


;;; A list of TN's describing the register arguments.
;;;
(defparameter *register-arg-tns*
  (mapcar #'(lambda (n)
              (make-random-tn :kind :normal
                              :sc (sc-or-lose 'descriptor-reg)
                              :offset n))
          *register-arg-offsets*))

#!+sb-thread
(defparameter thread-base-tn
  (make-random-tn :kind :normal :sc (sc-or-lose 'unsigned-reg)
                  :offset thread-offset))

(export 'single-value-return-byte-offset)

;;; This is used by the debugger.
(def!constant single-value-return-byte-offset 8)

;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location. It returns a thing that can be printed with PRINC.
(!def-vm-support-routine
 location-print-name (tn)
 (declare (type tn tn))
 (let ((sb (sb-name (sc-sb (tn-sc tn))))
       (offset (tn-offset tn)))
   (ecase sb
     (registers (or (svref *register-names* offset)
                    (format nil "R~D" offset)))
     (float-registers (format nil "F~D" offset))
     (stack (format nil "S~D" offset))
     (constant (format nil "Const~D" offset))
     (immediate-constant "Immed"))))

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
