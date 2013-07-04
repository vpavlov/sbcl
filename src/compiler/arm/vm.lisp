;;;; miscellaneous VM definition noise for the ARM
;;;;
;;;; This is based on cmucl/src/compiler/rt/vm.lisp -- VNP 2013-07-03

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")


;;;; SB and SC definition:

(define-storage-base registers :finite :size 16)
(define-storage-base float-registers :finite :size 32)
(define-storage-base stack :unbounded :size 8)
;; These are constants in components.
(define-storage-base constant :non-packed)
;; Anything I can cookup out of nowhere and store somewhere.
(define-storage-base immediate-constant :non-packed)

;;;
;;; Handy macro so we don't have to keep changing all the numbers whenever
;;; we insert a new storage class.
;;; 
(defmacro define-storage-classes (&rest classes)
  (do ((forms (list 'progn)
	      (let* ((class (car classes))
		     (sc-name (car class))
		     (constant-name (intern (concatenate 'simple-string
							 (string sc-name)
							 "-SC-NUMBER"))))
		(list* `(define-storage-class ,sc-name ,index
			  ,@(cdr class))
		       `(def!constant ,constant-name ,index)
		       forms)))
       (index 0 (1+ index))
       (classes classes (cdr classes)))
      ((null classes)
       (nreverse forms))))

(def!constant kludge-nondeterministic-catch-block-size 6)

(define-storage-classes

  ;; Non-immediate contstants in the constant pool
  (constant constant)


  (immediate immediate-constant)
  (null immediate-constant)


  ;; The control stack.  (Scanned by GC)
  (control-stack stack)

  ;; The non-descriptor stack SC's.
  (signed-stack stack) ; (signed-byte 32)
  (unsigned-stack stack) ; (unsigned-byte 32)
  (character-stack stack) ; non-descriptor characters.
  (sap-stack stack) ; System area pointers.
  (single-stack stack) ; single-floats
  (double-stack stack :element-size 2) ; double floats.
  (complex-single-stack stack :element-size 2)
  (complex-double-stack stack :element-size 4)

  ;; **** Things that can go in the non-descriptor registers.

  ;; Immediate descriptor objects.  Don't have to be seen by GC, but nothing
  ;; bad will happen if they are.  (fixnums, characters, header values, etc).
  (any-reg registers
   :locations (0 1 2 3 4 5 10 11)
   :constant-scs (immediate)
   :reserve-locations (3 4 5)
   :save-p t
   :alternate-scs (control-stack))

  ;; Descriptor objects.  Must be seen by GC.
  (descriptor-reg registers
   :locations (0 1 2 10 11)
   ;; Immediate (and constant) for moving NULL around (at least).
   :constant-scs (constant immediate null)
   :save-p t
   :alternate-scs (control-stack))

  ;; Non-Descriptor characters.
  (character-reg registers
   :locations (3 4 5)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (character-stack))

  ;; Non-Descriptor SAP's (arbitrary pointers into address space).
  (sap-reg registers
   :locations (3 4)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (sap-stack))

  ;; Non-Descriptor (signed or unsigned) numbers.
  (signed-reg registers
   :locations (3 4 5)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (signed-stack))
  (unsigned-reg registers
   :locations (3 4 5)
   :constant-scs (immediate)
   :save-p t
   :alternate-scs (unsigned-stack))

  ;; Random objects that must not be seen by GC.  Used only as temporaries.
  (non-descriptor-reg registers
   :locations (3 4 5))

  ;; Word-aligned pointers that cannot be in R0.  Used for temporaries and to
  ;; hold stack pointers.
  (word-pointer-reg registers
   :locations (0 1 2 3 4 10 11)
   :save-p t
   :alternate-scs (control-stack))

  ;; Pointers to the interior of objects.  Used only as an temporary.
  (interior-reg registers
   :locations (12 14))


  ;; **** Things that can go in the floating point registers.

  ;; Non-Descriptor single-floats.
  (single-reg float-registers
   :locations (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
		 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)
   :constant-scs ()
   :save-p t
   :alternate-scs (single-stack))
  ;; Non-Descriptor double-floats.
  (double-reg float-registers
   :locations (0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
		 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31)
   :constant-scs ()
   :save-p t
   :alternate-scs (double-stack))

  (complex-single-reg float-registers
   :locations (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-single-stack))

  (complex-double-reg float-registers
   :locations (0 2 4 6 8 10 12 14 16 18 20 22 24 26 28 30)
   :element-size 2
   :constant-scs ()
   :save-p t
   :alternate-scs (complex-double-stack))

  ;; A catch or unwind block.
  (catch-block stack :element-size kludge-nondeterministic-catch-block-size))


;;;; Magical Registers

;;; Other VOP/VM definition files use the definitions on this page when writing
;;; interface code for the compiler.
;;;
;;; I got rid of 3 registers from the original (CMUCL RT) backend in order to
;;; be able to accomodate PC, LR (not in the list) and THREAD. These are:
;;;   - nsp, nfp: we'll go with not having a separate number stack
;;;   - lexenv: the PPC backend says 'and why blow a register for this?'
;;; -- VNP 2013-07-03

(eval-when (:compile-toplevel :load-toplevel :execute)
  (def!constant a0-offset     0)
  (def!constant a1-offset     1)
  (def!constant a2-offset     2)
  (def!constant ocfp-offset   3)
  (def!constant nl0-offset    4)
  (def!constant nargs-offset  5)
  (def!constant cfp-offset    6)
  (def!constant code-offset   7)
  (def!constant null-offset   8)
  #!+sb-thread
  (def!constant thread-offset 9)
  (def!constant cname-offset 10)
  (def!constant lra-offset   11)
  (def!constant lip-offset   12)
  (def!constant csp-offset   13)
  (def!constant lr-offset    14))

;;; Lisp-interior-pointer register.
;;;
(defparameter lip-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset lip-offset))

;;; Nil.
;;;
(defparameter null-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset null-offset))


;;; Frame Pointer.
;;;
(defparameter cfp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset cfp-offset))


;;; Control stack pointer.
;;;
(defparameter csp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset csp-offset))


;;; Code Pointer.
;;;
(defparameter code-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset code-offset))


;;; Random non-descriptor tn
;;;
(defparameter nl0-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'non-descriptor-reg)
		  :offset nl0-offset))

(defparameter nargs-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'any-reg)
		  :offset nargs-offset))

(defparameter ocfp-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset ocfp-offset))

(defparameter lra-tn
  (make-random-tn :kind :normal
		  :sc (sc-or-lose 'descriptor-reg)
		  :offset lra-offset))

#!+sb-thread
(defparameter thread-base-tn
  (make-random-tn :kind :normal :sc (sc-or-lose 'unsigned-reg)
                  :offset thread-offset))


;;;; Constants

;;; IMMEDIATE-CONSTANT-SC  --  Interface.
;;;
;;; If value can be represented as an immediate constant, then return the
;;; appropriate SC number, otherwise return NIL.
;;;
(!def-vm-support-routine immediate-constant-sc (value)
  (typecase value
    ((or fixnum character system-area-pointer)
     (sc-number-or-lose 'immediate))
    (null
     (sc-number-or-lose 'null))
    (symbol
     (if (static-symbol-p value)
	 (sc-number-or-lose 'immediate)
	 nil))))



;;;; Function Call Parameters

;;; The SC numbers for register and stack arguments/return values.
;;;
;;; Other VOP/VM definition files use this when writing interface code for the
;;; compiler.
;;;
(def!constant register-arg-scn (meta-sc-number-or-lose 'descriptor-reg))
(def!constant immediate-arg-scn (meta-sc-number-or-lose 'any-reg))
(def!constant control-stack-arg-scn (meta-sc-number-or-lose 'control-stack))

(eval-when (:compile-toplevel :load-toplevel :execute)
;;; Offsets of special stack frame locations.
;;;
  (def!constant ocfp-save-offset 0)
  (def!constant lra-save-offset 1)

;;; The number of arguments/return values passed in registers.
;;;
;;; Other VOP/VM definition files use this when writing interface code for the
;;; compiler.
;;;
  (def!constant register-arg-count 3)

;;; The offsets within the register-arg SC where we supply values, first value
;;; first.
;;;
;;; Other VOP/VM definition files use this when writing interface code for the
;;; compiler.
;;;
  (defparameter *register-arg-offsets* '(0 1 2))

;;; Names to use for the argument registers.
;;; 
  (defparameter register-arg-names '(a0 a1 a2)))

;;; A list of TN's describing the register arguments.
;;;
(defparameter *register-arg-tns*
  (mapcar #'(lambda (n)
	      (make-random-tn :kind :normal
			      :sc (sc-or-lose 'descriptor-reg)
			      :offset n))
	  *register-arg-offsets*))



;;;; LOCATION-PRINT-NAME.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *register-names* #("A0" "A1" "A2" "OCFP" "NL0" "NARGS" "CFP" "CODE"
			     "NULL" #!+sb-thread "THREAD" #!-sb-thread "R9"
			     "CNAME" "LRA" "LIP" "CSP")))

;;; LOCATION-PRINT-NAME  --  Interface.
;;;
;;; This function is called by debug output routines that want a pretty name
;;; for a TN's location.  It returns a thing that can be printed with PRINC.
;;;
(!def-vm-support-routine location-print-name (tn)
  (declare (type tn tn))
  (let ((sb (sb-name (sc-sb (tn-sc tn))))
	(offset (tn-offset tn)))
    (ecase sb
      (registers (svref *register-names* offset))
      (float-registers (format nil "F~D" offset))
      (stack (format nil "S~D" offset))
      (constant (format nil "Const~D" offset))
      (immediate-constant "Immed"))))
