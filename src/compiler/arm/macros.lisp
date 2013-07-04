;;;; This file contains various useful macros for generating code for ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")



;;; Instruction-like macros.

;;; MOVE -- Interface.
;;;
(defmacro move (dst src)
  "Move SRC into DST unless they are LOCATION=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

;;; LOADW and STOREW -- Interface.
;;;
;;; Load and store words.
;;;
(macrolet ((def-mem-op (op inst)
             `(defmacro ,op (object base &optional (offset 0) (lowtag 0))
                `(inst ,',inst ,object ,base
		       (- (ash ,offset word-shift) ,lowtag)))))
  (def-mem-op loadw ldr)
  (def-mem-op storew str))

;;; LOAD-SYMBOL -- Interface.
;;;
(defmacro load-symbol (reg symbol)
  "Load a pointer to the static symbol into reg."
  `(inst add ,reg null-tn :src2 (static-symbol-offset ,symbol)))

;;; LOAD-SYMBOL-FUNCTION, STORE-SYMBOL-FUNCTION,
;;; LOAD-SYMBOL-VALUE, STORE-SYMBOL-VALUE		-- interface.
;;;
(macrolet
    ((frob (slot)
       (let ((loader (intern (concatenate 'simple-string
                                          "LOAD-SYMBOL-"
                                          (string slot))))
             (storer (intern (concatenate 'simple-string
                                          "STORE-SYMBOL-"
                                          (string slot))))
             (offset (intern (concatenate 'simple-string
                                          "SYMBOL-"
                                          (string slot)
                                          "-SLOT")
                             (find-package "SB!VM"))))
         `(progn
            (defmacro ,loader (reg symbol)
              `(inst ldr ,reg null-tn
                     (+ (static-symbol-offset ',symbol)
                        (ash ,',offset word-shift)
                        (- other-pointer-lowtag))))
            (defmacro ,storer (reg symbol)
              `(inst str ,reg null-tn
                     (+ (static-symbol-offset ',symbol)
                        (ash ,',offset word-shift)
                        (- other-pointer-lowtag))))))))
  (frob value)
  (frob function))

;; FIXME: These are only good for static-symbols, so why not
;; statically-allocate the static-symbol TLS slot indices at
;; cross-compile time so we can just use a fixed offset within the
;; TLS block instead of mucking about with the extra memory access
;; (and temp register, for stores)?
(defmacro load-tl-symbol-value (reg symbol)
  #!+sb-thread
  `(progn
     (inst ldr ,reg null-tn
	   (+ (static-symbol-offset ',symbol)
	      (ash symbol-tls-index-slot word-shift)
	      (- other-pointer-lowtag)))
     (inst ldr ,reg thread-base-tn ,reg))
  #!-sb-thread
  `(load-symbol-value ,reg ,symbol))

(defmacro store-tl-symbol-value (reg symbol temp-tn)
  #!+sb-thread
  `(progn
     (inst ldr ,temp-tn null-tn
	   (+ (static-symbol-offset ',symbol)
	      (ash symbol-tls-index-slot word-shift)
	      (- other-pointer-lowtag)))
     (inst str ,reg thread-base-tn ,temp-tn))
  #!-sb-thread
  `(store-symbol-value ,reg ,symbol))

;;; LOAD-TYPE -- Interface.
;;;
(defmacro load-type (target source &optional (-lowtag 0))
  "Loads the type bits from the source pointer into target, where pointer has
   the specified (- -LOWTAG)."
  (ecase *backend-byte-order*
    (:little-endian
     `(inst ldrb ,target ,source ,-lowtag))
    (:big-endian
     `(inst ldrb ,target ,source (+ ,-lowtag n-word-bytes -1)))))



;;;; Call and Return.

;;; Macros to handle the fact that we cannot use the machine native call and
;;; return instructions due to low-tag bits on pointers.
;;;

;;; LISP-JUMP -- Interface.
;;;
(defmacro lisp-jump (function lip)
  "Jump to the lisp function.  Lip is the lisp interior pointer register
   used as a temporary."
  `(progn
     (inst add ,lip ,function
	   :src2 (- (* n-word-bytes simple-fun-code-offset) fun-pointer-lowtag))
     (inst bx ,lip)))

;;; LISP-RETURN -- Interface.
;;;
(defmacro lisp-return (return-pc lip &key (offset 0))
  "Return to return-pc, an LRA.  Lip is the lisp interior pointer register
   used as a temporary.  Offset is the number of words to skip at the target,
   and it has to be small enough for whatever instruction used in here."
  `(progn
     (inst add ,lip ,return-pc
	   :src2 (- (* (1+ ,offset) n-word-bytes) other-pointer-lowtag))
     (inst bx ,lip)))

;;; EMIT-RETURN-PC -- Interface.
;;;
(defmacro emit-return-pc (label)
  "Emit a return-pc header word, making sure it is aligned properly for our
   low-tag bits since there are other-pointers referring to the LRA's.  label
   is the label to use for this return-pc."
  `(progn
     (emit-alignment n-lowtag-bits)
     (emit-label ,label)
     (inst lra-header-word)))



;;;; Stack TN's.

;;; LOAD-STACK-TN, STORE-STACK-TN  --  Interface.
;;;
;;; Move a stack TN to a register and vice-versa.
;;;
(defmacro load-stack-tn (reg stack)
  (once-only ((n-reg reg)
	      (n-stack stack))
    `(sc-case ,n-stack
       (control-stack
	(loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))
;;;
(defmacro store-stack-tn (stack reg)
  (once-only ((n-reg reg)
	      (n-stack stack))
    `(sc-case ,n-stack
       (control-stack
	(storew ,n-reg cfp-tn (tn-offset ,n-stack))))))


;;; MAYBE-LOAD-STACK-TN  --  Interface.
;;;
(defmacro maybe-load-stack-tn (reg reg-or-stack)
  "Move the TN Reg-Or-Stack into Reg if it isn't already there."
  (once-only ((n-reg reg)
              (n-stack reg-or-stack))
    `(sc-case ,n-reg
       ((any-reg descriptor-reg non-descriptor-reg word-pointer-reg)
        (sc-case ,n-stack
          ((any-reg descriptor-reg non-descriptor-reg word-pointer-reg)
           (move ,n-reg ,n-stack))
          ((control-stack)
           (loadw ,n-reg cfp-tn (tn-offset ,n-stack))))))))

;;;; allocation helpers

;;; Allocation within alloc_region (which is thread local) can be done
;;; inline.  If the alloc_region is overflown allocation is done by
;;; calling the C alloc() function.

;;; C calls for allocation don't /seem/ to make an awful lot of
;;; difference to speed. On pure consing it's about a 25%
;;; gain. Guessing from historical context, it looks like inline
;;; allocation was introduced before pseudo-atomic, at which time all
;;; calls to alloc() would have needed a syscall to mask signals for
;;; the duration.  Now we have pseudoatomic there's no need for that
;;; overhead.

(defun allocation-dynamic-extent (alloc-tn size lowtag)
  (inst sub csp-tn size)
  ;; FIXME: SIZE _should_ be double-word aligned (suggested but
  ;; unfortunately not enforced by PAD-DATA-BLOCK and
  ;; WITH-FIXED-ALLOCATION), so that SP is always divisible by 8 (for
  ;; 32-bit lispobjs).  In that case, this AND instruction is
  ;; unneccessary and could be removed.  If not, explain why.  -- CSR,
  ;; 2004-03-30
  ;;
  ;; Since the above seems correct, when we comment the instruction below and
  ;; something breaks, we known its a bug and we fix it. So, I'll remove it
  ;; from the ARM port at least.
  ;; -- VNP, 2013-06-02
  ;;(inst and sp-tn (lognot lowtag-mask))
  (aver (not (location= alloc-tn csp-tn)))
  (inst add alloc-tn csp-tn :src2 lowtag)
  (values))

(defun allocation-inline (alloc-tn size temp-tn temp2-tn)
  (let* ((ok (gen-label))
	 (done (gen-label)))

    ;; temp-tn = size
    (cond ((numberp size) (inst lr temp-tn size))
	  (t (move temp-tn size)))

    ;; alloc-tn = free pointer in the allocation region
    #!+sb-thread (inst ldr alloc-tn thread-base-tn
		       (* thread-alloc-region-slot n-word-bytes))
    #!-sb-thread (inst lr temp2-tn (make-fixup "boxed_region" :foreign))
    #!-sb-thread (inst ldr alloc-tn temp2-tn 0)

    ;; temp2-tn = end of the allocation region
    #!+sb-thread (inst ldr temp2-tn thread-base-tn
		       (* (1+ thread-alloc-region-slot) n-word-bytes))
    #!-sb-thread (inst ldr temp2-tn temp2-tn 4)

    (inst add alloc-tn temp-tn)
    (inst cmp alloc-tn temp2-tn)
    (inst b ok :cnd :ls)

    ;; free space is not enough; extend the allocation region
    (inst lr temp-tn (make-fixup "alloc_overflow" :foreign))
    (inst blx temp-tn)
    (inst b done)

    ;; inline allocation will succeed. Swap ALLOC-TN and free pointer
    (emit-label ok)
    #!+sb-thread (inst str alloc-tn thread-base-tn
		       (* thread-alloc-region-slot n-word-bytes))
    #!-sb-thread (inst lr temp2-tn (make-fixup "boxed_region" :foreign))
    #!-sb-thread (inst str alloc-tn temp2-tn)
    (inst sub alloc-tn temp-tn)

    (emit-label done)
    (values)))

;;; This is the main mechanism for allocating memory in the lisp heap.
;;; Emit code to allocate an object with a size in bytes given by SIZE. The
;;; size may be an integer or a TN.

;;; Allocation should only be used inside a pseudo-atomic section, which
;;; should also cover subsequent initialization of the object.

;;; (FIXME: so why aren't we asserting this?)
(defun allocation (alloc-tn size &key stack-p lowtag temp-tn temp2-tn)
  (if stack-p
      (allocation-dynamic-extent alloc-tn size lowtag)
      (progn
	(allocation-inline alloc-tn size temp-tn temp2-tn)
	(inst add alloc-tn alloc-tn :src2 lowtag)))
  (values))

(defmacro with-fixed-allocation ((result-tn temp-tn temp2-tn widetag size
					    &key (lowtag other-pointer-lowtag)
					    stack-allocate-p)
                                 &body forms)
  "Do stuff to allocate an object (having the specified LOWTAG) of fixed SIZE
with a single word header having the specified WIDETAG value. The result is
placed in RESULT-TN, and TEMP-TN/2 are non-descriptor temps (which may be
randomly used by the body). The body is placed inside the PSEUDO-ATOMIC,
and presumably initializes the object."
  (once-only ((result-tn result-tn)
	      (temp-tn temp-tn)
	      (temp2-tn temp2-tn)
	      (size size)
	      (stack-allocate-p stack-allocate-p)
	      (widetag widetag)
	      (lowtag lowtag))
	     `(maybe-pseudo-atomic
	       ,stack-allocate-p ,temp-tn
	       (allocation ,result-tn (pad-data-block ,size)
			   :stack-p ,stack-allocate-p
			   :lowtag ,lowtag
			   :temp-tn ,temp-tn
			   :temp2-tn ,temp2-tn)
	       (inst lr ,temp-tn (logior (ash (1- ,size) n-widetag-bits)
					 ,widetag))
	       (storew ,temp-tn ,result-tn 0 ,lowtag)
	       ,@forms)))

;;;; error code
(defun emit-error-break (vop kind code values)
  (assemble ()
	    (inst bkpt kind)
	    ;; The return PC points here; note the location for the debugger.
	    (when vop
	      (note-this-location vop :internal-error))
	    (with-adjustable-vector (vector)    ; interr arguments
	      (write-var-integer code vector)
	      (dolist (tn values)
		;; classic CMU CL comment:
		;;   zzzzz jrd here. tn-offset is zero for constant
		;;   tns.
		(write-var-integer (make-sc-offset (sc-number (tn-sc tn))
						   (or (tn-offset tn) 0))
				   vector))
	      (inst byte (length vector))
	      (dotimes (i (length vector))
		(inst byte (aref vector i))))))

(defun error-call (vop error-code &rest values)
  #!+sb-doc
  "Cause an error. ERROR-CODE is the error to cause."
  (emit-error-break vop error-trap (error-number-or-lose error-code) values))

(defun generate-error-code (vop error-code &rest values)
  #!+sb-doc
  "Generate-Error-Code Error-code Value*
  Emit code for an error with the specified Error-Code and context Values."
  (assemble (*elsewhere*)
	    (let ((start-lab (gen-label)))
	      (emit-label start-lab)
	      (emit-error-break vop error-trap
				(error-number-or-lose error-code) values)
	      start-lab)))



;;;; PSEUDO-ATOMIC

;;; This is used to wrap operations which leave untagged memory lying
;;; around.  It's an operation which the AOP weenies would describe as
;;; having "cross-cutting concerns", meaning it appears all over the
;;; place and there's no logical single place to attach documentation.
;;; grep (mostly in src/runtime) is your friend

;;; KLUDGE: since the stack on the ARM is treated conservatively, it
;;; does not matter whether a signal occurs during construction of a
;;; dynamic-extent object, as the half-finished construction of the
;;; object will not cause any difficulty.  We can therefore elide
(defmacro maybe-pseudo-atomic (not-really-p temp-tn &body forms)
  `(if ,not-really-p
       (progn ,@forms)
       (pseudo-atomic ,temp-tn ,@forms)))

(defmacro pseudo-atomic (temp-tn &body forms)
  (with-unique-names (label)
    `(let ((,label (gen-label)))
       ;; Note: we use CFP-TN as some random non-zero value
       #!+sb-thread
       (inst str cfp-tn thread-base-tn (* thread-pseudo-atomic-bits-slot
					  n-word-bytes))
       #!-sb-thread
       (store-symbol-value cfp-tn *pseudo-atomic-bits*)

       ,@forms
       
       #!+sb-thread
       (inst ldr ,temp-tn thread-base-tn (* thread-pseudo-atomic-bits-slot
					    n-word-bytes))
       #!-sb-thread
       (load-symbol-value ,temp-tn *pseudo-atomic-bits*)

       (inst xors ,temp-tn cfp-tn)
       (inst b ,label :cnd :eq)
       (inst bkpt pending-interrupt-trap)
       (emit-label ,label))))
