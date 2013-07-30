;;;; floating point support for the ARM

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;;; Move functions:

;;; X is source, Y is destination.
(define-move-fun (load-single 1) (vop x y)
  ((single-stack) (single-reg))
  (inst vldr.32 y cfp-tn (frame-byte-offset (tn-offset x))))

(define-move-fun (store-single 1) (vop x y)
  ((single-reg) (single-stack))
  (inst vstr.32 x cfp-tn (frame-byte-offset (tn-offset y))))

(define-move-fun (load-double 2) (vop x y)
  ((double-stack) (double-reg))
  (inst vldr.64 y cfp-tn (frame-byte-offset (+ (tn-offset x) 1))))

(define-move-fun (store-double 2) (vop x y)
  ((double-reg) (double-stack))
  (inst vstr.64 x cfp-tn (frame-byte-offset (+ (tn-offset y) 1))))



;;;; Move VOPs:

(macrolet ((frob (vop sc inst)
             `(progn
                (define-vop (,vop)
                  (:args (x :scs (,sc)
                            :target y
                            :load-if (not (location= x y))))
                  (:results (y :scs (,sc)
                               :load-if (not (location= x y))))
                  (:note "float move")
                  (:generator 0
			      (unless (location= y x)
				(inst ,inst y x))))
                (define-move-vop ,vop :move (,sc) (,sc)))))
  (frob single-move single-reg vmov.f32)
  (frob double-move double-reg vmov.f64))

(define-vop (move-from-float)
  (:args (x :to :save))
  (:results (y))
  (:note "float to pointer coercion")
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) temp2)
  (:variant-vars double-p size type data)
  (:generator 13
    (with-fixed-allocation (y temp temp2 type size)
      (if double-p
          (inst vstr.64 x y (- (* data n-word-bytes) other-pointer-lowtag))
          (inst vstr.32 x y (- (* data n-word-bytes) other-pointer-lowtag))))))

(macrolet ((frob (name sc &rest args)
             `(progn
                (define-vop (,name move-from-float)
                  (:args (x :scs (,sc) :to :save))
                  (:results (y :scs (descriptor-reg)))
                  (:variant ,@args))
                (define-move-vop ,name :move (,sc) (descriptor-reg)))))
  (frob move-from-single single-reg
	nil single-float-size single-float-widetag single-float-value-slot)
  (frob move-from-double double-reg
	t double-float-size double-float-widetag double-float-value-slot))

(macrolet ((frob (name sc double-p value)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (descriptor-reg)))
                  (:results (y :scs (,sc)))
                  (:note "pointer to float coercion")
                  (:generator 2
			      (inst ,(if double-p 'vldr.64 'vldr.32) y x
				    (- (* ,value n-word-bytes)
				       other-pointer-lowtag))))
                (define-move-vop ,name :move (descriptor-reg) (,sc)))))
  (frob move-to-single single-reg nil single-float-value-slot)
  (frob move-to-double double-reg t double-float-value-slot))

(macrolet ((frob (name sc stack-sc double-p)
             `(progn
                (define-vop (,name)
                  (:args (x :scs (,sc) :target y)
                         (nfp :scs (any-reg)
                              :load-if (not (sc-is y ,sc))))
                  (:results (y))
                  (:note "float arg move")
                  (:generator ,(if double-p 2 1)
			      (sc-case y
				(,sc
				 (unless (location= x y)
				   (inst ,(if double-p 'vmov.f64 'vmov.f32)
					 y x)))
				(,stack-sc
				 (let ((offset (* (tn-offset y) n-word-bytes)))
				   (inst ,(if double-p 'vstr.64 'vstr.32) x nfp
					 offset))))))
                (define-move-vop ,name :move-arg
                  (,sc descriptor-reg) (,sc)))))
  (frob move-single-float-arg single-reg single-stack nil)
  (frob move-double-float-arg double-reg double-stack t))



;;;; Complex float move functions

(defun complex-single-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (tn-offset x)))
(defun complex-single-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'single-reg)
                  :offset (1+ (tn-offset x))))

(defun complex-double-reg-real-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
                  :offset (tn-offset x)))
(defun complex-double-reg-imag-tn (x)
  (make-random-tn :kind :normal :sc (sc-or-lose 'double-reg)
                  :offset (1+ (tn-offset x))))


(define-move-fun (load-complex-single 2) (vop x y)
  ((complex-single-stack) (complex-single-reg))
  (let ((real-tn (complex-single-reg-real-tn y)))
    (inst vldr.32 real-tn cfp-tn (frame-byte-offset (tn-offset x))))
  (let ((imag-tn (complex-single-reg-imag-tn y)))
    (inst vldr.32 imag-tn cfp-tn (frame-byte-offset (1+ (tn-offset x))))))

(define-move-fun (store-complex-single 2) (vop x y)
  ((complex-single-reg) (complex-single-stack))
  (let ((real-tn (complex-single-reg-real-tn x)))
    (inst vstr.32 real-tn cfp-tn (frame-byte-offset (tn-offset y))))
  (let ((imag-tn (complex-single-reg-imag-tn y)))
    (inst vstr.32 imag-tn cfp-tn (frame-byte-offset (1+ (tn-offset y))))))

(define-move-fun (load-complex-double 4) (vop x y)
  ((complex-double-stack) (complex-double-reg))
  (let ((real-tn (complex-double-reg-real-tn y)))
    (inst vldr.64 real-tn cfp-tn (frame-byte-offset (tn-offset x))))
  (let ((imag-tn (complex-double-reg-imag-tn y)))
    (inst vldr.64 imag-tn cfp-tn (frame-byte-offset (+ (tn-offset x) 2)))))

(define-move-fun (store-complex-double 4) (vop x y)
  ((complex-double-reg) (complex-double-stack))
  (let ((real-tn (complex-double-reg-real-tn x)))
    (inst vstr.64 real-tn cfp-tn (frame-byte-offset (tn-offset y))))
  (let ((imag-tn (complex-double-reg-imag-tn x)))
    (inst vstr.64 imag-tn cfp-tn (frame-byte-offset (+ (tn-offset y) 2)))))


;;;
;;; Complex float register to register moves.
;;;
(define-vop (complex-single-move)
  (:args (x :scs (complex-single-reg) :target y
            :load-if (not (location= x y))))
  (:results (y :scs (complex-single-reg) :load-if (not (location= x y))))
  (:note "complex single float move")
  (:generator 0
     (unless (location= x y)
       (let ((x-real (complex-single-reg-real-tn x))
             (y-real (complex-single-reg-real-tn y)))
         (inst vmov.f32 y-real x-real))
       (let ((x-imag (complex-single-reg-imag-tn x))
             (y-imag (complex-single-reg-imag-tn y)))
         (inst vmov.f32 y-imag x-imag)))))
;;;
(define-move-vop complex-single-move :move
  (complex-single-reg) (complex-single-reg))

(define-vop (complex-double-move)
  (:args (x :scs (complex-double-reg)
            :target y :load-if (not (location= x y))))
  (:results (y :scs (complex-double-reg) :load-if (not (location= x y))))
  (:note "complex double float move")
  (:generator 0
     (unless (location= x y)
       ;; Note the complex-float-regs are aligned to every second
       ;; float register so there is not need to worry about overlap.
       (let ((x-real (complex-double-reg-real-tn x))
             (y-real (complex-double-reg-real-tn y)))
         (inst vmov.f64 y-real x-real))
       (let ((x-imag (complex-double-reg-imag-tn x))
             (y-imag (complex-double-reg-imag-tn y)))
         (inst vmov.f64 y-imag x-imag)))))
;;;
(define-move-vop complex-double-move :move
  (complex-double-reg) (complex-double-reg))


;;;
;;; Move from a complex float to a descriptor register allocating a
;;; new complex float object in the process.
;;;
(define-vop (move-from-complex-single)
  (:args (x :scs (complex-single-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) temp2)
  (:note "complex single float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y temp temp2 complex-single-float-widetag
                               complex-single-float-size)
       (let ((real-tn (complex-single-reg-real-tn x)))
         (inst vstr.32 real-tn y (- (* complex-single-float-real-slot
                                    n-word-bytes)
                                 other-pointer-lowtag)))
       (let ((imag-tn (complex-single-reg-imag-tn x)))
         (inst vstr.32 imag-tn y (- (* complex-single-float-imag-slot
                                    n-word-bytes)
                                 other-pointer-lowtag))))))
;;;
(define-move-vop move-from-complex-single :move
  (complex-single-reg) (descriptor-reg))

(define-vop (move-from-complex-double)
  (:args (x :scs (complex-double-reg) :to :save))
  (:results (y :scs (descriptor-reg)))
  (:temporary (:scs (non-descriptor-reg)) temp)
  (:temporary (:scs (non-descriptor-reg)) temp2)
  (:note "complex double float to pointer coercion")
  (:generator 13
     (with-fixed-allocation (y temp temp2 complex-double-float-widetag
                               complex-double-float-size)
       (let ((real-tn (complex-double-reg-real-tn x)))
         (inst vstr.64 real-tn y (- (* complex-double-float-real-slot
                                    n-word-bytes)
                                 other-pointer-lowtag)))
       (let ((imag-tn (complex-double-reg-imag-tn x)))
         (inst vstr.64 imag-tn y (- (* complex-double-float-imag-slot
                                    n-word-bytes)
                                 other-pointer-lowtag))))))
;;;
(define-move-vop move-from-complex-double :move
  (complex-double-reg) (descriptor-reg))


;;;
;;; Move from a descriptor to a complex float register
;;;
(define-vop (move-to-complex-single)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-single-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-single-reg-real-tn y)))
      (inst vldr.32 real-tn x (- (* complex-single-float-real-slot n-word-bytes)
				 other-pointer-lowtag)))
    (let ((imag-tn (complex-single-reg-imag-tn y)))
      (inst vldr.32 imag-tn x (- (* complex-single-float-imag-slot n-word-bytes)
				 other-pointer-lowtag)))))
(define-move-vop move-to-complex-single :move
  (descriptor-reg) (complex-single-reg))

(define-vop (move-to-complex-double)
  (:args (x :scs (descriptor-reg)))
  (:results (y :scs (complex-double-reg)))
  (:note "pointer to complex float coercion")
  (:generator 2
    (let ((real-tn (complex-double-reg-real-tn y)))
      (inst vldr.64 real-tn x (- (* complex-double-float-real-slot n-word-bytes)
				 other-pointer-lowtag)))
    (let ((imag-tn (complex-double-reg-imag-tn y)))
      (inst vldr.64 imag-tn x (- (* complex-double-float-imag-slot n-word-bytes)
				 other-pointer-lowtag)))))
(define-move-vop move-to-complex-double :move
  (descriptor-reg) (complex-double-reg))


;;;
;;; Complex float move-arg vop
;;;
(define-vop (move-complex-single-float-arg)
  (:args (x :scs (complex-single-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-single-reg))))
  (:results (y))
  (:note "complex single-float arg move")
  (:generator 1
    (sc-case y
      (complex-single-reg
       (unless (location= x y)
         (let ((x-real (complex-single-reg-real-tn x))
               (y-real (complex-single-reg-real-tn y)))
           (inst vmov.f32 y-real x-real))
         (let ((x-imag (complex-single-reg-imag-tn x))
               (y-imag (complex-single-reg-imag-tn y)))
           (inst vmov.f32 y-imag x-imag))))
      (complex-single-stack
       (let ((offset (* (tn-offset y) n-word-bytes)))
         (let ((real-tn (complex-single-reg-real-tn x)))
           (inst vstr.32 real-tn nfp offset))
         (let ((imag-tn (complex-single-reg-imag-tn x)))
           (inst vstr.32 imag-tn nfp (+ offset n-word-bytes))))))))
(define-move-vop move-complex-single-float-arg :move-arg
  (complex-single-reg descriptor-reg) (complex-single-reg))

(define-vop (move-complex-double-float-arg)
  (:args (x :scs (complex-double-reg) :target y)
         (nfp :scs (any-reg) :load-if (not (sc-is y complex-double-reg))))
  (:results (y))
  (:note "complex double-float arg move")
  (:generator 2
    (sc-case y
      (complex-double-reg
       (unless (location= x y)
         (let ((x-real (complex-double-reg-real-tn x))
               (y-real (complex-double-reg-real-tn y)))
           (inst vmov.f64 y-real x-real))
         (let ((x-imag (complex-double-reg-imag-tn x))
               (y-imag (complex-double-reg-imag-tn y)))
           (inst vmov.f64 y-imag x-imag))))
      (complex-double-stack
       (let ((offset (* (tn-offset y) n-word-bytes)))
         (let ((real-tn (complex-double-reg-real-tn x)))
           (inst vstr.64 real-tn nfp offset))
         (let ((imag-tn (complex-double-reg-imag-tn x)))
           (inst vstr.64 imag-tn nfp (+ offset (* 2 n-word-bytes)))))))))
(define-move-vop move-complex-double-float-arg :move-arg
  (complex-double-reg descriptor-reg) (complex-double-reg))


(define-move-vop move-arg :move-arg
  (single-reg double-reg complex-single-reg complex-double-reg)
  (descriptor-reg))

