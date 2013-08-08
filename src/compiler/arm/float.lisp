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



;;;; Arithmetic VOPs:

(define-vop (float-op)
  (:args (x) (y))
  (:results (r))
  (:policy :fast-safe)
  (:note "inline float arithmetic")
  (:vop-var vop)
  (:save-p :compute-only))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-op)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:results (r :scs (,sc)))
                (:arg-types ,ptype ,ptype)
                (:result-types ,ptype))))
  (frob single-float-op single-reg single-float)
  (frob double-float-op double-reg double-float))

(macrolet ((frob (op sinst sname scost dinst dname dcost)
             `(progn
                (define-vop (,sname single-float-op)
                  (:translate ,op)
                  (:generator ,scost
                    (inst ,sinst r x y)))
                (define-vop (,dname double-float-op)
                  (:translate ,op)
                  (:generator ,dcost
                    (inst ,dinst r x y))))))
  (frob + vadd.f32 +/single-float 2 vadd.f64 +/double-float 2)
  (frob - vsub.f32 -/single-float 2 vsub.f64 -/double-float 2)
  (frob * vmul.f32 */single-float 4 vmul.f64 */double-float 5)
  (frob / vdiv.f32 //single-float 12 vdiv.f64 //double-float 19))

(macrolet ((frob (name inst translate sc type)
             `(define-vop (,name)
                (:args (x :scs (,sc)))
                (:results (y :scs (,sc)))
                (:translate ,translate)
                (:policy :fast-safe)
                (:arg-types ,type)
                (:result-types ,type)
                (:note "inline float arithmetic")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 1
                  (note-this-location vop :internal-error)
                  (inst ,inst y x)))))
  (frob abs/single-float vabs.f32 abs single-reg single-float)
  (frob abs/double-float vabs.f64 abs double-reg double-float)
  (frob %negate/single-float vneg.f32 %negate single-reg single-float)
  (frob %negate/double-float vneg.f64 %negate double-reg double-float))



;;;; Comparison:

(define-vop (float-compare)
  (:args (x) (y))
  (:conditional)
  (:info target not-p)
  (:variant-vars format yep nope)
  (:policy :fast-safe)
  (:note "inline float comparison")
  (:vop-var vop)
  (:save-p :compute-only)
  (:generator 3
    (note-this-location vop :internal-error)
    (ecase format
      (:single
       (inst vcmp.f32 x y))
      (:double
       (inst vcmp.f64 x y)))
    (inst vmrs apsr-tn)
    (inst b target :cnd (if not-p nope yep))))

(macrolet ((frob (name sc ptype)
             `(define-vop (,name float-compare)
                (:args (x :scs (,sc))
                       (y :scs (,sc)))
                (:arg-types ,ptype ,ptype))))
  (frob single-float-compare single-reg single-float)
  (frob double-float-compare double-reg double-float))

(macrolet ((frob (translate yep nope sname dname)
             `(progn
                (define-vop (,sname single-float-compare)
                  (:translate ,translate)
                  (:variant :single ,yep ,nope))
                (define-vop (,dname double-float-compare)
                  (:translate ,translate)
                  (:variant :double ,yep ,nope)))))
  (frob < :lt :ge </single-float </double-float)
  (frob > :gt :le >/single-float >/double-float)
  (frob = :eq :ne eql/single-float eql/double-float))



;;;; Conversion:

(macrolet ((frob (name translate inst from-sc to-sc from-type to-type)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
                (:results (y :scs (,to-sc)))
                (:arg-types ,from-type)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator
		 2
		 ;; FIXME: not sure if this works with doubles
		 ;; -- VNP 2013-08-08
		 (inst vmov y x)
		 (inst ,inst y y)))))
  (frob %single-float/signed %single-float vcvt.f32.s32
	signed-reg single-reg
	signed-num single-float)
  (frob %single-float/unsigned %single-float vcvt.f32.u32
	unsigned-reg single-reg
	unsigned-num single-float)
  (frob %double-float/signed %double-float vcvt.f64.s32
	signed-reg double-reg
	signed-num double-float)
  (frob %double-float/unsigned %double-float vcvt.f64.u32
	unsigned-reg double-reg
	unsigned-num double-float))

(macrolet ((frob (name translate inst from-sc from-type to-sc to-type)
             `(define-vop (,name)
                (:args (x :scs (,from-sc)))
                (:results (y :scs (,to-sc)))
                (:arg-types ,from-type)
                (:result-types ,to-type)
                (:policy :fast-safe)
                (:note "inline float coercion")
                (:translate ,translate)
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 2
                  (note-this-location vop :internal-error)
                  (inst ,inst y x)))))
  (frob %single-float/double-float %single-float vcvt.f32.f64
	double-reg double-float single-reg single-float)
  (frob %double-float/single-float %double-float vcvt.f64.f32
	single-reg single-float double-reg double-float))

(macrolet ((frob (trans from-sc from-type inst)
             `(define-vop (,(symbolicate trans "/" from-type))
                (:args (x :scs (,from-sc)))
                (:results (y :scs (signed-reg)))
                (:arg-types ,from-type)
                (:result-types signed-num)
                (:translate ,trans)
                (:policy :fast-safe)
                (:note "inline float truncate")
                (:vop-var vop)
                (:save-p :compute-only)
                (:generator 5
                  (note-this-location vop :internal-error)
		  (inst ,inst y x)))))
  (frob %unary-truncate/single-float single-reg single-float vcvt.s32.f32)
  (frob %unary-truncate/double-float double-reg double-float vcvt.s32.f64)
  (frob %unary-round single-reg single-float vcvtr.s32.f32)
  (frob %unary-round double-reg double-float vcvtr.s32.f64))

(define-vop (make-single-float)
  (:args (bits :scs (signed-reg) :target res
               :load-if (not (sc-is bits signed-stack))))
  (:results (res :scs (single-reg)
                 :load-if (not (sc-is res single-stack))))
  (:temporary (:scs (signed-reg) :from (:argument 0) :to (:result 0)) temp)
  (:arg-types signed-num)
  (:result-types single-float)
  (:translate make-single-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (sc-case bits
      (signed-reg
       (sc-case res
         (single-reg
	  (inst vmov res bits))
         (single-stack
	  (inst str bits cfp-tn (frame-byte-offset (tn-offset res))))))
      (signed-stack
       (sc-case res
         (single-reg
	  (inst vldr.32 res cfp-tn (frame-byte-offset (tn-offset bits))))
         (single-stack
          (unless (location= bits res)
	    (inst ldr temp cfp-tn (frame-byte-offset (tn-offset bits)))
	    (inst str temp cfp-tn (frame-byte-offset (tn-offset res))))))))
    ))

(define-vop (make-double-float)
  (:args (hi-bits :scs (signed-reg))
         (lo-bits :scs (unsigned-reg)))
  (:results (res :scs (double-reg)
                 :load-if (not (sc-is res double-stack))))
  (:temporary (:scs (double-stack)) temp)
  (:arg-types signed-num unsigned-num)
  (:result-types double-float)
  (:translate make-double-float)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 2
    (let ((stack-tn (sc-case res
                      (double-stack res)
                      (double-reg temp))))
      (inst str hi-bits cfp-tn (frame-byte-offset (tn-offset stack-tn)))
      (inst str lo-bits cfp-tn (frame-byte-offset (1+ (tn-offset stack-tn))))
      (when (sc-is res double-reg)
	(inst vldr.64 res cfp-tn (frame-byte-offset (tn-offset temp)))))))

(define-vop (single-float-bits)
  (:args (float :scs (single-reg descriptor-reg)
                :load-if (not (sc-is float single-stack))))
  (:results (bits :scs (signed-reg)
                  :load-if (or (sc-is float descriptor-reg single-stack)
                               (not (sc-is bits signed-stack)))))
  (:arg-types single-float)
  (:result-types signed-num)
  (:translate single-float-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 4
    (sc-case bits
      (signed-reg
       (sc-case float
         (single-reg
	  (inst vmov bits float))
	 (single-stack
	  (inst ldr bits cfp-tn (frame-byte-offset (tn-offset float))))))
      (signed-stack
       (sc-case float
         (single-reg
	  (inst vstr.32 float cfp-tn (frame-byte-offset (tn-offset bits)))))))))

(define-vop (double-float-high-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (hi-bits :scs (signed-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types signed-num)
  (:translate double-float-high-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst vstr.64 float cfp-tn (frame-byte-offset (tn-offset stack-temp)))
       (inst ldr hi-bits cfp-tn (frame-byte-offset (tn-offset stack-temp))))
      (double-stack
       (inst ldr hi-bits cfp-tn (frame-byte-offset (tn-offset float))))
      (descriptor-reg
       (loadw hi-bits float double-float-value-slot other-pointer-lowtag)))))

(define-vop (double-float-low-bits)
  (:args (float :scs (double-reg descriptor-reg)
                :load-if (not (sc-is float double-stack))))
  (:results (lo-bits :scs (unsigned-reg)))
  (:temporary (:scs (double-stack)) stack-temp)
  (:arg-types double-float)
  (:result-types unsigned-num)
  (:translate double-float-low-bits)
  (:policy :fast-safe)
  (:vop-var vop)
  (:generator 5
    (sc-case float
      (double-reg
       (inst vstr.64 float cfp-tn (frame-byte-offset (tn-offset stack-temp)))
       (inst ldr lo-bits cfp-tn
	     (frame-byte-offset (1+ (tn-offset stack-temp)))))
      (double-stack
       (inst ldr lo-bits cfp-tn (frame-byte-offset
				 (1+ (tn-offset stack-temp)))))
      (descriptor-reg
       (loadw lo-bits float (1+ double-float-value-slot)
	      other-pointer-lowtag)))))
