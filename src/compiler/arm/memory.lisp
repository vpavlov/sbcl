;;;; the ARM definitions of some general purpose memory reference VOPs
;;;; inherited by basic memory reference operations

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; Cell-Ref and Cell-Set are used to define VOPs like CAR, where the offset to
;;; be read or written is a property of the VOP used.
;;;
(define-vop (cell-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (loadw value object offset lowtag)))
;;;
(define-vop (cell-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 4
    (storew value object offset lowtag)))

;;; Slot-Ref and Slot-Set are used to define VOPs like Closure-Ref, where the
;;; offset is constant at compile time, but varies for different uses.  We add
;;; in the standard g-vector overhead.
;;;
(define-vop (slot-ref)
  (:args (object :scs (descriptor-reg)))
  (:results (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (loadw value object (+ base offset) lowtag)))
;;;
(define-vop (slot-set)
  (:args (object :scs (descriptor-reg))
         (value :scs (descriptor-reg any-reg)))
  (:variant-vars base lowtag)
  (:info offset)
  (:generator 4
    (storew value object (+ base offset) lowtag)))



;;;; Indexed references:

;;; Define some VOPs for indexed memory reference.
(defmacro define-indexer (name write-p op shift)
  `(define-vop (,name)
       (:args (object :scs (descriptor-reg))
	      (index :scs (any-reg immediate))
	      ,@(when write-p
		      '((value :scs (any-reg descriptor-reg) :target result))))
     (:arg-types * tagged-num ,@(when write-p '(*)))
     (:temporary (:scs (non-descriptor-reg)) temp)
     (:results (,(if write-p 'result 'value)
		 :scs (any-reg descriptor-reg)))
     (:result-types *)
     (:variant-vars offset lowtag)
     (:policy :fast-safe)
     (:generator 5
		 (sc-case index
			  (immediate
			   (let ((offset (- (+ (ash (tn-value index)
						    (- word-shift ,shift))
					       (ash offset word-shift))
					    lowtag)))
			     (inst lr temp offset)
			     (inst ,op value object temp)))
			  (t
			   ,@(unless (zerop shift)
				     `((inst lsr temp index ,shift)))
			   (inst add temp ,(if (zerop shift) 'index 'temp)
				 (- (ash offset word-shift) lowtag))
			   (inst ,op value object temp)))
		 ,@(when write-p
			 '((move result value))))))

(define-indexer word-index-ref nil ldr 0)
(define-indexer word-index-set t str 0)
(define-indexer halfword-index-ref nil ldrh 1)
(define-indexer signed-halfword-index-ref nil ldrsh 1)
(define-indexer halfword-index-set t strh 1)
(define-indexer byte-index-ref nil ldrb 2)
(define-indexer signed-byte-index-ref nil ldrsb 2)
(define-indexer byte-index-set t strb 2)

#!+compare-and-swap-vops
(define-vop (word-index-cas)
  (:args (object :scs (descriptor-reg))
         (index :scs (any-reg immediate))
         (old-value :scs (any-reg descriptor-reg))
         (new-value :scs (any-reg descriptor-reg)))
  (:arg-types * tagged-num * *)
  (:temporary (:sc non-descriptor-reg) temp)
  (:results (result :scs (any-reg descriptor-reg) :from :load))
  (:result-types *)
  (:variant-vars offset lowtag)
  (:policy :fast-safe)
  (:generator 5
    (sc-case index
      (immediate
       (let ((offset (- (+ (ash (tn-value index) word-shift)
                           (ash offset word-shift))
                        lowtag)))
         (inst lr temp offset)))
      (t
       ;; KLUDGE: This relies on N-FIXNUM-TAG-BITS being the same as
       ;; WORD-SHIFT.  I know better than to do this.  --AB, 2010-Jun-16
       (inst add temp index (- (ash offset word-shift) lowtag))))
    (inst add temp temp object)
    (inst isb)
    LOOP
    (inst ldrex result temp)
    (inst cmp result old-value)
    (inst b EXIT :cnd :ne)
    (inst strex temp new-value temp)
    (inst cmp temp 1)
    (inst b LOOP :cnd :ne)
    EXIT
    (inst isb)))
