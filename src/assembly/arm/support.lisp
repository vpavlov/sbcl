;;;; the machine-specific support routines needed by the file assembler

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(!def-vm-support-routine generate-call-sequence (name style vop)
  (ecase style
    ((:raw :none)
     (let ((jump (make-symbol "JUMP")))
       (values
        `((inst lr ,jump (make-fixup ',name :assembly-routine))
          (inst blx ,jump))
        `((:temporary (:sc any-reg) ,jump)))))
    (:full-call
     (let ((temp (make-symbol "TEMP"))
	   (jump (make-symbol "JUMP"))
	   (lra (make-symbol "LRA")))
       (values
	`((let ((lra-label (gen-label)))
	    (inst compute-lra-from-code ,lra code-tn lra-label ,temp)
	    (note-next-instruction ,vop :call-site)
	    (inst lr ,jump (make-fixup ',name :assembly-routine))
	    (inst blx ,jump)
	    (emit-return-pc lra-label)
	    (note-this-location ,vop :single-value-return)
	    (without-scheduling ()
	      (move csp-tn ocfp-tn)
	      (inst nop))
	    (inst compute-code-from-lra code-tn lra-tn lra-label ,temp)))
	`((:temporary (:scs (non-descriptor-reg) :from (:eval 0) :to (:eval 1))
		      ,temp)
	  (:temporary (:sc descriptor-reg :offset lra-offset
			   :from (:eval 0) :to (:eval 1)) ,lra)
	  (:temporary (:sc any-reg) ,jump)
	  (:save-p :compute-only)))))))

(!def-vm-support-routine generate-return-sequence (style)
  (ecase style
    (:raw
     `((inst bx (make-random-tn :kind :normal :sc (sc-or-lose 'interior-reg)
				:offset lr-offset))))
    (:full-call
     `((lisp-return (make-random-tn :kind :normal
                                    :sc (sc-or-lose 'descriptor-reg )
                                    :offset lra-offset)
		    (make-random-tn :kind :normal
                                    :sc (sc-or-lose 'interior-reg )
                                    :offset lip-offset)
		    :offset 2)))
    (:none)))
