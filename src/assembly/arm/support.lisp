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
     (let ((jump (make-symbol "JUMP")))
       (values
        `((inst lr ,jump (make-fixup ',name :assembly-routine))
          (note-this-location ,vop :call-site)
          (inst blx ,jump)
          (note-this-location ,vop :single-value-return)
          (let ((single-value (gen-label)))
            (inst b single-value :cc)
            ;; @@@TODO: figure out what this should be in the ARM ABI
            ;; --vnp 2013-05-17
            ;; (move esp-tn ebx-tn)
            (emit-label single-value)))
        `((:temporary (:sc any-reg) ,jump)
          (:save-p :compute-only)))))))

(!def-vm-support-routine generate-return-sequence (style)
  (ecase style
    ((:raw :full-call)
     `((inst bx (make-random-tn :kind :normal
                                :sc (sc-or-lose 'interior-reg)
                                :offset lr-offset))))
    (:none)))
