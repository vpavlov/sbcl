;;;; the ARM VM definition of operand loading/saving and the Move VOP

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

(define-move-fun (load-immediate 1) (vop x y)
  ((null immediate)
   (any-reg descriptor-reg))
  (let ((val (tn-value x)))
    (etypecase val
      (integer
       (inst lr y (fixnumize val)))
      (null
       (move y null-tn))
      (symbol
       (load-symbol y val))
      (character
       (inst lr y (logior (ash (char-code val) n-widetag-bits)
			  character-widetag))))))
