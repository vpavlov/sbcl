;;;; the PPC VM definition of operand loading/saving and the Move VOP

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
    ((immediate)
     (any-reg descriptor-reg))
  (let ((val (encode-value-if-immediate x)))
    (if (zerop val)
        (inst eor y y)
        (inst lr y val))))
