;;;; a bunch of handy macros for ARM

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

(defmacro move (dst src)
  "Move SRC into DST unless they are location=."
  (once-only ((n-dst dst)
              (n-src src))
    `(unless (location= ,n-dst ,n-src)
       (inst mov ,n-dst ,n-src))))

;; POSSIBLE FIXME: Current implementation of loadw/storew uses plain ldr/str
;;        instructions whose limit for the offset is +/-4096. The ppc backend
;;        uses lwz/stw whose limit is +/-32,768 (more precisely signed 16-bit
;;        value). Thus, in case we need to reach farther than 4096, we have
;;        implement some hairy thing.
;;        --VNP 2013-05-23
(macrolet ((def (op inst)
	     `(defmacro ,op (dst src &optional (slot 0) (lowtag 0))
		`(inst ,',inst ,dst ,src (- (* ,slot n-word-bytes) ,lowtag)))))
  (def loadw ldr)
  (def storew str))
