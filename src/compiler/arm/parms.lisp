;;;; This file contains some parameterizations of various VM
;;;; attributes for the ARM. This file is separate from other stuff so
;;;; that it can be compiled and loaded earlier.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; This software is derived from the CMU CL system, which was
;;;; written at Carnegie Mellon University and released into the
;;;; public domain. The software is in the public domain and is
;;;; provided with absolutely no warranty. See the COPYING and CREDITS
;;;; files for more information.

(in-package "SB!VM")

;;; the number of bits per word, where a word holds one lisp descriptor
(def!constant n-word-bits 32)

;;; the natural width of a machine word (as seen in e.g. register width,
;;; address space)
(def!constant n-machine-word-bits 32)

;;; the number of bits per byte, where a byte is the smallest
;;; addressable object
(def!constant n-byte-bits 8)

(def!constant float-sign-shift 31)

(def!constant single-float-bias 126)
(defconstant-eqx single-float-exponent-byte    (byte 8 23) #'equalp)
(defconstant-eqx single-float-significand-byte (byte 23 0) #'equalp)
(def!constant single-float-normal-exponent-min 1)
(def!constant single-float-normal-exponent-max 254)
(def!constant single-float-hidden-bit (ash 1 23))
(def!constant single-float-trapping-nan-bit (ash 1 22))

(def!constant double-float-bias 1022)
(defconstant-eqx double-float-exponent-byte    (byte 11 20) #'equalp)
(defconstant-eqx double-float-significand-byte (byte 20 0)  #'equalp)
(def!constant double-float-normal-exponent-min 1)
(def!constant double-float-normal-exponent-max #x7FE)
(def!constant double-float-hidden-bit (ash 1 20))
(def!constant double-float-trapping-nan-bit (ash 1 19))

(def!constant single-float-digits
  (+ (byte-size single-float-significand-byte) 1))

(def!constant double-float-digits
  (+ (byte-size double-float-significand-byte) n-word-bits 1))
