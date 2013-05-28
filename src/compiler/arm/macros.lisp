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
             `(defmacro ,op (dst obj &optional (slot 0) (lowtag 0))
                `(inst ,',inst ,dst ,obj (- (* ,slot n-word-bytes) ,lowtag)))))
  (def loadw ldr)
  (def storew str))

(defmacro load-symbol (reg symbol)
  `(inst ldr ,reg null-tn (static-symbol-offset ,symbol)))

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
  (frob function)

  ;; FIXME: These are only good for static-symbols, so why not
  ;; statically-allocate the static-symbol TLS slot indices at
  ;; cross-compile time so we can just use a fixed offset within the
  ;; TLS block instead of mucking about with the extra memory access
  ;; (and temp register, for stores)?
  #!+sb-thread
  (defmacro load-tl-symbol-value (reg symbol)
    `(progn
       (inst ldr ,reg null-tn
             (+ (static-symbol-offset ',symbol)
                (ash symbol-tls-index-slot word-shift)
                (- other-pointer-lowtag)))
       (inst ldr ,reg thread-base-tn ,reg)))
  #!-sb-thread
  (defmacro load-tl-symbol-value (reg symbol)
    `(load-symbol-value ,reg ,symbol))

  #!+sb-thread
  (defmacro store-tl-symbol-value (reg symbol temp)
    `(progn
       (inst ldr ,temp null-tn
             (+ (static-symbol-offset ',symbol)
                (ash symbol-tls-index-slot word-shift)
                (- other-pointer-lowtag)))
       (inst str ,reg thread-base-tn ,temp)))
  #!-sb-thread
  (defmacro store-tl-symbol-value (reg symbol temp)
    (declare (ignore temp))
    `(store-symbol-value ,reg ,symbol)))

(defmacro load-type (target source &optional (offset 0))
  "Loads the type bits of a pointer into target independent of
  byte-ordering issues."
  (once-only ((n-target target)
              (n-source source)
              (n-offset offset))
    (ecase *backend-byte-order*
      (:little-endian
       `(inst ldrb ,n-target ,n-source ,n-offset))
      (:big-endian
       `(inst ldrb ,n-target ,n-source (+ ,n-offset (1- n-word-bytes)))))))
