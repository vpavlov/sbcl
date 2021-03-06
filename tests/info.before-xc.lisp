;;;; tests of the INFO compiler database, initially with particular
;;;; reference to knowledge of constants, intended to be executed as
;;;; soon as the cross-compiler is built.

;;;; This software is part of the SBCL system. See the README file for
;;;; more information.
;;;;
;;;; While most of SBCL is derived from the CMU CL system, the test
;;;; files (like this one) were written from scratch after the fork
;;;; from CMU CL.
;;;;
;;;; This software is in the public domain and is provided with
;;;; absolutely no warranty. See the COPYING and CREDITS files for
;;;; more information.

(in-package "SB!KERNEL")

(/show "beginning tests/info.before-xc.lisp")

(assert (eq (sb!int:info :variable :kind 'sb!vm:vector-data-offset)
            :constant))
;;; It's possible in general for a constant to have the value NIL, but
;;; not for vector-data-offset, which must be a number:
(assert (boundp 'sb!vm:vector-data-offset))
(assert (integerp (symbol-value 'sb!vm:vector-data-offset)))

(in-package "SB!C")

(let ((foo-iv (packed-info-insert +nil-packed-infos+ +no-auxilliary-key+
                                  5 "hi 5"))
      (bar-iv (packed-info-insert +nil-packed-infos+ +no-auxilliary-key+
                                  6 "hi 6"))
      (baz-iv (packed-info-insert +nil-packed-infos+ 'mumble
                                  9 :phlebs)))

  ;; removing nonexistent types returns NIL
  (assert (equal nil (packed-info-remove foo-iv +no-auxilliary-key+
                                         4 6 7)))
  (assert (equal nil (packed-info-remove baz-iv 'mumble 4 6 7)))

  ;; removing the one info shrinks the vector to nothing
  ;; and all values of nothing are EQ
  (assert (equalp #(0) (packed-info-remove foo-iv +no-auxilliary-key+ 5)))
  (assert (eq (packed-info-remove foo-iv +no-auxilliary-key+ 5)
              (packed-info-remove bar-iv +no-auxilliary-key+ 6)))
  (assert (eq (packed-info-remove foo-iv +no-auxilliary-key+ 5)
              (packed-info-remove baz-iv 'mumble 9))))

;; Test that the packing invariants are maintained:
;; 1. if an FDEFINITION is present in an info group, it is the *first* info
;; 2. if SETF is an auxilliary key, it is the *first* aux key
(let ((vect +nil-packed-infos+)
      (s 'foo))
  (flet ((iv-put (aux-key number val)
           (setq vect (packed-info-insert vect aux-key number val)))
         (iv-del (aux-key number)
           (awhen (packed-info-remove vect aux-key number)
             (setq vect it)))
         (verify (ans)
           (let (result) ; => ((name . ((type . val) (type . val))) ...)
             (%call-with-each-info
              (lambda (name type-number value)
                (let ((pair (cons type-number value)))
                  (if (equal name (caar result))
                      (push pair (cdar result))
                      (push (list name pair) result))))
              vect s)
             (unless (equal (mapc (lambda (cell)
                                    (rplacd cell (nreverse (cdr cell))))
                                  (nreverse result))
                            ans)
               (error "Failed test ~S" ans)))))
    (iv-put 0 12 "info#12")
    (verify `((,s (12 . "info#12"))))

    (iv-put 'cas 3 "CAS-info#3")
    (verify `((,s (12 . "info#12"))
              ((CAS ,s) (3 . "CAS-info#3"))))

    ;; SETF moves in front of (CAS)
    (iv-put 'setf 6 "SETF-info#6")
    (verify `((,s (12 . "info#12"))
              ((SETF ,s) (6 . "SETF-info#6"))
              ((CAS ,s) (3 . "CAS-info#3"))))

    (iv-put 'frob 15 "FROB-info#15")
    (verify `((,s (12 . "info#12"))
              ((SETF ,s) (6 . "SETF-info#6"))
              ((CAS ,s) (3 . "CAS-info#3"))
              ((FROB ,s) (15 . "FROB-info#15"))))

    (iv-put 'cas 1 "CAS-fdefn") ; pretend
    ;; fdefinition for (CAS) moves in front of its info type #3
    (verify `((,s (12 . "info#12"))
              ((SETF ,s) (6 . "SETF-info#6"))
              ((CAS ,s) (1 . "CAS-fdefn") (3 . "CAS-info#3"))
              ((FROB ,s) (15 . "FROB-info#15"))))

    (iv-put 'frob 1 "FROB-fdefn")
    (verify `((,s (12 . "info#12"))
              ((SETF ,s) (6 . "SETF-info#6"))
              ((CAS ,s) (1 . "CAS-fdefn") (3 . "CAS-info#3"))
              ((FROB ,s) (1 . "FROB-fdefn") (15 . "FROB-info#15"))))

    (iv-put 'setf 1 "SETF-fdefn")
    (verify `((,s (12 . "info#12"))
              ((SETF ,s) (1 . "SETF-fdefn") (6 . "SETF-info#6"))
              ((CAS ,s) (1 . "CAS-fdefn") (3 . "CAS-info#3"))
              ((FROB ,s) (1 . "FROB-fdefn") (15 . "FROB-info#15"))))

    (iv-del 'cas 1)
    (iv-del 'setf 1)
    (iv-del 'frob 1)
    (verify `((,s (12 . "info#12"))
              ((SETF ,s) (6 . "SETF-info#6"))
              ((CAS ,s) (3 . "CAS-info#3"))
              ((FROB ,s) (15 . "FROB-info#15"))))

    (iv-del 'setf 6)
    (iv-del 0 12)
    (verify `(((CAS ,s) (3 . "CAS-info#3"))
              ((FROB ,s) (15 . "FROB-info#15"))))

    (iv-put 'setf 1 "fdefn")
    (verify `(((SETF ,s) (1 . "fdefn"))
              ((CAS ,s) (3 . "CAS-info#3"))
              ((FROB ,s) (15 . "FROB-info#15"))))))

(let ((ht (make-info-hashtable)))
  (setf (info-gethash '(hairy name) ht) :whatever)
  (assert (eq (info-gethash (list 'hairy 'name) ht) :whatever)))

(/show "done with tests/info.before-xc.lisp")
