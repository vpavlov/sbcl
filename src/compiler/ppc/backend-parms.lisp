(in-package "SB!VM")

(def!constant +backend-fasl-file-implementation+ :ppc)
(setf *backend-register-save-penalty* 3)
(setf *backend-byte-order* :big-endian)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; On Linux, the ABI specifies the page size to be 4k-64k, use the
  ;; maximum of that range. FIXME: it'd be great if somebody would
  ;; find out whether using exact multiples of the page size actually
  ;; matters in the few places where that's done, or whether we could
  ;; just use 4k everywhere.
  (setf *backend-page-bytes* #!+linux 65536 #!-linux 4096))

;;; The size of the card table in cards.  This doesn't directly constrain
;;; the heap size, since the table is addressed modulo its size.
;;; NOTE: in emit-write-barrier the address must be AND-ed with
;;;       gencgc-card-count - 1. As of the time of this writing,
;;;       it ASSUMES that this is 2^16, so that the AND is
;;;       implicitly performed by the srwi instruction. If this
;;;       is changed, more/different instructions will be needed
;;;       to perform the AND. However, since *backend-page-bytes* is
;;;       2^16, and gencgc-card-count is 2^16, this totals up to
;;;       2^32 which is the addressable space in ppc 32bit anyway
;;;       --VNP 2012-03-23
#!+sb-sw-barrier
(def!constant gencgc-card-count (ash 1 16))
;;; The overflow area of the card table (in cards).  Small constant
;;; offsets are precomputed away, but after the modulo computation, so
;;; an overflow area must be left at the end of the table.
;;; NOTE: The overflow area is not used in the PPC port
;;;       --VNP 2012-03-21
#!+sb-sw-barrier
(def!constant gencgc-overflow-card-count 0)
;;; The size in bytes of GENCGC cards, i.e. the granularity at which
;;; writes to old generations are logged.  With mprotect-based write
;;; barriers, this must be a multiple of the OS page size.
(def!constant gencgc-card-bytes *backend-page-bytes*)
;;; The minimum size of new allocation regions.  While it doesn't
;;; currently make a lot of sense to have a card size lower than
;;; the alloc granularity, it will, once we are smarter about finding
;;; the start of objects.
(def!constant gencgc-alloc-granularity 0)
;;; The minimum size at which we release address ranges to the OS.
;;; This must be a multiple of the OS page size.
(def!constant gencgc-release-granularity *backend-page-bytes*)
