;;############################################################################
;; Module: townsfolk
;;
;; All townsfolk are loaded from this file. This file should be loaded by saved
;; games as well as new ones.
;;
;; Townsfolk are NPCs who have conversations, schedules, etc. They are
;; "permanent", in that they aren't automatically generated for combat or guard
;; duty.
;;############################################################################

;;============================================================================
;; Procedures
;;
;; Procedures common but limited to this module should go here.
;;============================================================================

;;----------------------------------------------------------------------------
;; mk-townsman -- create a townsman from the list of options in `kwargs`, which
;; is of the form '((keyword1 . value1) (keyword2 . value2) ...). In addition
;; to the keywords supported by `mk-char`, this requires:
;;
;; `ctor` - a procedure of no args that creates the gob
;; `desc` - a string used as a summary description of the townsman
;;----------------------------------------------------------------------------
(define (mk-townsman kwargs)
  (define (arg key)
    (get kwargs key))
  (let ((kchar (mk-char kwargs)))
    (bind kchar (apply (arg 'ctor)))
    (kern-char-set-description kchar (arg 'desc))
    kchar))

;;============================================================================
;; The files to load.
;;============================================================================
(load "townsfolk/abe.scm")