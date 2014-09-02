;; Procedures for managing place gobs.

;; Return the place gob, creating one if necessary.
(define (place-ensure-gob kplace)
  (let ((gob (kern-place-get-gob kplace)))
    (cond ((null? gob)
	   (let ((gob (tbl-mk)))
	     (kern-place-set-gob kplace gob)
	     gob))
	  (else gob)
	  )))

;; Set the place description.
(define (place-describe! kplace description)
  (let ((gob (place-ensure-gob kplace)))
    (tbl-set! gob 'description description)
    ))

;; Mark the place's location as unknown.
(define (place-set-location-unknown! kplace)
  (let ((gob (place-ensure-gob kplace)))
    (tbl-set! gob 'location-unknown? #t)
    ))

;; Return #t if the location is marked unknown. Defaults to #f (known).
(define (place-location-is-unknown? kplace)
  (let ((gob (kern-place-get-gob kplace)))
    (if (null? gob)
	#f
	(let ((v (tbl-get gob 'location-unknown?)))
	  (and v (notnull? v))
	  ))))
