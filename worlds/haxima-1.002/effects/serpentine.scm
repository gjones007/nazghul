;;----------------------------------------------------------------------------
;; Serpentine
;;
;; Used to implement segmented snake-like critters. Given a "head" object and
;; an npc-type for creating new segments, the serpentine-make-body procedure
;; will spawn a chain of segments on suitable terrain around the head.
;;
;; The head and each segment includes a doubly-linked list in its gob,
;; referring to the location of the previous and next segments. For example, 3
;; segment critter might have these entries for the head:
;;
;;  'next (list 2 3)
;;  'prev nil
;;
;; And the middle segment:
;;
;;  'next (list 3 3)
;;  'prev (list 1 3)
;;
;; And the tail:
;;
;;  'next nil
;;  'prev (list 2 3)
;;
;;----------------------------------------------------------------------------

(define (get-segment-at kplace loc)
  (define (is-segment? kobj)
    (and (kern-obj-is-being? kobj)
	 (let ((gg (gob kobj)))
	   (and (is-tbl? gg)
		(tbl-get gg 'is-segment)))))
  (if (null? loc)
      nil
      (safe-car (filter is-segment? (kern-get-objects-at (cons kplace loc))))))

;; When a segment is damaged, propogate the damage to all other segments and
;; the head.
(define (serpentine-on-damage efgob ksegment)
  (let* ((segloc (kern-obj-get-location ksegment))
	 (kplace (loc-place segloc)))
    (define (propogate-damage kobj dir)
      (let ((kprev (get-segment-at kplace (tbl-get (gob kobj) dir))))
	(if (not (null? kprev))
	    (begin
	      (kern-obj-inc-ref kprev)
	      (kern-char-set-hp kprev (kern-char-get-hp kobj))
	      (propogate-damage kprev dir)
	      (kern-obj-dec-ref kprev)))))
    (propogate-damage ksegment 'prev)
    (propogate-damage ksegment 'next)))

(kern-mk-effect
 'ef_damage_serpentine ;; tag
 "damage serpentine"        ;; name (unused in this case)
 nil                 ;; sprite
 'serpentine-on-damage  ;; exec
 nil                 ;; on-apply
 nil                 ;; on-remove
 nil                 ;; restart
 on-damage-hook      ;; hook
 0                   ;; detection difficulty class
 #f                  ;; is-cumulative?
 -1                  ;; duration (turns, -1 for infinite)
 )

(define xy-to-dir-matrix
  (vector
   (vector NORTHWEST NORTH NORTHEAST)
   (vector WEST      HERE  EAST)
   (vector SOUTHWEST SOUTH SOUTHEAST)
   ))

(define (xy-to-dir x y)
  (vector-ref (vector-ref xy-to-dir-matrix
			  (+ 1 y))
	      (+ 1 x)))

(define (set-facing kobj x0 y0 prev next)
  (define (dir-from x1 y1)
    (xy-to-dir (- x1 x0) (- y1 y0)))
  (let ((next-dir (if (null? next) -1
		      (dir-from (car next) (cadr next))))
	(prev-dir (if (null? prev) -1
		      (dir-from (car prev) (cadr prev))))
	)
    (cond ((= prev-dir WEST)
	   (cond ((= next-dir NORTH) (kern-obj-set-facing kobj NORTHWEST))
		 ((= next-dir EAST) (kern-obj-set-facing kobj WEST))
		 ((= next-dir SOUTH) (kern-obj-set-facing kobj SOUTHWEST))
		 (else (kern-obj-set-facing kobj prev-dir))))
	  ((= prev-dir NORTH)
	   (cond ((= next-dir EAST) (kern-obj-set-facing kobj NORTHEAST))
		 ((= next-dir SOUTH) (kern-obj-set-facing kobj NORTH))
		 ((= next-dir WEST) (kern-obj-set-facing kobj NORTHWEST))
		 (else (kern-obj-set-facing kobj prev-dir))))
	  ((= prev-dir EAST)
	   (cond ((= next-dir SOUTH) (kern-obj-set-facing kobj SOUTHEAST))
		 ((= next-dir WEST) (kern-obj-set-facing kobj EAST))
		 ((= next-dir NORTH) (kern-obj-set-facing kobj NORTHEAST))
		 (else (kern-obj-set-facing kobj prev-dir))))
	  ((= prev-dir SOUTH)
	   (cond ((= next-dir WEST) (kern-obj-set-facing kobj SOUTHWEST))
		 ((= next-dir NORTH) (kern-obj-set-facing kobj SOUTH))
		 ((= next-dir EAST) (kern-obj-set-facing kobj SOUTHEAST))
		 (else (kern-obj-set-facing kobj prev-dir))))
	  (else
	   (cond ((= next-dir WEST) (kern-obj-set-facing kobj EAST))
		 ((= next-dir NORTH) (kern-obj-set-facing kobj SOUTH))
		 ((= next-dir EAST) (kern-obj-set-facing kobj WEST))
		 (else (kern-obj-set-facing kobj NORTH)))))))

;; When the head moves, have all the segments follow. This is called before the
;; head moves. 'newx' and 'newy' tell where it will move to.
(define (serpentine-on-exec fxgob khead kplace newx newy)
  (define (follow head-coords my-newloc ksegment)
    (if (null? ksegment)
	nil
	(let ((gg (gob ksegment))
	      (my-oldloc (kern-obj-get-location ksegment)))
	  (kern-obj-relocate ksegment my-newloc nil)
	  (tbl-set! gg 'prev head-coords)
	  (tbl-set! gg 'next (follow (loc-coords my-newloc)
				     my-oldloc
				     (get-segment-at kplace
						     (tbl-get gg 'next))))
	  (set-facing ksegment (loc-x my-newloc) (loc-y my-newloc)
		      head-coords
		      (tbl-get gg 'next))
	  (loc-coords my-newloc)
	  )))
  (let* ((head-gob (gob khead))
	 (next (follow (list newx newy)
		       (kern-obj-get-location khead)
		       (get-segment-at kplace (tbl-get head-gob 'next))))
	 )
    (tbl-set! head-gob 'next next)
    (set-facing khead newx newy nil next)
    ))

(kern-mk-effect
 'ef_move_serpentine ;; tag
 "move serpentine"        ;; name (unused in this case)
 nil                 ;; sprite
 'serpentine-on-exec  ;; exec
 nil                 ;; on-apply
 nil                 ;; on-remove
 nil                 ;; restart
 move-done-hook      ;; hook
 0                   ;; detection difficulty class
 #f                  ;; is-cumulative?
 -1                  ;; duration (turns, -1 for infinite)
 )

;; Generate segments behind the head. 'args' should be a list:
;;   (npc-type-tag num-segments)
(define (serpentine-make-body khead args)
  (define (place-segment segment curloc)
    (define (choose-good-tile tiles)
      (if (null? tiles)
	  nil
	  (if (is-good-loc? segment (car tiles))
	      (car tiles)
	      (choose-good-tile (cdr tiles)))))
    (let* ((tiles (get-4-neighboring-tiles curloc))
	   (newloc (choose-good-tile tiles)))
      (if (not (null? newloc))
	  (kern-obj-put-at segment newloc))
      newloc))
  (define (spawn-segments prevloc n)
    (cond ((= n 0) nil)
	  (else
	   (let* ((ksegment (spawn-npc (if (= n 1) (cadr args) (car args))
				       (kern-char-get-level khead)))
		  (loc (place-segment ksegment prevloc)))
	     (cond ((null? loc) nil)
		   (else
		    (let ((next (spawn-segments loc (- n 1)))
			  (prev (loc-coords prevloc)))
		      (bind ksegment (tbl-build 'prev prev
						'next next
						'is-segment #t))
		      (set-facing ksegment (loc-x loc) (loc-y loc) prev next)
		      (loc-coords loc))))))))
  (let* ((headloc (kern-obj-get-location khead))
	 (next (spawn-segments headloc (* 4 (kern-char-get-level khead)))))
    (bind khead (tbl-build 'next next
			   'prev nil
			   'is-segment #t))
    (set-facing khead (loc-x headloc) (loc-y headloc) nil next)
  ))
