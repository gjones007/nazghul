;; Defines the regions of the world map.

(kern-load "regions.scm")

(define regions
  (list
   (region-make "The Great Forest" (list 332 318 97 132))
   ))

(define (get-region loc)
  (first (lambda (r) (region-contains? r loc))
	 regions))
