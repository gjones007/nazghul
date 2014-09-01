;; Defines the regions of the world map.

(kern-load "regions.scm")

(define regions
  (list
   (region-make "in" "The Great Forest" (list 332 318 97 132))
   (region-make "in" "The Long River Valley" (list 254 354 46 126))
   (region-make "by" "Deepwater Bay" (list 228 444 25 36))
   (region-make "among" "The Barrier Mountains" (list 300 354 31 124))
   (region-make "in" "The Upper Lake Region" (list 256 192 64 64))
   (region-make "in" "The Glacier Valley Region" (list 192 192 64 64))
   ))

(define (get-region loc)
  (first (lambda (r) (region-contains? r loc))
	 regions))
