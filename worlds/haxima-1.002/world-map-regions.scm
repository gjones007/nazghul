;; Defines the regions of the world map.

(kern-load "regions.scm")

(define regions
  (list
   (region-make "in" "The Great Forest" (list 332 318 97 132) nil nil)
   (region-make "in" "The Long River Valley" (list 254 354 46 126) nil nil)
   (region-make "by" "Deepwater Bay" (list 228 444 25 36) nil nil)
   (region-make "among" "The Barrier Mountains" (list 300 354 31 124) nil nil)
   (region-make "in" "The Upper Lake Region" (list 256 192 64 64) nil nil)
   (region-make "in" "The Ismere Valley" (list 192 192 64 64) 
		"The Ismere Valley is a hilly valley between the Upper Lake Region and the Fens."
		(list "Learned folk say it was carved by a glacier long ago."
		      "You might encounter trogs there."
		      "The Paladins patrol it."
		      "The River Ismere makes passage difficult."
		      "The source of the River Ismere is supposed to be an icy lake somewhere in the mountains."))
   (region-make "in" "The North Fens" (list 128 192 64 64)
		"The Fens are a vast, swampy region in the northwest."
		(list "The bogs can poison an entire party."
		      "A nasty, evil, foul-smelling place."
		      "The way through the Fens is a twisty maze of passages."
		      ))
   (region-make "in" "The West Fens" (list 64 192 64 64) nil nil)
   (region-make "by" "The Sylumere " (list 64 192 64 64) nil nil) ;; Bog-lake
   ))

(define (get-region-by-loc loc)
  (first (lambda (r) (region-contains? r loc))
	 regions))

(define (get-region-by-name name)
  (first (lambda (r) (equal? (region-name r) name))
	 regions))
