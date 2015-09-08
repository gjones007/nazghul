;; Defines the regions of the world map.

(kern-load "regions.scm")

(define regions
  (list

   (region-make "in" "The Fire Sea" (list 448 0 128 128) nil nil)

   (region-make "on" "The North Coast"        (list 256 64 128 64) nil nil)
   (region-make "among" "The Boatgrave Isles" (list 384 64 64 64) nil nil)

   (region-make "in" "Breakwater"             (list 64 128 64 64) nil nil)
   (region-make "in" "The Minstrel Mountains" (list 192 128 64 64) nil nil)
   (region-make "on" "The Gint Road"          (list 256 128 64 64) nil nil)
   (region-make "in" "The Magus Mountains"    (list 320 128 64 64) nil nil)

   (region-make "in" "The West Fens" (list 64 192 64 64) nil nil)

   (region-make "in" "The North Fens" (list 128 192 64 64)
		"The Fens are a vast, swampy region in the northwest."
		(list "The bogs can poison an entire party."
		      "A nasty, evil, foul-smelling place."
		      "The way through the Fens is a twisty maze of passages."
		      ))

   (region-make "in" "The Ismere Valley" (list 192 192 64 64)
		"The Ismere Valley is a hilly valley between the Upper Lake Region and the Fens."
		(list "Learned folk say it was carved by a glacier long ago."
		      "You might encounter trogs there."
		      "The Paladins patrol it."
		      "The River Ismere makes passage difficult."
		      "The source of the River Ismere is supposed to be an icy lake somewhere in the mountains."))

   (region-make "in" "The Fenmere" (list 296 192 64 64) nil nil)

   (region-make "by" "Lake Broadbrim" (list 320 192 64 64)
		nil
		(list "Isinford bridges the Broadbrim River."
		      "The Broadbrim river flows east out of Broadbrim Lake."
		      "Broadbrim Lake is the largest body of freshwater anywhere."
		      "I hear fishing is good in Broadbrim lake."
		      "The small lake north of Broadbrim is Genamere."
		      ))

   (region-make "by" "The Broadbrim Delta" (list 384 192 64 64)
		nil
		(list "The Broadbrim River flows into the sea here."
		      ))

   (region-make "on" "The Western Coast"       (list 64 256 64 128) nil nil)
   (region-make "by" "The Sylumere "           (list 128 256 64 64) nil nil)
   (region-make "in" "Midgate Passage"         (list 192 256 64 64) nil nil)
   (region-make "in" "North Barrier Mountains" (list 256 256 64 64) nil nil)
   (region-make "in" "Brundegart Pass"         (list 320 256 64 64) nil nil)
   (region-make "in" "Brundewatch Mountains"   (list 384 256 64 64) nil nil)

   (region-make "in" "The Cloudhigh Mountains" (list 128 320 64 64) nil nil)
   (region-make "on" "The High Steppes"        (list 192 320 64 64) nil nil)
   (region-make "in" "Clovismarch"             (list 256 320 64 64) nil nil)
   (region-make "in" "The Great Forest"        (list 320 320 128 128)
		"The Great Forest is a vast primordial wood east of the Barrier Mountains"
		(list  "Goblins still thrive there."
		       "They say bandits and foul creatures abound in the deep forest."
		 ))

   (region-make "in" "Fogbottom"             (list 128 384 64 64) nil nil)
   (region-make "in" "The Quarry Hills"      (list 192 384 64 64) nil nil)
   (region-make "in" "The Long River Valley" (list 256 384 64 64) nil nil)

   (region-make "by" "Deepwater Bay"         (list 192 448 64 64) nil nil)
   (region-make "by" "Barrier Cliffs"        (list 320 448 64 64) nil nil)

   (region-make "on" "The Ur Isles"          (list 0 512 64 64) nil nil)
   (region-make "on" "Crescent Isle"         (list 448 512 64 64) nil nil)
   ))

(define (get-region-by-loc loc)
  (first (lambda (r) (region-contains? r loc))
	 regions))

(define (get-region-by-name name)
  (first (lambda (r) (equal? (region-name r) name))
	 regions))
