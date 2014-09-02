;; Regions of the world map.

(define (region-make prep name area desc facts)
  (tbl-build
   'type '*region*
   'name name
   'area area
   'preposition prep
   'description desc
   'facts facts
   ))

(define (region-name region) (tbl-get region 'name))
(define (region-area region) (tbl-get region 'area))
(define (region-preposition region) (tbl-get region 'preposition))
(define (region-description region) (tbl-get region 'description))
(define (region-facts region) (tbl-get region 'facts))
(define (region-contains? region loc)
  (loc-in-rect? loc (region-area region)))
