;; Regions of the world map.

(define (region-make prep name area)
  (tbl-build
   'type '*region*
   'name name
   'area area
   'preposition prep
   ))

(define (region-name region) (tbl-get region 'name))
(define (region-area region) (tbl-get region 'area))
(define (region-preposition region) (tbl-get region 'preposition))
(define (region-contains? region loc)
  (println "region-contains?" region "," loc)
  (loc-in-rect? loc (region-area region)))
