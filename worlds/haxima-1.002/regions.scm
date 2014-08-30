;; Regions of the world map.

(define (region-make name area)
  (tbl-build
   'type '*region*
   'name name
   'area area))

(define (region-name region) (tbl-get region 'name))
(define (region-area region) (tbl-get region 'area))
(define (region-contains? region loc)
  (println "region-contains?" region "," loc)
  (loc-in-rect? loc (region-area region)))
