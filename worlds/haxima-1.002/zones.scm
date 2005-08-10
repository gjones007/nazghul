;; Enchanter's Tower
(define (mk-zone x y w h) (list 'p_enchanters_tower x y w h))
(define enchtwr-campsite         (mk-zone  0  0  5   5))
(define enchtwr-dining-room-1    (mk-zone  0  0  5   5))
(define enchtwr-dining-room      (mk-zone  0  0  5   5))

;; Bole
(define (mk-zone x y w h) (list 'p_bole x y w h))
(define bole-bed-kathryn  (mk-zone 31 18  1  1))
(define bole-bedroom-thud (mk-zone 31 17  2  2))
(define bole-bed-2        (mk-zone 38 18  1  1))
(define bole-bed-3        (mk-zone 31 21  1  1))
(define bole-bed-4        (mk-zone 38 21  1  1))
(define bole-bed-may      (mk-zone 44 17  1  1))
(define bole-bed-melvin   (mk-zone 40 17  1  1))
(define bole-bed-bill     (mk-zone 23 19  1  1))
(define bole-bed-hackle   (mk-zone 5  8   1  1))
(define bole-bedroom-may  (mk-zone 40 18  5  4))
(define bole-bills-hut    (mk-zone 20 15  4  5))
(define bole-courtyard    (mk-zone 24 25  5  5))
(define bole-dining-hall  (mk-zone 31 23  5  7))
(define bole-hackles-hut  (mk-zone 5   8  5  5))
(define bole-hackles-yard (mk-zone 2   3  5 13))
(define bole-kitchen      (mk-zone 39 23  3  7))
(define bole-n-woods      (mk-zone 22  0  8 11))
(define bole-table-1      (mk-zone 32 26  1  1))
(define bole-table-2      (mk-zone 34 26  1  1))
(define bole-table-3      (mk-zone 34 27  1  1))
(define bole-table-4      (mk-zone 32 27  1  1))
