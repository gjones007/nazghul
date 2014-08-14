(kern-mk-sprite-set 'ss_stone_lantern 32 32 1 2 0 0 "stone-lantern.png")
(kern-mk-sprite 's_stone_lantern_unlit ss_stone_lantern 1 0 #f 0)
(kern-mk-sprite 's_stone_lantern_lit ss_stone_lantern 1 1 #f 0)

(define (stone-lantern-state on?)
  (println "stone-lantern-state:" on?)
  (if on?
      (state-mk 's_stone_lantern_lit #f pclass-boulder 512)
      (state-mk 's_stone_lantern_unlit #f pclass-boulder 0)))

(mk-obj-type
 't_stone_lantern
 "stone lantern"
 nil
 layer-mechanism
 (ifc bim-ifc
      (method 'handle bim-toggle)
      (method 'state stone-lantern-state)))

(define (mk-stone-lantern)
  (bind (kern-mk-obj t_stone_lantern 1)
	(bim-mk #f nil nil)))
				  
