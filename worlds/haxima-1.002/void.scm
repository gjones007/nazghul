
(load "void-map.scm")

(kern-mk-place 
 'p_void
 "The Void"
 nil          ; sprite 
 m_void      ; map
 #f  ;; wraps
 #f  ;; underground
 #t  ;; wilderness
 #f  ;; tmp combat place

 ;; subplaces
 (list
  (list p_engineers_hut     73  72)
  (list p_void_temple       11  14)
  (list p_ancient_derelict  54  98)
  (list p_ankh_shrine       111 113)
  )

 nil ; neighbors

 ;; objects:
 (list

  ;; moongates
  (list (kern-tag 'mg-8 (mk-moongate 'ord)) 93 70)

  ;; npc party generator
  (put (mk-edge-spawn-generator) 0 0)
  ) ;; end of objects

 nil ; hooks
 nil ; edge entrances
 )

(kern-obj-put-at (mk-world-musicdata 'ml-travelling) (list p_world 0 0))
(kern-place-add-on-entry-hook p_world 'music-on-combat-entry)
