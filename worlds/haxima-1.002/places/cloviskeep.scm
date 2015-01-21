(kern-mk-place
 'p_cloviskeep
 "Cloviskeep"
 s_castle
 m_cloviskeep
 #f #f #f #f
 nil  ; subplaces
 nil  ; neighbots

 ;; *** contents ***
 (list
  (put (mk-monman) 0 0)
  (put (spawn-pt2 'wyrm 'serpentine-make-body 'wyrm-segment 6) 22 53)
  )

 ;; *** on-entry hooks ***
 (list
  'on-entry-to-dungeon-room)

 ;; *** entrances ***
 (list
  )
)

(mk-place-music p_cloviskeep 'ml-peaceful-area)
