(kern-mk-place 
 'p_shard
 "The Shard Surface"
 nil          ; sprite 
 ;; map:
 (kern-mk-map 
  'm_shard (* 3 19) (* 3 19) pal_expanded
  (list
      ".. .. .. .. .. .. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. %% %% %% -- %% %% %% %% %% %% %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. %% %% -- __ -- %% %% %% %% %% %% %% %% .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
      ".. .. .. .. .. .. .. .. .. %% %% %% -- %% %% %% %% %% %% %% %% .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      ".. .. .. .. .. .. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ "
      ".. .. .. .. .. .. .. .. .. .. .. %% %% %% %% %% %% ~~ %% %% %% %% %% %% .. .. {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {{ ^^ {{ {{ {{ {{ ^^ {{ {{ {{ {{ {{ ^^ ^^ "
      ".. .. .. .. .. .. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% -- %% %% %% .. {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ ^^ "
      ".. .. .. .. .. .. %% %% %% .. %% %% %% %% %% %% %% %% %% %% -- __ -- %% %% .. .. {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ ^^ "
      ".. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% -- %% %% %% %% -- %% %% %% .. .. .. {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ "
      ".. .. .. .. %% %% %% -- %% %% %% %% %% %% %% -- __ -- %% %% %% %% %% %% .. .. .. .. {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ "
      ".. .. .. .. %% %% -- __ -- %% %% %% %% %% %% %% -- %% %% %% %% %% %% .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ "
      ".. .. .. .. %% %% %% -- %% %% %% %% %% %% %% %% %% %% %% .. %% %% %% .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ "
      ".. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ ^^ "
      ".. .. .. .. .. .. %% %% %% %% %% %% %% %% %% %% %% %% .. .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ "
      ".. .. .. .. .. .. .. %% %% %% %% %% ~~ %% %% %% .. .. .. .. .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ {{ ^^ ^^ ^^ "
      "{{ .. .. .. .. .. .. .. %% %% ~~ ~~ ~~ ~~ ~~ %% .. .. .. .. .. .. .. .. .. .. ^^ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "{{ {{ .. .. .. .. .. .. %% ~~ ~~ -- -- -- ~~ ~~ ~~ ~~ ~~ ~~ ~~ ~~ .. .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ {{ {{ {{ .. .. .. .. .. ~~ -- -- __ -- -- ~~ ~~ -- -- -- -- ~~ .. .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ {{ {{ {{ {{ .. .. .. ~~ -- __ __ __ -- ~~ -- -- __ __ -- ~~ ~~ .. .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ {{ {{ {{ {{ {{ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ {{ {{ {{ .. .. ~~ -- -- __ -- -- ~~ -- __ __ __ __ -- ~~ .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ .. .. .. .. .. {{ {{ {{ {{ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ {{ {{ .. ~~ ~~ -- -- -- ~~ ~~ -- __ __ __ __ -- ~~ .. .. ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ {{ {{ .. .. .. .. .. .. .. {{ {{ {{ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ .. .. ~~ ~~ ~~ ~~ ~~ ~~ -- -- __ __ -- -- ~~ .. .. ^^ ^^ {{ tt {{ tt {{ ^^ ^^ ^^ {{ ^^ ^^ {{ {{ {{ .. .. .. .. tt tt .. .. .. .. {{ {{ {{ {{ ^^ ^^ "
      "^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ .. .. .. .. .. .. ~~ ~~ -- -- -- -- ~~ ~~ .. .. ^^ ^^ tt .. .. .. tt ^^ ^^ {{ {{ {{ ^^ {{ {{ .. .. .. .. tt tt tt tt .. .. .. .. {{ {{ {{ ^^ ^^ "
      "^^ {{ {{ .. {{ {{ ^^ ^^ {{ .. .. .. .. .. .. .. ~~ ~~ -- -- ~~ ~~ ~~ .. .. ^^ ^^ {{ .. .. .. {{ ^^ ^^ ^^ {{ {{ {{ {{ .. .. .. tt tt tt tt tt tt tt tt .. .. .. {{ {{ ^^ ^^ "
      "^^ {{ .. .. .. {{ ^^ ^^ {{ .. .. .. .. .. .. .. .. ~~ ~~ ~~ ~~ .. ~~ .. .. .. ^^ tt .. /3 .. tt ^^ ^^ ^^ ^^ {{ {{ .. .. .. tt tt tt tt || || tt tt tt .. .. .. {{ {{ {{ ^^ "
      "^^ {{ {{ .. {{ {{ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. ~~ .. .. .. ^^ {{ tt /7 tt {{ ^^ ^^ ^^ {{ {{ .. .. .. tt tt tt tt tt || || || tt tt tt .. .. .. {{ {{ ^^ "
      "^^ ^^ {{ {{ {{ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. ~~ ~~ .. .. ^^ ^^ ^^ /7 ^^ ^^ ^^ ^^ {{ {{ .. .. .. tt tt tt tt tt tt tt || || || tt tt .. .. .. {{ {{ {{ "
      "^^ ^^ ^^ {{ {{ {{ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ~~ .. .. tt ^^ ^^ /7 ^^ ^^ {{ {{ {{ .. .. .. tt tt tt || || tt tt tt tt || || tt tt tt .. .. .. {{ {{ "
      "^^ ^^ ^^ ^^ {{ {{ {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ~~ ~~ .. .. tt {{ /7 {{ {{ {{ .. .. .. .. .. tt tt tt || || || tt tt tt tt tt tt tt tt tt .. .. {{ {{ "
      "^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ~~ .. .. .. .. /7 .. .. .. .. .. .. tt tt tt tt tt tt || || || tt tt tt tt tt tt tt tt .. .. .. .. "
      "^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. ~~ ~~ ~~ ~~ tt /7 .. .. .. .. || tt tt tt tt tt tt tt tt || || || tt tt || || || tt tt .. .. .. .. "
      "^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. %% ~~ tt /8 /d /d /2 || tt tt || || || tt tt tt || || || || || || || || || tt tt tt tt .. .. "
      "^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt .. tt tt tt tt ~~ || tt tt tt /7 || tt tt tt || || || tt || || || || || || || || || || tt tt tt tt .. .. "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt || || || ~~ || || || tt /7 tt tt || || || || || || || || || || || || || || || || || || tt tt .. .. "
      "^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. tt tt tt || || || tt tt tt || || || ~~ || || || tt /7 tt || || || || || || || || || || || || || || || || || || || tt tt tt .. "
      "^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. tt tt tt || || || || || tt tt tt || || ~~ ~~ || || tt /7 tt || || || || || || || || || || || || || || || || tt || || || tt tt .. "
      "^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. tt tt || || || || || || || tt tt tt || || ~~ || || || /8 /2 tt || || tt || || || || || || || || || || || tt tt tt || || tt tt tt "
      "^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. tt tt || || || || || || || tt tt tt || || ~~ || || || || /7 tt tt tt tt tt || || || || || || || || || || || tt || || || || tt tt "
      "^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. tt tt || || || || || || || tt tt tt || || ~~ || || || || /7 tt || tt tt tt || || || || || || || || || || || || || || || || tt tt "
      "^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. tt tt tt || || || || || tt tt tt tt || || ~~ || || || || /7 || || tt tt tt tt tt tt || || || || || || || || || || || || || || || "
      "^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. tt tt tt || || || tt tt tt tt tt || || ~~ || || || tt /7 tt || || tt tt tt tt tt || || || || || || || || || || || || || || || "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt .. tt tt || || ~~ ~~ ~~ || tt /7 tt tt || || tt tt tt tt tt || || || || || || || || || || || || || || "
      "^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt .. .. tt tt tt || tt tt ~~ tt .. /7 .. tt tt || || tt tt tt tt tt || || || || || || || || || || || || || "
      "^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. /0 /d /d /d /d /d /d /d /d /d /d == /d /d .. /d /d /d /d /d /d /d /d /d /2 || || || || || || || || || || || || || "
      "^^ ^^ ^^ ^^ ^^ {{ {{ {{ .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. tt tt tt tt ~~ tt .. /7 .. tt tt || || || tt tt tt /7 tt || || || || || || || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. tt tt ~~ ~~ ~~ tt tt /7 tt tt || || || || tt tt tt /7 tt tt tt tt || || || || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. tt tt ~~ tt tt tt tt /7 tt || || || || tt tt tt .. /7 tt tt tt tt tt tt || || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt ~~ tt tt tt tt /7 || || || || tt tt tt .. .. /8 /d /d /d /d /d /d /d /d /d /d /d /d /d "
      "{{ {{ ^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt ~~ tt tt tt tt /7 || || || tt tt tt .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt tt "
      ".. {{ {{ {{ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. /7 .. .. .. .. .. .. .. tt ~~ ~~ ~~ ~~ tt /7 || tt tt tt tt .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt tt "
      ".. .. {{ {{ {{ /d /d /d /d /d /d /d /d /d /d /d /d /d /d /a .. .. .. .. .. .. tt tt tt tt tt ~~ tt /8 /d /2 tt tt .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt "
      ".. .. {{ {{ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt .. tt tt ~~ tt tt tt /7 tt .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt tt tt "
      ".. .. .. {{ ^^ ^^ {{ {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt ~~ ~~ ~~ tt /7 .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt tt tt || "
      ".. .. .. ^^ ^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt ~~ tt /7 .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt tt tt || || || "
      ".. .. .. ^^ ^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt ~~ tt /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt || || || || "
      ".. .. .. ^^ ^^ ^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt ~~ tt /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt || || || "
      ".. .. .. ^^ ^^ ^^ ^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt ~~ tt /7 .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. tt tt tt || "
   )
  )

 #f  ;; wraps
 #f  ;; underground
 #t  ;; wilderness
 #f  ;; tmp combat place

 ;; subplaces:
 (list
  (list p_moongate_clearing 29 23)
  (list p_gregors_hut       40 30)
  (list p_abandoned_farm    50 36)
  (list p_trigrave          33 43)
  (list p_enchanters_tower  19 13)
  )

 ;; (list p_wivernscross       4 50)

 nil ; neighbors

 ;; objects:
 (list

  ;; dungeons
  (list (mk-mine-entrance 'p_lost_halls 1 4) 46 12)

  ;; terrain features
  ;;(list (mk-bridge east) 30 43)

  ;; existing npc parties
  (list (kern-mk-party t_bandit_gang faction-monster nil) 29 28)

  ;; monster/ambush generators
  (list (mk-generator t_orc_generator) 1 22)
  (list (mk-generator t_skeleton_generator) 0 0)
  (list (mk-generator t_bandit_generator) 30 38)
  (list (mk-wilderness-ambush-generator 
         t_queen_spider_generator ;; type
         43 ;; x
         34 ;; y
         5  ;; w
         6  ;; h
         "A nest of spiders!" ;; msg
         )
        46 37)

  )

 nil ; hooks
 nil ; edge entrances
 )
