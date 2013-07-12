
(load "hring-map.scm")
(load "test-place.scm")
(kern-load "raise-merciful-death.scm")

(kern-mk-place 
 'p_shard
 "The Shard Surface"
 nil          ; sprite 
 m_hring      ; map
 #f  ;; wraps
 #f  ;; underground
 #t  ;; wilderness
 #f  ;; tmp combat place

 ;; subplaces:
 (list
  (list p_moongate_clearing 336 389)
  (list p_test_place 337 390)
  (list p_gregors_hut       61  67)
  (list p_abandoned_farm    43  51)
  (list p_trigrave          29  51)
  (list p_enchanters_tower  29  21)
  (list p_green_tower       80  59)
  (list p_bole              86  40)
  (list p_glasdrin          82  18)
  (list p_oparine           12  78)
  (list p_absalot           120  4)
  (list p_engineers_hut     50   4)
  (list p_void_temple        5  46)
  (list p_poor_house        19  65)
  (list p_ankh_shrine       97   4)
  (list p_westpass          45  61)
  (list p_eastpass          43  61)
  (list p_ancient_derelict  43  17)
  (list p_gate_to_absalot  120  10)
  (list p_tower_of_absalot 120   8)
  (list p_kun               60  38)
  (list p_lost_halls_entrance (loc-x lost-halls-loc) (loc-y lost-halls-loc))
  (list p_voidgap_entrance   7  75)
  (list p_voidgap_exit       4  77)
  )


 nil ; neighbors

 ;; objects:
 (list

  ;; moongates
  (list (kern-tag 'mg-1 (mk-moongate 'ord)) 62 50) ;; near green tower
  (list (kern-tag 'mg-2 (mk-moongate 'ord)) 34 56) ;; near trigrave
  (list (kern-tag 'mg-3 (mk-moongate 'ord)) 19 82) ;; near oparine
  (list (kern-tag 'mg-8 (mk-moongate 'ord)) 21 19) ;; near enchanter's tower
  (list (kern-tag 'mg-5 (mk-moongate 'ord)) 56 27) ;; near kurpolis
  (list (kern-tag 'mg-6 (mk-moongate 'ord)) 90 26) ;; near lost halls & the man
  (list (kern-tag 'mg-7 (mk-moongate 'ord)) 125 18) ;; near absalot
  (list (kern-tag 'mg-4 (mk-moongate 'ord)) 50 7) ;; engineer's hut

  ;; npc party generator
  (put (mk-edge-spawn-generator) 0 0)

  ;; dungeons
  (put (mk-dungeon 'p_kurpolis_entrance 9 10) 53 18)
  (put (mk-dungeon 'p_mushroom_cave 7 12) 78 74)
  (put (mk-dungeon 'p_necromancers_lair 9 9) 40 70)
  (put (mk-dungeon 'p_smoldering_cave 9 9) 118 46)
  (put (mk-dungeon 'p_slimy_cavern 8 30) 13 8)
  
  

  ;; wreck of the Merciful Death
  (put (mk-raise-listener 'raise-merciful-death nil) 
       merciful-death-x 
       merciful-death-y)

  ;; Bandit Hideout
  (put (mk-step-trig 'mk-bandit-hideout nil)
       (loc-x bandit-hideout-loc)
       (loc-y bandit-hideout-loc))

  ;; Angriss's Lair
  (put (mk-step-trig 'mk-angriss-lair nil)
       (loc-x angriss-lair-loc)
       (loc-y angriss-lair-loc))

 ;; Brundegardt
  (put (mk-step-trig 'mk-brundegardt nil)
       (loc-x brundegardt-loc)
       (loc-y brundegardt-loc))

  ;; The MAN's hideout
  (put (mk-step-trig 'mk-mans-hideout nil)
       (loc-x the-mans-hideout-loc)
       (loc-y the-mans-hideout-loc))



  ) ;; end of objects

 nil ; hooks
 nil ; edge entrances
 )

(kern-obj-put-at (mk-world-musicdata 'ml-travelling) (list p_shard 0 0))
(kern-place-add-on-entry-hook p_shard 'music-on-combat-entry)