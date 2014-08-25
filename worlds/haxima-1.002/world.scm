
(load "world-map.scm")
(load "test-place.scm")
(kern-load "raise-merciful-death.scm")

(kern-mk-place 
 'p_world
 "The Shard Surface"
 nil          ; sprite 
 m_world      ; map
 #f  ;; wraps
 #f  ;; underground
 #t  ;; wilderness
 #f  ;; tmp combat place

 ;; subplaces:
 (list
  (list p_moongate_clearing 336 389)
  (list p_gregors_hut       338 436)
  (list p_abandoned_farm    284 377)
  (list p_trigrave          271 400)
  (list p_enchanters_tower  143 239)
  (list p_green_tower       380 398)
  (list p_bole              404 304)
  (list p_glasdrin          276 207)
  (list p_oparine           240 455)
  (list p_absalot           367 141)
;;  (list p_engineers_hut     50   4)
;;  (list p_void_temple        5  46)
  (list p_poor_house        240 466)
;;  (list p_ankh_shrine       342 112)
;;  (list p_westpass          312 409)
;;  (list p_eastpass          311 409)
;;  (list p_ancient_derelict  43  17)
  (list p_tower_of_absalot 367 145)
  (list p_gate_to_absalot  367 147)
  (list p_kun               128 337)
  (list p_lost_halls_entrance (loc-x lost-halls-loc) (loc-y lost-halls-loc))
;;  (list p_voidgap_entrance   7  75)
;;  (list p_voidgap_exit       4  77)
  )

 nil ; neighbors

 ;; objects:
 (list

  ;; moongates
  (list (kern-tag 'mg-1 (mk-moongate 'ord)) 366 397) ;; near green tower
  (list (kern-tag 'mg-2 (mk-moongate 'ord)) 262 363) ;; near cloviskeep
  (list (kern-tag 'mg-3 (mk-moongate 'ord)) 223 449) ;; near oparine
  (list (kern-tag 'mg-8 (mk-moongate 'ord)) 149 243) ;; near enchanter's tower
  (list (kern-tag 'mg-5 (mk-moongate 'ord)) 301 182) ;; near glasdrin, tulemane
  (list (kern-tag 'mg-6 (mk-moongate 'ord)) 46 524) ;; circe's lagoon
  (list (kern-tag 'mg-7 (mk-moongate 'ord)) 380 219) ;; near absalot
  (list (kern-tag 'mg-4 (mk-moongate 'ord)) 112 372) ;; southwest planes

  ;; npc party generator
  (put (mk-edge-spawn-generator) 0 0)

  ;; dungeons
  (put (mk-dungeon 'p_kurpolis_entrance 9 10)
       (loc-x kurpolis-loc)
       (loc-y kurpolis-loc))
  (put (mk-dungeon 'p_mushroom_cave 7 12)
       (loc-x mushroom-cave-loc)
       (loc-y mushroom-cave-loc))
  (put (mk-dungeon 'p_necromancers_lair 9 9)
       (loc-x necromancers-lair-loc)
       (loc-y necromancers-lair-loc))
  (put (mk-dungeon 'p_smoldering_cave 9 9)
       (loc-x smoldering-cave-loc)
       (loc-y smoldering-cave-loc))
  (put (mk-dungeon 'p_slimy_cavern 8 30)
       (loc-x slimy-cavern-loc)
       (loc-y slimy-cavern-loc))
  
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

(kern-obj-put-at (mk-world-musicdata 'ml-travelling) (list p_world 0 0))
(kern-place-add-on-entry-hook p_world 'music-on-combat-entry)
