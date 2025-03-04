
(load "world-map.scm")
(kern-load "raise-merciful-death.scm")
(kern-load "world-map-regions.scm")

(kern-mk-place 
 'p_world
 "The Shard Surface"
 nil          ; sprite 
 m_world      ; map
 #f  ;; wraps
 #f  ;; underground
 #t  ;; wilderness
 #f  ;; tmp combat place

 ;; subplaces:g
 (list
  (list p_cloviskeep        275 336)
  (list p_moongate_clearing 336 389)
  (list p_gregors_hut       335 416)
  (list p_abandoned_farm    336 442)
  (list p_trigrave          271 400)
  (list p_green_tower       380 398)
  (list p_bole              403 304)
  (list p_westpass          313 409)
  (list p_eastpass          311 409)
  (list p_glasdrin          276 207)
  (list p_absalot           367 141)
  (list p_enchanters_tower  143 239)
  (list p_tower_of_absalot  367 145)
  (list p_gate_to_absalot   367 147)
  (list p_oparine           240 455)
  (list p_poor_house        262 400)
  (list p_kun               296 288)
  (list p_kurpolis          (loc-x kurpolis-loc) (loc-y kurpolis-loc))

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
  (list (kern-tag 'mg-3 (mk-moongate 'ord)) 149 243)
  (list (kern-tag 'mg-4 (mk-moongate 'ord)) 112 372) ;; southwest planes
  (list (kern-tag 'mg-5 (mk-moongate 'ord)) 301 182) ;; near glasdrin, tulemane
  (list (kern-tag 'mg-6 (mk-moongate 'ord)) 46 524) ;; circe's lagoon
  (list (kern-tag 'mg-7 (mk-moongate 'ord)) 380 219) ;; near absalot

  ;; road signs
  (list (mk-road-sign
	 "Green Tower - East"
	 "Barrier Pass - West"
	 ) 336 395)
  (list (mk-road-sign
	 "Green Tower - East"
	 "Bole - North"
	 ) 354 388)
  (list (mk-road-sign
	 "Green Tower - South"
	 "Bole - North"
	 ) 387 386)
  (list (mk-road-sign
	 "Bole - East"
	 "Brundegardt Pass - North"
	 ) 346 343)
  ;; below Broadbrim Lake
  (list (mk-road-sign
	 "Great Forest - South" 
	 "Glasdrin - West"
	 "Absalot - East"
	 ) 338 264)
  ;; below Isinford
  (list (mk-road-sign
	 "Glasdrin - North"
	 "Absalot - East"
	 "Midgate Passage - West"
	 ) 304 247)
  ;; north of Isinford
  (list (mk-road-sign
	 "Glasdrin - West"
	 ) 305 228)
  ;; Midgate Passage
  (list (mk-road-sign
	 "Glasdrin - East"
	 "Northern Fens - West"
	 "Long River Valley - South"
	 ) 232 307)
  ;; Clovismarch
  (list (mk-road-sign
	 "Cloviskeep - East"
	 "Midgate Passage - North"
	 "Trigrave - South"
	 ) 257 354)
  ;; west of Trigrave
  (list (mk-road-sign
	 "Cloviskeep - North"
	 "Trigrave - East"
	 "Oparine - South"
	 ) 264 401)
  (list (mk-road-sign
	 "East - Trigrave"
	 "South - Oparine"
	 ) 240 446)

  ;; npc party generator
  (put (mk-edge-spawn-generator) 0 0)

  ;; dungeons
;  (put (mk-dungeon 'p_kurpolis_entrance 9 10) (loc-x kurpolis-loc) (loc-y kurpolis-loc))
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
(kern-place-add-on-entry-hook p_world 'music-on-place-entry)
