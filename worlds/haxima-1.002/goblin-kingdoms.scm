(mk-dungeon-room
 'p_kurpolis_entrance "Entrance to Kurpolis"
 (list
      "rr rr rr rr xx xx x! xx xx && xx xx x! xx xx rr rr rr rr "
      "rr .. .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx ,, ,, ,, ,, ,, ,, [[ @@ @@ @@ ]] ,, ,, x! rr rr rr rr "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx ,, ,, ,, xx xx xx xx xx x! xx xx xx xx xx rr bb ,, rr "
      "xx ,, ,, ,, xx xx .K .U .R .P .O .L .I .S xx bb bb bb ,, "
      "xx ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, ,, x! bb ,, bb ,, "
      "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ bb ,, ,, ,, "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb ,, "
      "xx ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ bb bb bb ,, "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, x! bb ,, bb rr "
      "xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx ,, bb rr rr "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
  )
 (put (mk-ladder-up 'p_shard 39 75) 9 10)
 (put (mk-door) 4 15)
 (put (mk-door) 4 3)
 (put (mk-door) 4 1)
 (put (mk-windowed-door) 14 10)
 (put (mk-chest nil (list (list 10 t_food))) 1 1)
 (put (mk-bed) 5 17)
 (put (mk-bed) 7 17)
 (put (mk-bed) 9 17)
 (put (mk-bed) 11 17)
 (put (mk-bed) 13 17)

 (put (spawn-pt 'cave-goblin-slinger) 18 7)
 (put (spawn-pt 'cave-goblin-slinger) 18 11)
 (put (spawn-pt 'cave-goblin-berserker) 15 9)

 (put (guard-pt 'crossbowman) 13 9)
 (put (guard-pt 'crossbowman) 13 11)
 (put (guard-pt 'halberdier) 11 10)

 )

(mk-dungeon-room
 'p_goblin_crossroads "Goblin Crossroads"
 (list
  "rr rr rr rr rr rr rr {{ {{ ,, ,, {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr {{ ,, {{ {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr rr rr rr rr rr {{ {{ ,, .. {{ rr {{ {{ {{ rr rr "
  "rr rr rr rr rr rr rr rr rr {{ ,, bb .. {{ {{ {{ {{ rr rr "
  "rr rr rr rr rr rr rr rr rr ,, {{ .. .. .. .. {{ {{ rr rr "
  "rr rr rr rr rr rr rr rr rr rr {{ {{ {{ .. bb .. {{ {{ rr "
  "rr rr rr rr rr rr rr rr rr rr bb {{ {{ {{ .. {{ {{ {{ rr "
  "{{ {{ {{ rr rr rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ "
  ",, {{ {{ .. bb rr rr rr rr rr rr {{ {{ {{ {{ {{ ,, {{ ,, "
  ",, {{ ,, {{ .. {{ rr rr rr rr rr {{ {{ ,, ,, ,, {{ ,, ,, "
  ",, ,, ,, .. ,, ,, {{ rr rr rr {{ {{ {{ {{ ,, ,, ,, {{ ,, "
  "{{ {{ {{ .. bb .. .. {{ {{ {{ {{ {{ .. {{ {{ {{ {{ {{ {{ "
  "rr {{ {{ {{ .. .. bb .. {{ {{ {{ .. bb .. {{ {{ {{ rr rr "
  "rr rr {{ rr rr {{ .. .. .. {{ {{ .. .. {{ {{ {{ rr rr rr "
  "rr rr rr rr {{ {{ {{ .. .. .. .. .. {{ {{ {{ {{ rr rr rr "
  "rr rr rr rr {{ {{ {{ {{ {{ .. bb .. {{ {{ rr rr rr rr rr "
  "rr rr rr rr rr {{ {{ {{ {{ {{ .. {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr rr rr rr rr rr {{ {{ {{ rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (spawn-pt 'cave-goblin-slinger) 14 11)
 (put (spawn-pt 'cave-goblin-berserker) 15 9)

 (put (spawn-pt 'forest-goblin-hunter) 15 5)
 (put (spawn-pt 'forest-goblin-hunter) 12 7)
 ;;(put (spawn-pt 'forest-goblin-stalker) 13 7)
 
 )

(mk-dungeon-room
 'p_grey_goblin_village "Grey Goblin Village"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr {{ {{ {{ rr {{ {{ {{ {{ {{ {{ {{ rr rr rr {{ {{ {{ rr "
  "rr {{ .. {{ rr {{ ,, ,, ,, ,, {{ {{ {{ rr {{ ,, ,, {{ rr "
  "rr {{ ,, ,, {{ ,, ,, ,, && ,, ,, bb {{ {{ {{ ,, ,, {{ rr "
  "rr {{ .. ,, .. .. ,, ,, ,, ,, ,, ,, .. .. .. ,, ,, {{ rr "
  "rr {{ {{ {{ {{ {{ .. .. ,, ,, {{ {{ ,, {{ {{ ,, ,, {{ rr "
  "rr rr rr rr rr {{ {{ {{ .. {{ bb {{ ,, rr {{ .. ,, {{ rr "
  "{{ {{ {{ {{ bb {{ bb {{ .. .. {{ ,, ,, rr {{ .. ,, {{ rr "
  "{{ {{ {{ {{ bb {{ {{ .. ,, ,, ,, ,, {{ rr rr {{ .. {{ rr "
  ",, {{ ,, .. .. .. .. ,, .. ,, ,, ,, {{ {{ rr rr {{ {{ rr "
  ",, ,, .. {{ bb {{ .. ,, ,, ,, ,, .. {{ {{ {{ rr {{ {{ rr "
  "{{ {{ {{ {{ bb {{ {{ ,, ,, ,, ,, ,, .. .. {{ rr rr rr rr "
  "rr rr rr rr rr {{ bb {{ ,, ,, ,, {{ ,, .. .. ~~ ~~ rr rr "
  "rr rr rr rr rr {{ {{ {{ {{ ,, {{ {{ {{ .. ~~ -- -- ~~ rr "
  "rr rr rr rr rr rr rr bb bb .. bb bb rr {{ ~~ -- -- ~~ rr "
  "rr rr rr rr rr rr rr {{ {{ .. {{ {{ rr {{ {{ ~~ ~~ {{ rr "
  "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr {{ {{ {{ {{ rr "
  "rr rr rr rr rr rr rr {{ {{ {{ .. {{ rr rr {{ {{ {{ rr rr "
  "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr rr rr rr rr rr "
  )
 (put (spawn-pt 'cave-goblin-slinger) 8 4)
 (put (spawn-pt 'cave-goblin-berserker) 9 9)
 (put (spawn-pt 'cave-goblin-priest) 9 10)
 )

(mk-dungeon-room
 'p_trolls_den "Troll's Den"
 (list
  "rr rr rr rr rr rr rr {{ .. ,, ,, {{ rr rr rr rr rr rr rr "
  "rr {{ {{ {{ rr rr rr {{ .. ,, {{ {{ rr rr rr rr rr rr rr "
  "rr {{ {{ {{ {{ rr rr {{ .. .. ,, {{ rr rr rr rr rr rr rr "
  "rr {{ {{ {{ {{ {{ rr rr {{ .. ,, bb rr rr rr rr rr rr rr "
  "rr rr {{ {{ rr {{ {{ rr {{ ,, .. {{ rr rr rr rr rr rr rr "
  "rr rr {{ rr rr rr {{ {{ {{ .. {{ {{ rr rr rr rr rr rr rr "
  "rr rr {{ {{ rr {{ {{ rr {{ .. .. {{ rr {{ {{ rr rr rr rr "
  "rr {{ {{ {{ .. {{ rr rr rr {{ .. .. .. .. {{ {{ {{ rr rr "
  "rr {{ {{ .. bb .. {{ rr {{ .. .. rr {{ .. .. {{ .. .. rr "
  "rr {{ {{ {{ .. {{ {{ {{ {{ .. rr rr rr {{ .. .. .. .. rr "
  "rr rr {{ {{ {{ {{ rr {{ .. .. {{ rr {{ {{ .. .. .. && rr "
  "rr rr rr {{ {{ rr rr rr .. {{ {{ {{ .. .. .. .. .. .. rr "
  "rr rr rr {{ .. {{ rr {{ .. rr rr .. bb .. .. {{ .. .. rr "
  "rr rr {{ bb .. .. {{ .. .. rr rr rr .. {{ {{ {{ {{ rr rr "
  "rr {{ .. .. .. .. .. .. {{ {{ rr rr rr rr {{ {{ {{ rr rr "
  "rr {{ .. .. .. bb {{ {{ {{ {{ rr rr rr rr rr rr rr rr rr "
  "rr {{ {{ .. .. bb {{ {{ rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr {{ {{ {{ rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-mongen2 900 3 'is-troll? 'mk-troll nil) 16 10)
 )

(mk-dungeon-room
 'p_shamans_grove "Shaman's Grove"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr tt bb tt rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr tt tt tt tt tt rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr tt bb tt .. tt bb tt rr rr rr rr rr rr "
  "rr rr rr rr rr rr tt tt .. aa .. tt tt rr rr rr rr rr rr "
  "rr rr rr rr rr rr tt tt tt .. tt tt tt rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr tt bb tt bb tt rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr tt tt tt rr rr rr rr rr rr rr rr "
  "rr rr rr tt tt tt rr rr || || || rr rr || || || rr rr rr "
  "rr rr tt tt .. tt tt rr || || || rr || || || || || rr rr "
  "rr tt tt .. .. .. tt bb || || || || || || || || || || rr "
  "rr tt .. .. && .. .. tt || || || || || || tt || || || rr "
  "rr tt tt .. .. .. tt bb || || || || || || || || || || rr "
  "rr rr tt tt .. tt tt rr || || || rr || || || || || rr rr "
  "rr rr rr tt tt tt rr rr || || || rr rr || || || rr rr rr "
  "rr rr rr rr rr rr rr rr || tt || rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr tt tt tt rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr tt tt tt rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-down 'p_dank_cave 9 1) 17 11)

 (put (spawn-pt 'forest-goblin-shaman) 9 3)
 (put (spawn-pt 'forest-goblin-shaman) 10 4)
 (put (spawn-pt 'forest-goblin-shaman) 8 4)
 (put (spawn-pt 'forest-goblin-shaman) 9 5)
 (put (spawn-pt 'forest-goblin-hunter) 3 11)
 (put (spawn-pt 'forest-goblin-hunter) 4 10)
 (put (spawn-pt 'forest-goblin-stalker) 5 11)
 (put (spawn-pt 'forest-goblin-stalker) 4 12)
 )

(mk-dungeon-level 
 (list nil                 p_shamans_grove     nil                  )
 (list p_kurpolis_entrance p_goblin_crossroads p_grey_goblin_village)
 (list nil                 nil                 p_trolls_den         )
 )
