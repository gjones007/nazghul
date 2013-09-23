(kern-load "gregor.scm")

;;-----------------------------------------------------------------------------
;; Make some chests containing items to get the player started. 
;; These will be placed on the map in the shrine room in the SW.
;;-----------------------------------------------------------------------------
(define supplies_chest
  (mk-chest
   nil ;; trap

   '(
    
    ;; Food
    (10 t_food)

    ;; Gold
    (10 t_gold_coins)

    ;; Reagents
    (10 sulphorous_ash)
    (10 ginseng)
    (10 garlic)
    (10 spider_silk)
    (6 blood_moss)
    (6 black_pearl)
    (3 nightshade)
    (3 mandrake)
    
    ;; Items
    (2 t_heal_potion)
    (2 t_cure_potion)
    (2 t_mana_potion)
    (6 t_torch)
    (3 t_picklock)
    
    ;; Arms
    (1  t_sword)
    (1  t_shield)
    (1  t_staff)

    (1  t_sling)
    (1  t_self_bow)
    (20 t_arrow)

    ;; Hints/instructions
    (1 t_manual)
    (1 t_letter_from_enchanter)
    (1 t_spell_book_white_magick_1   )
    (1 t_spell_book_force_magick_12 )
    )
   ))


;;----------------------------------------------------------------------------
;; Moongate Clearing
;;
;; This is where the player starts out.
;;----------------------------------------------------------------------------
(kern-mk-place 'p_moongate_clearing "Moongate Clearing"
  s_shrine ;; sprite
  (kern-mk-map
   'm_moongate_shrine 19 30 pal_expanded
   (list
    "rn rn rn rn rn rn rn xx ,, ,, ,, xx rn rn rn rn rn rn rn "
    "rn rn rn rn rn rn rn xx ,, ,, ,, xx rn rn rn rn rn rn rn "
    "rn rn rn rn xx xx xx xx ,, ,, ,, xx xx xx xx rn rn rn rn "
    "rn rn rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
    "rn rn rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
    "rn rn rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
    "rn rn rn rn xx xx xx xx ,, ,, ,, xx xx xx xx rn rn rn rn "
    "rn rn rn rn rn rc t7 xx ,, ,, ,, xx t3 t5 r2 rn rn rn rn "
    "rn rn rn rn rc t3 tt xx ,, ,, ,, xx tt tt ra rn rn rn rn "
    "rn rn rn r4 t3 tt tt xx xx ,, xx xx tt tt t5 r2 rn rn rn "
    "rn rn rn rc tt tt tt tt t5 .. t3 tt tt tt tt ra rn rn rn "
    "rn rn rc |# || tt tt bb tt .. tt bb tt tt || |% ra rn rn "
    "rn r4 |# || || || tt tt tt .. tt tt tt || || || |% r2 rn "
    "rn r4 || || || || tt tt tt .. tt tt tt || || || || r2 rn "
    "rn r4 || || || tt tt bb tt .. tt bb tt tt || || || r2 rn "
    "rn rc || || tt tt tt tt tc .. ta tt tt tt tt || || ra rn "
    "r4 |# || || tt bb tt tc t# .. t% ta tt bb tt || || |% r2 "
    "r4 || || tt tt tt tt t# .. .. .. t% tt tt tt tt || || r2 "
    "r4 || || tt tt tt tt .. .. dd .. .. tt tt tt tt || || r2 "
    "r4 || || tt tt tt tt tA .. .. .. tC tt tt tt tt || || r2 "
    "r4 |A || || tt bb tt t5 tA .. tC t3 tt bb tt || || |C r2 "
    "rn r5 || || tt tt tt tt tt tt tt tt tt tt tt || || r3 rn "
    "rn r4 || || || tt tt bb tt tt tt bb tt tt || || || r2 rn "
    "rn r4 || || || || tt tt tt tt tt tt tt || || || || r2 rn "
    "rn r4 |A || || || || || tt tt tt || || || || || |C r2 rn "
    "rn rn r5 |A || || || || || || || || || || || |C r3 rn rn "
    "rn rn rn r1 r5 |A || || || || || || || |C r3 r1 rn rn rn "
    "rn rn rn rn rn r1 r5 |A || || || |C r3 r1 rn rn rn rn rn "
    "rn rn rn rn rn rn rn r5 |A || |C r3 rn rn rn rn rn rn rn "
    "rn rn rn rn rn rn rn rn r9 r9 r9 rn rn rn rn rn rn rn rn "
    ))
  #f #f #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil

  ;; *** contents of the place ***
  (list
   (list (kern-tag 'black-gate (mk-moongate nil)) 11 11)
   (list (mk-gregor) 1 23)
   (list supplies_chest   1 24)
   )

  nil ;; hooks
  nil ;; edge entrances
) ;; end of place p_moongate_clearing

(mk-place-music p_moongate_clearing 'ml-small-town)
