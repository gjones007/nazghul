(kern-load "gregor.scm")
(kern-load "moongate-clearing-mech.scm")

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

(kcontainer-add-listener supplies_chest 'on-open-supplies)

;;----------------------------------------------------------------------------
;; Moongate Clearing
;;
;; This is where the player starts out.
;;----------------------------------------------------------------------------
(kern-mk-place 'p_moongate_clearing "Moongate Clearing"
  s_shrine ;; sprite
  (kern-mk-map 'm_moongate_clearing 23 28 pal_expanded
    (list
		"rr rr rr rr rr rr rr rr rr t3 tt tt tt td rr rr rr rr rr rr rr rr rr "
		"rr {{ {{ {{ rr rr rr rr t3 tt tt || || rr rr rr {{ {{ {{ rr {{ {{ rr "
		"rr {{ {{ {{ {{ rr t3 tt tt || tt tt || |% rr rr {C t7 {{ rr rr {{ rr "
		"rr {{ {{ {{ {{ {C tt || || || || tt || || |% rr t3 tt {A {{ rr {{ rr "
		"rr rr {{ {{ {C t3 || || || || tt tt tt || || tt tt tt t5 {A {{ {{ rr "
		"rr rr rr rr t3 tt || || || tt tt tt tt tt tt tt tt tt tt td {{ rr rr "
		"rr rr rr rr || || || || tt tt tc bb ta tt tt tt tt tt || rr rr rr rr "
		"rr rr rr rr || || || tt tt bb .. .. .. bb tt tt tt || || |% rr rr |& "
		"rr rr rr rr || || tt tt tt td .. .. .. tb tt tt tt tt || || || || || "
		"rr rr |# || || || tt tt bb .. .. .. .. .. bb tt tt tt tt tt tt tt tt "
		"rr |# || || || || tt tc .. .. .. .. .. .. .. ta tt tt tt tt tt tt tc "
		"{{ |A || || || || tt bb .. .. .. .. .. .. .. bb tt tt tt tt tt tc {& "
		"{{ {% |A || || tt tt t5 .. .. .. .. .. .. .. t3 tt tc {# {% te {# {{ "
		"rr {{ {{ {{ {% ta tt tt bb .. .. .. .. .. bb tt tc {# {{ {{ {{ {{ {{ "
		"rr rr rr {{ {{ {% ta tt tt td .. .. .. tb tt tt {# rr rr {{ {{ rr rr "
		"rr rr rr rr {{ {{ {% ta tt bb .. .. .. bb tt tt {A rr rr rr {{ {{ rr "
		"rr rr .. rr rr rr {{ {% tt td .. .. .. tb tt tt td {{ rr rr {{ {{ rr "
		"rr .. .. .. .. rr {{ {{ tt bb .. .. .. bb tt tt {B {{ {{ {{ {{ rr rr "
		"rr .. .. .. .. rr {{ {{ tt td .. .. .. tb tt tt tt tt t5 {A rr rr rr "
		"rr {a .. .. {c rr {{ {{ tt bb .. .. .. bb te bb te bb tt t5 rr rr rr "
		"rr rr {e rr rr rr {{ {{ tt td .. .. .. .. .. .. .. .. ta tt rr rr rr "
		"rr rr rr rr rr rr rr {{ tt bb .. .. .. .. .. .. .. .. bb tt rr rr rr "
		"rr rr ,H ,A ,I ,L rr rr tt t5 .. .. .. .. .. .. .. .. tb tt rr rr rr "
		"rr .. .. .. .. .. .. rr ta tc bb .. bb t7 bb .. .. .. bb tt t5 rr rr "
		"rr .. .. .. .. .. .. .. .. .. .. tC t3 tt td .. .. .. tb tt tt t5 rr "
		"rr .. .. .. .. .. .. rr t3 || tt tt tt tt bb .. .. .. bb tt tt tt t5 "
		"rr ,S ,E ,E ,K ,E ,R rr || || || tt tt tc .. .. .. .. .. ta tt tt tt "
		"rr rr rr rr rr rr rr rr ta || tt tt tc bb .. .. .. .. .. bb ta tt tc "
     )
    )
  #f #f #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil

  ;; *** contents of the place ***
  (list
   (list (kern-tag 'black-gate (mk-moongate nil)) 11 11)
   (list (mk-gregor) 1 23)
   (list supplies_chest 1 24)
   (put (mk-step-clue "Press 'o' to o)pen chests.") 2 24)
   )

  nil ;; hooks
  (list  ;; edge entrances
   (list north 16 27)
   (list east   0 11)
   (list west  22 10)
   (list northeast 8 27)
   (list southeast   9 0)
   (list southwest  22 7)
   )
) ;; end of place p_moongate_clearing

(mk-place-music p_moongate_clearing 'ml-peaceful-area)
