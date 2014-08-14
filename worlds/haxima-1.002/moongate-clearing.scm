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
(load "moongate-clearing-map.scm")
(kern-mk-place 'p_moongate_clearing "The Shrine of the Black Gate"
  s_shrine ;; sprite
  m_moongate_clearing
  #f #f #f #f
  ;; subplaces
  nil
  ;; neighbors
  nil

  ;; *** contents of the place ***
  (list
   (list (kern-tag 'black-gate (mk-moongate nil)) 15 15)
   (list (mk-gregor) 1 23)
   (list supplies_chest   1 24)
   (put (mk-stone-lantern) 13 9)
   )

  nil ;; hooks
  nil ;; edge entrances
) ;; end of place p_moongate_clearing

(mk-place-music p_moongate_clearing 'ml-small-town)
