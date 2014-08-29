;;----------------------------------------------------------------------------
;; NPC type constructors
;;----------------------------------------------------------------------------

;;----------------------------------------------------------------------------
;; Local Constants
;;----------------------------------------------------------------------------
(define default-level 1)

;;----------------------------------------------------------------------------
;; Local Procedures
;;----------------------------------------------------------------------------

;; Lookup the value of symbol 'key' in associated list 'alist'. If not found,
;; return 'default' if specified else throw an error. Evaluate symbol values,
;; just return atoms.
(define (get alist key . default)
  (let ((entry (assoc key alist)))
    (if entry 
        (if (symbol? (cdr entry))
            (eval (cdr entry))
            (cdr entry))
        (if (pair? default)
            (car default)
            (error "Missing key:" key)))))

;; Wrapper to kern-mk-char that supports variable args and provides defaults,
;; when possible, where nothing is specified. 'kwargs' should be an associated
;; list of keyword-argument pairs.
(define (mk-char kwargs)
  (define (optarg key default)
    (get kwargs key default))
  (define (arg key)
    (get kwargs key))
  (kern-mk-char (optarg 'tag nil)
                (optarg 'name nil)
                (arg 'species)
                (optarg 'occ nil)
                (arg 'sprite)
                (arg 'faction)
                (optarg 'str 0)
                (optarg 'int 0)
                (optarg 'dex 0)
                (optarg 'hp_mod 0)
                (optarg 'hp_mult 0)
                (optarg 'mp_mod 0)
                (optarg 'mp_mult 0)
                (optarg 'hp 0)
                (optarg 'xp -1)
                (optarg 'mp 0)
                (optarg 'ap 0)
                (optarg 'lvl default-level)
                (optarg 'dead #f)
                (optarg 'conv nil)
                (optarg 'sched nil)
                (eval (optarg 'ai ''std-ai))
                (mk-inventory (filter notnull?
                                      (map (lambda (x) 
                                             (apply roll-to-add x))
                                           (optarg 'stuff nil))))
                (optarg 'arms nil)
                (optarg 'hooks nil)))

;; mk-stock-char -- convenience wrapper for kern-mk-char. Handles the
;; boilerplate associated with first-time "stock" character creations. A stock
;; character is a monster, guard or similar cannon-fodder NPC, with no
;; interesting conversation, no schedule of appointments, etc.
(define (mk-stock-char name species occupation sprite faction ai container 
                       arms conv)
  (kern-mk-char
   nil ;;..........tag
   name ;;.........name
   species ;;.......species
   occupation ;;...occupation
   sprite ;;.......sprite
   faction ;;....;.faction
   0 ;;............custom strength modifier
   0 ;;............custom intelligence modifier
   0 ;;............custom dexterity modifier
   0 ;;............custom base hp modifier
   0 ;;............custom hp multiplier (per-level)
   0 ;;............custom base mp modifier
   0 ;;............custom mp multiplier (per-level)
   (max-hp species occupation default-level 0 0) ;;.current hit points
   -1  ;;...........current experience points
   (max-mp species occupation default-level 0 0) ;;.current magic points
   0  ;; AP_per_turn
   default-level  ;;............current level
   #f ;;...........dead?
   conv ;;.........conversation (optional)
   nil ;;..........schedule (optional)
   ai ;;...........custom ai (optional)
   container ;;....container (and contents)
   arms ;;.........readied arms (in addition to the container contents)
   nil ;;..........hooks in effect
   ))

;; Curried version of mk-stock-char for characters without an occupation, ai,
;; container or armamenets
(define (mk-animal name species sprite)
  (mk-stock-char name species nil sprite faction-none nil nil nil nil))

(define (mk-readied-items . items)
  items)

(define (mk-at-level ctor-tag lvl-dice . args)
  (set-level (apply (eval ctor-tag) args) 
             (kern-dice-roll lvl-dice)))

;; npct -- NPC type
;; (define (mk-npct2 name spec occ spr traps equip eff ai faction conv drop-fx drop-fx-parms)
;;   (list name spec occ spr traps equip eff ai faction conv drop-fx drop-fx-parms))
;; (define (mk-npct name spec occ spr traps equip eff ai faction conv)
;;   (mk-npct2 name spec occ spr traps equip eff ai faction conv nil nil))
;; (define (npct-name npct) (car npct))
;; (define (npct-spec npct) (cadr npct))
;; (define (npct-occ npct) (caddr npct))
;; (define (npct-spr npct) (cadddr npct))
;; (define (npct-traps npct) (list-ref npct 4))
;; (define (npct-eqp npct) (list-ref npct 5))
;; (define (npct-effects npct) (list-ref npct 6))
;; (define (npct-ai npct) (list-ref npct 7))
;; (define (npct-faction npct) (list-ref npct 8))
;; (define (npct-conv npct) (list-ref npct 9))
;; (define (npct-drop-fx npct) (list-ref npct 10))
;; (define (npct-drop-fx-parms npct) (list-ref npct 11))



;; npcg -- generic NPC gob
(define (npcg-mk type) 
  (list 'npcg 
        type 
        #f  ;; taunted
        #f  ;; spawned
        nil ;; post
        nil ;; subgob
        ))
(define (npcg-type npcg) (cadr npcg))
(define (npcg-taunted? npcg) (caddr npcg))
(define (npcg-spawned? npcg) (cadddr npcg))
(define (npcg-is-type? npcg type) (equal? type (npcg-type npcg)))
(define (npcg-set-taunted! npcg val) (set-car! (cddr npcg) val))
(define (npcg-set-spawned! npcg val) (set-car! (cdddr npcg) val))
(define (npcg-set-post! npcg val) (set-car! (list-tail npcg 4) val))
(define (npcg-has-post? npcg) (not (null? (npcg-get-post npcg))))
(define (npcg-get-post npcg) (list-ref npcg 4))
(define (npcg-get-subgob npcg) (list-ref npcg 5))
(define (npcg-set-subgob! npcg val) (set-car! (list-tail npcg 5) val))
(define (is-npcg? gob) (eq? (car gob) 'npcg))

(define (kbeing-is-npc-type? kbeing type)
  (let ((npcg (gob kbeing)))
    (and (not (null? npcg))
         (is-npcg? npcg)
         (npcg-is-type? npcg type))))

(define (kbeing-was-spawned? kbeing)
  (let ((npcg (gob kbeing)))
    (and (not (null? npcg))
         (is-npcg? npcg)
         (npcg-spawned? npcg))))

;; An NPC inventory is kind of an abstract container. It should never appear on
;; the world map.
(define (mk-inventory contents)
  (kern-mk-inventory contents))

;; mk-npc -- create a kernel character of the given type, faction and level
;; (define (mk-npc npct-tag lvl)
;;   (let* ((npct (eval npct-tag))
;;          (npc (bind
;;                (set-level
;;                 (kern-char-arm-self
;;                  (mk-stock-char
;;                   (npct-name npct)
;;                   (npct-spec npct)
;;                   (npct-occ npct)
;;                   (npct-spr npct)
;;                   (npct-faction npct)
;;                   (npct-ai npct)
;;                   (mk-inventory
;;                    (filter notnull?
;;                            (map (lambda (x)
;;                                   (apply roll-to-add x))
;;                                 (npct-eqp npct))))
;;                   nil
;;                   (npct-conv npct)))
;;                 lvl)
;;                (npcg-mk npct-tag))))
;;     (map (lambda (eff) (apply-eff-pkg npc eff))
;;          (npct-effects npct))
;;     (if (not (null? (npct-drop-fx npct)))
;;         (kern-obj-add-effect npc 
;;                              ef_loot_drop 
;;                              (loot-drop-mk (npct-drop-fx npct)
;;                                            (npct-drop-fx-parms npct))))
;;     npc))

(define (mk-npc npct-tag lvl)
  (let* ((npct (eval npct-tag))
         (npc (mk-char npct)))
    (kern-char-arm-self npc)
    (set-level npc lvl)
    (bind npc (npcg-mk npct-tag))
    (map (lambda (eff) 
           (apply-eff-pkg npc eff))
         (get npct 'effects nil))
    npc))

;; spawn-npc -- like mk-npc but mark the npc as spawned (this allows monster
;; managers to periodically clean up old spawned NPC's)
;(define (spawn-npc npct-tag lvl)
;  (let ((kchar (mk-npc npct-tag lvl)))
;    (npcg-set-spawned! (gob kchar) #t)
;    kchar))

(define (spawn-npc npct-tag lvl)
  (let ((kchar (mk-npc npct-tag lvl)))
    (npcg-set-spawned! (gob kchar) #t)
    kchar))

;;----------------------------------------------------------------------------
;; trap packages
(define no-traps (list nil))
(define basic-traps  (list nil 'burn 'spike-trap))
(define wizard-traps (list nil 'poison-trap 'sleep-trap 'lightning-trap))
(define wrogue-traps (list nil 'self-destruct-trap 'bomb-trap 'sleep-trap 'poison-trap 'spike-trap 'sleep-trap 'burn))

;;----------------------------------------------------------------------------
;; effect packages
(define slime-effects  (list ef_poison_immunity 
                             (list ef_split split-gob-mk 'green-slime)))
(define yellow-slime-effects  (list ef_poison_immunity))
(define undead-effects (list ef_poison_immunity 
                             ef_fire_immunity 
                             ef_disease_immunity 
                             ef_sleep_immunity 
                             ef_magical_kill_immunity))
(define demon-effects (list ef_poison_immunity 
                            ef_fire_immunity 
                            ef_disease_immunity 
                            ef_sleep_immunity
                            ef_magical_kill_immunity))
(define hydra-effects (list ef_poison_immunity ef_grow_head))
(define drag-effects (list ef_fire_immunity))
(define wisp-effects (list ef_poison_immunity 
                           ef_disease_immunity))
(define fire-slime-effects (list ef_fire_immunity
                                 (list ef_split split-gob-mk 'fire-slime)))
(define sludge-kraken-effects (list ef_cleanup_tentacles))

(define (apply-eff-pkg knpc pkg)
  (if (pair? pkg)
      (let* ((eff (car pkg))
             (gob-ctor (cadr pkg))
             (gob-args (cddr pkg))
             (gob (apply gob-ctor gob-args)))
        (kern-obj-add-effect knpc eff gob))
      (kern-obj-add-effect knpc pkg nil)))

;;----------------------------------------------------------------------------
;; equipment packages for different types of npcs
(define wizard-equip 
  (list (list 100 "1"     t_dagger)
        ))
(define archer-equip 
  (list (list 100 "1"     t_bow)
        (list 100 "1d6"   t_arrow)
        (list 100 "1"     t_dagger)
        ))
(define stalker-equip 
  (list (list 100 "2"     t_dagger)
        (list 100 "2"     t_dagger)
        ))
(define slinger-equip 
  (list (list 100 "1"     t_sling)
        ))
(define berserker-equip 
  (list (list 100 "2"     t_axe)         
        (list 100 "1d2"   t_heal_potion)
        ))
(define ranger-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_bow)
        (list 100 "20"    t_arrow)
        (list 100 "1"     t_leather_helm)
        (list 100 "1"     t_armor_leather)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define skeletal-warrior-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_shield)
        (list 100 "1"     t_iron_helm)
        ))
(define spear-thrower-equip
  (list (list 100 "1d20"  t_spear)
        (list 100 "1"     t_iron_helm)
        (list 100 "1"     t_axe)
        ))
(define death-knight-equip
  (list (list 100 "1"     t_2h_axe)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1d3-1" t_mana_potion)
        ))
(define knight-equip
  (list (list 100 "1"     t_2h_sword)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define squire-equip
  (list (list 100 "1"     t_crossbow)
        (list 100 "1d10"  t_bolt)
        (list 100 "1"     t_dagger)
        (list 100 "1"     t_armor_chain)
        (list 100 "1"     t_chain_coif)
        (list 100 "1d2-1" t_heal_potion)
        ))
(define halberdier-equip
  (list (list 100 "1"     t_halberd)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_armor_chain)
        (list 100 "1d3-1" t_heal_potion)
        (list 10  "1"     t_vas_mani_scroll)
        (list 10  "1"     t_in_an_scroll)
        ))
(define crossbowman-equip
  (list (list 100 "1"     t_crossbow)
        (list 100 "10"    t_bolt)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_dagger)
        (list 100 "1"     t_armor_chain)
        (list 100 "1d3-1" t_heal_potion)
        (list 10  "1"     t_vas_mani_scroll)
        (list 10  "1"     t_in_an_scroll)
        ))
(define wrogue-1-equip
  (list (list 100 "1"     t_dagger)
        (list 100 "2d6-2" t_gold_coins)
        (list 50  "1d5"   t_food)
        (list 10  "1d3"   t_torch)
        (list 15  "1d3"   t_smoke_bomb)
        ))
(define wrogue-2-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_sling)
        (list 100 "1"     t_leather_helm)
        (list 100 "1"     t_armor_leather)
        ))
(define wrogue-3-equip
  (list (list 100 "1"     t_sword)
        (list 100 "1"     t_leather_helm)
        (list 100 "1"     t_armor_leather)
        (list 100 "1d10"  t_arrow)
        (list 75  "1"     t_bow)
        ))
(define wrogue-4-equip
  (list (list 100 "1"     t_armor_chain)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_sword)
        (list 100 "1d10"  t_bolt)
        (list 75  "1"     t_crossbow)
        ))
(define medik-equip
  (list (list 100 "1d3"   t_heal_potion)
        (list 100 "1d2"   t_mana_potion)
        (list 25  "1d2"   t_cure_potion)
        (list 100 "1"     t_chain_coif)
        (list 100 "1"     t_staff)
        (list 100 "1"     t_armor_chain)
        ))
(define trog-equip
  (list (list 100 "1d3" t_thrown_boulder)
        (list 25  "1d3"   t_food)
        (list 100 "2d10"  t_gold_coins)
        ))
(define geomancer-equip
  (list (list 100 "1d3-1" t_mana_potion)
        (list 50  "1d3"   t_gem)
        (list 50  "1d20"  t_gold_coins)
        ))
(define gint-warrior-equip
  (list (list 100 "1"     t_2h_axe)
        (list 100 "1"     t_2h_sword)
        (list 100 "1d3-1" t_heal_potion)
        ))
(define headless-equip
  (list (list 100 "1"     t_axe)
        (list 100 "1"     t_shield)
        ))
(define craven-archer-equip
  (list (list 100 "1"     t_bow)
        (list 100 "20"    t_arrow)
        (list 100 "1"     t_armor_plate)
        (list 100 "1"     t_iron_helm)
        (list 100 "1"     t_dagger)
        (list 100 "1d3-1" t_mana_potion)
        ))
(define nixie-1-equip
  (list (list 100 "1d20" t_spear)
        ))
(define nixie-2-equip
  (list (list 100 "1d20" t_sword)
        ))
(define bomber-equip
  (list (list 100 "1d5" t_oil)
        (list 100 "1"   t_dagger)
        (list 25  "1d3" t_smoke_bomb)
        ))

(define accursed-1-equip
  (list (list 100 "1" t_dagger)
        ))
(define accursed-2-equip
  (list (list 100 "1" t_dagger)
        (list 75  "1" t_sling)
        ))
(define accursed-3-equip
  (list (list 100 "1" t_staff)
        (list 75  "1" t_sling)
        ))
(define accursed-4-equip
  (list (list 100 "1" t_sword)
        (list 100 "1" t_shield)
        (list 100 "1" t_leather_helm)
        (list 100 "1" t_armor_leather)
        ))
(define accursed-5-equip
  (list (list 100 "1" t_sword)
        (list 100 "1" t_shield)
        (list 100 "1" t_chain_coif)
        (list 100 "1" t_armor_chain)
        (list 100 "1" t_crossbow)
        (list 100 "1d10" t_bolt)
        ))
(define accursed-6-equip
  (list (list 100 "1" t_sword)
        (list 100 "1" t_morning_star)
        (list 100 "1" t_iron_helm)
        (list 100 "1" t_armor_plate)
        ))
        
(define demon-equip
  (list (list 100 "1" t_flaming_sword)
        ))

;;----------------------------------------------------------------------------
;; Loot drops
(define animal-loot
  (list (list 25 "1" 't_food)
        ))

(define deer-loot
  (list (list 100 "1" 't_animal_corpse)
        ))

(define bull-loot
  (list (list 100 "5" 't_food)
        ))

(define wizard-loot
  (list (list 100 "1d2-1" 't_heal_potion)
        (list 100 "1d2+1" 't_mana_potion)
        (list 100 "1d20"  't_gold_coins)
        (list 10  "1d3"   't_food)
        (list 10  "1"     't_cure_potion)
        (list 10  "1"     't_poison_immunity_potion)
        (list 20  "1d5"   'sulphorous_ash)
        (list 20  "1d5"   'ginseng)
        (list 20  "1d5"   'garlic)
        (list 10  "1d3"   'spider_silk)
        (list 10  "1d3"   'blood_moss)
        (list 10  "1d3"   'black_pearl)
        (list 5   "1d2"   'nightshade)
        (list 5   "1d2"   'mandrake)
        (list 5   "1"     't_in_mani_corp_scroll)
        (list 5   "1"     't_xen_corp_scroll)
        (list 10  "1"     't_in_quas_xen_scroll)
        (list 10  "1"     't_an_xen_ex_scroll)
        (list 20  "1"     't_in_an_scroll)
        (list 20  "1"     't_vas_mani_scroll)
        (list 5   "1"     't_dagger)
        ))

(define std-loot
  (list (list 25 "1d2" 't_food)
        (list 100 "1d10" 't_gold)
        (list 25 "1" 't_heal_potion)
        (list 10 "1" 't_torch)
        (list 1 "1" 't_gem)
        ))

(define archer-loot 
  (list
        (list 100 "1d10"  't_gold_coins)
        (list 20  "1d3"   't_food)
        (list 10  "1"   't_bow)
        ))
(define stalker-loot 
  (list
        (list 100 "1d15"  't_gold_coins)
        (list 30  "1d3"   't_food)
        ))
(define slinger-loot 
  (list (list 100 "1d10"  't_gold_coins)
        (list 20  "1d3"   't_food)
        ))
(define berserker-loot 
  (list (list 100 "1d2"   't_heal_potion)
        (list 100 "1d15"  't_gold_coins)
        (list 30  "1d3"   't_food)
        ))
(define ranger-loot
  (list (list 100 "1d10" 't_gold_coins)
        (list 30  "1d3"   't_food)
        (list 100 "1d3-1" 't_heal_potion)
        ))
(define skel-war-loot
  (list (list 100 "1d20"  't_gold_coins)
        ))
(define spear-thrower-loot
  (list (list 100 "1d20"  't_gold_coins)
        ))
(define dea-kni-loot
  (list
        (list 100 "1d20"  't_gold_coins)
        ))
(define cra-arch-loot
  (list (list 100 "1d20"  't_gold_coins)
        ))
(define knight-loot
  (list (list 100 "1d20"  't_gold_coins)
        ))
(define squire-loot
  (list (list 100 "1d10"  't_gold_coins)
        ))
(define halberdier-loot
  (list (list 50  "1d5"   't_food)
        ))
(define crossbowman-loot
  (list (list 50  "1d5"   't_food)
        ))
(define wrogue-2-loot
  (list (list 100 "2d6-2" 't_gold_coins)
        (list 100 "1d3-1" 't_picklock)
        (list 50  "1d5"   't_food)
        (list 10  "1d3"   't_torch)
        (list 10  "1d3"  't_smoke_bomb)
        ))
(define wrogue-3-loot
  (list (list 100 "2d6-2" 't_gold_coins)
        (list 100 "1d3-1" 't_picklock)
        (list 50  "1d5"   't_food)
        (list 10  "1d3"   't_torch)
        (list 5   "1d3"   't_smoke_bomb)
        ))
(define wrogue-4-loot
  (list (list 100 "2d6-2" 't_gold_coins)
        (list 100 "1d3-1" 't_picklock)
        (list 50  "1d5"   't_food)
        (list 50  "1d10"  't_bolt)
        (list 10  "1"     't_in_ex_por_scroll)
        (list 10  "1"     't_wis_quas_scroll)
        (list 5   "1"     't_sanct_lor_scroll)
        (list 5   "1"     't_an_tym_scroll)
        (list 5   "1"     't_vas_rel_por_scroll)
        (list 20  "1"     't_mana_potion)
        (list 10  "1"     't_cure_potion)
        (list 10  "1"     't_poison_immunity_potion)
        (list 10  "1d3"   't_torch)
        (list 2   "1d3"   't_smoke_bomb)
        ))
(define medik-loot
  (list (list 100 "1d3"   't_heal_potion)
        (list 100 "1d2"   't_mana_potion)
        (list 25  "1d2"   't_cure_potion)
        (list 25  "1d2"   't_vas_mani_scroll)
        (list 20  "1d5"   'sulphorous_ash)
        (list 20  "1d5"   'ginseng)
        (list 20  "1d5"   'garlic)
        (list 10  "1d3"   'spider_silk)
        (list 10  "1d3"   'blood_moss)
        (list 10  "1d3"   'black_pearl)
        (list 5   "1d2"   'nightshade)
        (list 5   "1d2"   'mandrake)
        ))
(define gint-loot
  (list (list 100 "4d25"  't_gold_coins)
        (list 100 "1d5"   't_food)
        ))
(define reaper-loot
  (list (list 100 "1d5"   't_torch)
        ))
(define headless-loot
  (list (list 100 "1d5-1" 't_gold_coins)
        ))
(define dragon-loot
  (list (list 100 "1d100+19" 't_gold_coins)
        (list 100 "1d20"     't_food)
        (list 100 "1d5-1"    't_gem)
        (list 100 "1"        't_dragons_blood)
        ;; none of this is logical, beyond 'dragons oughta have the good stuff'
        (list 10  "1"     't_in_ex_por_scroll)
        (list 10  "1"     't_wis_quas_scroll)
        (list 5   "1"     't_sanct_lor_scroll)
        (list 5   "1"     't_an_tym_scroll)
        (list 5   "1"     't_vas_rel_por_scroll)
        (list 20  "1"     't_mana_potion)
        (list 10  "1"     't_cure_potion)
        (list 20  "1d5"   'sulphorous_ash)
        (list 20  "1d5"   'ginseng)
        (list 20  "1d5"   'garlic)
        (list 10  "1d3"   'spider_silk)
        (list 10  "1d3"   'blood_moss)
        (list 10  "1d3"   'black_pearl)
        (list 5   "1d2"   'nightshade)
        (list 5   "1d2"   'mandrake)
        ))

(define hydra-loot
  (list (list 100 "1" 't_hydras_blood)
        ))

(define lich-loot
  (cons (list 100 "1" 't_lichs_blood)
        wizard-loot))

(define zorn-loot
  (list (list 100 "1d20+9" 't_gold_coins)
        ))

(define bomber-loot
  (list (list 50 "1d3" 't_oil)
        ))
        
(define dryad-loot
  (list (list 100 "1d5" 't_torch)
        ))
        
(define demon-loot
  (list (list 100 "2d20" 't_gold_coins)
        ))
        
(define ghast-loot
  (list (list 50 "1" 't_mana_potion)
        ))
        
(define yellow-slime-loot
  (list (list 50 "1" 't_royal_cape)
        ))
        
(define fire-slime-loot
  (list (list 100 "1" 't_oil)
        ))
        
(define spider-loot
  (list (list 50 "1" 'spider_silk)
        ))
        
(define queen-spider-loot
  (list (list 50 "1d3" 'spider_silk)
        (list 25 "1" 't_poison_immunity_potion)
        ))

(define accursed-1-loot
  (list (list 50 "1d2-1" 't_heal_potion)
        (list 50 "1d2" 't_mana_potion)
        (list 100 "1d10"  't_gold_coins)
        (list 10  "1d2"   't_food)
        (list 5  "1"     't_cure_potion)
        (list 5  "1"     't_poison_immunity_potion)
        (list 10  "1d3"   'sulphorous_ash)
        (list 10  "1d3"   'ginseng)
        (list 10  "1d3"   'garlic)
        (list 5  "1d2"   'spider_silk)
        (list 5  "1d2"   'blood_moss)
        (list 5  "1d2"   'black_pearl)
        (list 5  "1"     't_in_quas_xen_scroll)
        (list 5  "1"     't_an_xen_ex_scroll)
        (list 5  "1"     't_in_an_scroll)
        (list 5  "1"     't_vas_mani_scroll)
        )) 

(define accursed-5-loot
  (list (list 50  "1d5"   't_food)
        (list 100 "2d10" 't_gold_coins)
        ))  
        
        
(define (drop-generic knpc loot)
  (if (not (kern-place-is-wilderness? (loc-place (kern-obj-get-location knpc))))
           (let ((loc (kern-obj-get-location knpc)))
             (map (lambda (triple)
                    (let ((thresh (car triple))
                          (dice (cadr triple))
                          (type-tag (caddr triple)))
                      (if (< (modulo (random-next) 100) thresh)
                          (let ((quantity (kern-dice-roll dice)))
                            (if (> quantity 0)
                            		(let ((obj (kern-mk-obj (eval type-tag) quantity)))
                            			(if (can-be-dropped? obj loc cant)
                                			(kern-obj-put-at obj loc)
                       			)))))))
                  loot)
             )))
  
;; npc types
;;      scheme variable                 name                       species          occup.     sprite             chest traps  equipment              effects       ai               faction
;;      ======================          ========================== ================ ========== ================== ============ ====================== ============= ==============   ========
;;(define forest-goblin-shaman  (mk-npct2 "forest goblin shaman"  sp_forest_goblin oc_wizard  s_fgob_shaman wizard-traps wizard-equip  nil 'shaman-ai  faction-forest-goblin nil 'drop-generic wizard-loot ))
(define forest-goblin-shaman
  '((name    . "forest goblin shaman")
    (species . sp_forest_goblin)
    (occ     . oc_wizard)
    (sprite  . s_fgob_shaman)
    (stuff   . wizard-equip)
    (ai      . 'shaman-ai)
    (faction . faction-forest-goblin)))

(define forest-goblin-hunter
  '((name    . "forest goblin hunter")
    (species . sp_forest_goblin)
    (occ     . oc_warrior)
    (sprite  . s_fgob_archer)
    (stuff   . archer-equip)
    (faction . faction-forest-goblin)))

(define forest-goblin-stalker
  '((name    . "forest goblin stalker")
    (species . sp_forest_goblin)
    (occ     . oc_warrior)
    (sprite  . s_fgob_stalker)
    (stuff   . stalker-equip)
    (faction . faction-forest-goblin)))

(define cave-goblin-slinger
  '((name    . "cave goblin slinger")
    (species . sp_cave_goblin)
    (occ     . oc_warrior)
    (sprite  . s_cgob_slinger)
    (stuff   . slinger-equip)
    (faction . faction-cave-goblin)))

(define cave-goblin-berserker
  '((name    . "cave goblin berserker")
    (species . sp_cave_goblin)
    (occ     . oc_warrior)
    (sprite  . s_cgob_berserk)
    (stuff   . berserker-equip)
    (faction . faction-cave-goblin)))

(define cave-goblin-priest
  '((name    . "cave goblin priest")
    (species . sp_cave_goblin)
    (occ     . oc_wizard)
    (sprite  . s_cgob_shaman)
    (stuff   . wizard-equip)
    (ai      . 'priest-ai)
    (faction . faction-cave-goblin)))

(define ranger
  '((name    . "ranger")
    (species . sp_human)
    (occ     . oc_ranger)
    (sprite  . s_ranger)
    (stuff   . ranger-equip)
    (ai      . 'ranger-ai)
    (faction . faction-men)))

(define skeletal-spear-thrower
  '((name    . "skeletal spear-thrower")
    (species . sp_skeleton)
    (occ     . oc_warrior)
    (sprite  . s_spearskeleton)
    (stuff   . spear-thrower-equip)
    (effects . undead-effects)
    (ai      . 'nolight-ai)
    (faction . faction-monster)))

(define skeletal-warrior
  '((name    . "skeletal warrior")
    (species . sp_skeleton)
    (occ     . oc_warrior)
    (sprite  . s_skeleton)
    (stuff   . skeletal-warrior-equip)
    (effects . undead-effects)
    (ai      . 'nolight-ai)
    (faction . faction-monster)))

(define skeletal-archer
  '((name    . "skeletal archer")
    (species . sp_skeleton)
    (occ     . oc_archer)
    (sprite  . s_skeletonarcher)
    (stuff   . archer-equip)
    (effects . undead-effects)
    (ai      . 'nolight-ai)
    (faction . faction-monster)))

(define death-knight
  '((name    . "death knight")
    (species . sp_skeleton)
    (occ     . oc_warrior)
    (sprite  . s_deathknight)
    (stuff   . death-knight-equip)
    (effects . undead-effects)
    (ai      . 'death-knight-ai)
    (faction . faction-monster)))

(define craven-archer
  '((name    . "craven archer")
    (species . sp_skeleton)
    (occ     . oc_warrior)
    (sprite  . s_deatharcher)
    (stuff   . craven-archer-equip)
    (effects . undead-effects)
    (ai      . 'craven-archer-ai)
    (faction . faction-monster)))

(define halberdier    
  '((name . "halberdier")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_guard)
    (stuff . halberdier-equip)
    (ai . 'guard-ai)
    (faction . faction-men)))

(define crossbowman   
  '((name . "crossbowman")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_xbowguard)
    (stuff . crossbowman-equip)
    (ai . 'guard-ai)
    (faction . faction-men)))

(define medik         
  '((name . "medik")
    (species . sp_human)
    (occ . oc_wizard)
    (sprite . s_blue_wizard)
    (stuff . medik-equip)
    (ai . 'medik-ai)
    (faction . faction-men)))

(define trog         
  '((name . "trog")
    (species . sp_trog)
    (occ . oc_warrior)
    (sprite . s_male_trog)
    (stuff . trog-equip)
    (ai . 'std-ai)
    (faction . faction-trog)))

(define glasdrin-halberdier    
  '((name . "halberdier")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_guard)
    (stuff . halberdier-equip)
    (ai . 'guard-ai)
    (faction . faction-glasdrin)))

(define glasdrin-crossbowman   
  '((name . "crossbowman")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_xbowguard)
    (stuff . crossbowman-equip)
    (ai . 'guard-ai)
    (faction . faction-glasdrin)))

;; Bandit types

(define footpad    
  '((name . "footpad")
    (species . sp_human)
    (occ . oc_wrogue)
    (sprite . s_brigand)
    (stuff . wrogue-1-equip)
    (ai . 'std-ai)
    (faction . faction-outlaw)))

(define bandit     
  '((name . "bandit")
    (species . sp_human)
    (occ . oc_wrogue)
    (sprite . s_brigand)
    (stuff . wrogue-2-equip)
    (ai . 'std-ai)
    (faction . faction-outlaw)))

(define highwayman 
  '((name . "highwayman")
    (species . sp_human)
    (occ . oc_wrogue)
    (sprite . s_brigand)
    (stuff . wrogue-3-equip)
    (ai . 'std-ai)
    (faction . faction-outlaw)))

(define blackguard 
  '((name . "blackguard")
    (species . sp_human)
    (occ . oc_wrogue)
    (sprite . s_brigand)
    (stuff . wrogue-4-equip)
    (ai . 'std-ai)
    (faction . faction-outlaw)))

(define bomber     
  '((name . "mad jester")
    (species . sp_human)
    (occ . oc_wrogue)
    (sprite . s_jester)
    (stuff . bomber-equip)
    (ai . 'std-ai)
    (faction . faction-outlaw)))


(define bat 
  '((name . "bat")
    (species . sp_bat)
    (sprite . s_bat)
    (ai . 'animal-ai)
    (faction . faction-monster)))

(define rat 
  '((name . "dire rat")
    (species . sp_rat)
    (sprite . s_rat)
    (ai . 'rat-ai)
    (faction . faction-monster)))

(define zorn 
  '((name . "zorn")
    (species . sp_zorn)
    (occ . oc_wrogue)
    (sprite . s_zorn)
    (ai . 'animal-ai)
    (faction . faction-monster)))

(define bull 
  '((name . "bull")
    (species . sp_bull)
    (sprite . s_bull)
    (ai . 'animal-ai)
    (faction . faction-none)))

(define lich 
  '((name . "lich")
    (species . sp_lich)
    (occ . oc_wizard)
    (sprite . s_lich)
    (stuff . wizard-equip)
    (ai . 'spell-sword-ai)
    (faction . faction-monster)))

(define dryad 
  '((name . "dryad")
    (species . sp_dryad)
    (sprite . s_reaper)
    (ai . 'dryad-ai)
    (faction . faction-monster)))

(define gazer 
  '((name . "gazer")
    (species . sp_gazer)
    (occ . oc_wizard)
    (sprite . s_gazer)
    (ai . 'gazer-ai)
    (faction . faction-monster)))

(define demon 
  '((name . "demon")
    (species . sp_demon)
    (sprite . s_demon)
    (stuff . demon-equip)
    (ai . 'demon-ai)
    (faction . faction-monster)))

(define ghast 
  '((name . "ghast")
    (species . sp_ghast)
    (sprite . s_ghost)
    (ai . 'std-ai)
    (faction . faction-monster)))

(define snake 
  '((name . "snake")
    (species . sp_snake)
    (sprite . s_snake)
    (ai . 'snake-ai)
    (faction . faction-monster)))

(define insect 
  '((name . "insect swarm")
    (species . sp_insect)
    (sprite . s_insects)
    (ai . 'animal-ai)
    (faction . faction-monster)))

(define dragon 
  '((name . "dragon")
    (species . sp_dragon)
    (sprite . s_dragon)
    (ai . 'dragon-ai)
    (faction . faction-monster)))

(define knight 
  '((name . "knight")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_knight)
    (stuff . knight-equip)
    (ai . 'guard-ai)
    (faction . faction-trigrave)))

(define paladin 
  '((name . "paladin")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_companion_paladin)
    (stuff . knight-equip)
    (ai . 'std-ai)
    (faction . faction-men)))

(define tinker 
  '((name . "tinker")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_companion_tinker)
    (stuff . wrogue-4-equip)
    (ai . 'std-ai)
    (faction . faction-men)))

(define squire 
  '((name . "squire")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_xbowguard)
    (stuff . squire-equip)
    (ai . 'guard-ai)
    (faction . faction-trigrave)))

(define warlock 
  '((name . "warlock")
    (species . sp_human)
    (occ . oc_wizard)
    (sprite . s_wizard)
    (stuff . wizard-equip)
    (ai . 'warlock-ai)
    (faction . faction-monster)))

(define wizard 
  '((name . "wizard")
    (species . sp_human)
    (occ . oc_wizard)
    (sprite . s_companion_wizard)
    (stuff . wizard-equip)
    (ai . 'spell-sword-ai)
    (faction . faction-men)))

(define headless 
  '((name . "headless")
    (species . sp_headless)
    (occ . oc_warrior)
    (sprite . s_headless)
    (stuff . headless-equip)
    (ai . 'animal-ai)
    (faction . faction-monster)))

(define gint-mage 
  '((name . "gint mage")
    (species . sp_gint)
    (occ . oc_wizard)
    (sprite . s_gint_mage)
    (stuff . wizard-equip)
    (ai . 'shaman-ai)
    (faction . faction-gint)))

(define gint-warrior 
  '((name . "gint warrior")
    (species . sp_gint)
    (occ . oc_warrior)
    (sprite . s_gint)
    (stuff . gint-warrior-equip)
    (ai . 'std-ai)
    (faction . faction-gint)))

(define yellow-slime 
  '((name . "yellow slime")
    (species . sp_yellow_slime)
    (sprite . s_yellow_slime)
    (ai . 'yellow-slime-ai)
    (faction . faction-monster)))

(define trog-geomancer 
  '((name . "trog geomancer")
    (species . sp_trog)
    (occ . oc_wizard)
    (sprite . s_female_trog)
    (stuff . geomancer-equip)
    (ai . 'geomancer-ai)
    (faction . faction-trog)))

(define corrupt-halberdier 
  '((name . "halberdier")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_guard)
    (stuff . halberdier-equip)
    (ai . 'guard-ai)
    (faction . faction-monster)))

(define corrupt-crossbowman 
  '((name . "crossbowman")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_guard)
    (stuff . crossbowman-equip)
    (ai . 'guard-ai)
    (faction . faction-monster)))

(define giant-spider 
  '((name . "giant spider")
    (species . sp_spider)
    (sprite . s_spider)
    (ai . 'spider-ai)
    (faction . faction-monster)))

(define queen-spider 
  '((name . "queen spider")
    (species . sp_queen_spider)
    (sprite . s_queen_spider)
    (ai . 'spider-ai)
    (faction . faction-monster)))

(define fire-slime 
  '((name . "fire slime")
    (species . sp_fire_slime)
    (sprite . s_red_slime)
    (ai . 'animal-ai)
    (faction . faction-monster)))

(define hydra 
  '((name . "hydra")
    (species . sp_hydra)
    (sprite . s_hydra)
    (ai . 'hydra-ai)
    (faction . faction-monster)))

(define mimic 
  '((name . "mimic")
    (species . sp_mimic)
    (sprite . s_mimic)
    (ai . 'animal-ai)
    (faction . faction-monster)))

(define ratling-swarmer 
  '((name . "ratling swarmer")
    (species . sp_ratling)
    (sprite . s_mouse)
    (ai . 'ratling-ai)
    (faction . faction-monster)))

(define ratling-sorcerer 
  '((name . "ratling sorcerer")
    (species . sp_ratling)
    (occ . oc_wizard)
    (sprite . s_ratling_sorcerer)
    (stuff . wizard-equip)
    (ai . 'ratling-sorcerer-ai)
    (faction . faction-monster)))

(define carabid 
  '((name . "carabid")
    (species . sp_carabid)
    (sprite . s_carabid)
    (ai . 'carabid-ai)
    (faction . faction-monster)))

(define deer 
  '((name . "deer")
    (species . sp_deer)
    (sprite . s_deer)
    (faction . faction-none)))

(define chicken 
  '((name . "chicken")
    (species . sp_chicken)
    (sprite . s_chicken)
    (ai . 'animal-ai)
    (faction . faction-none)))

;; NPC's with no drops

(define green-slime
  '((name . "green slime")
    (species . sp_green_slime)
    (sprite . s_slime)
    (effects . slime-effects)
    (ai . 'animal-ai)
    (faction . faction-monster)))

(define kraken          
  '((name . "kraken")
    (species . sp_kraken)
    (sprite . s_kraken)
    (ai . 'kraken-ai)
    (faction . faction-monster)))

(define sea-serpent     
  '((name . "sea serpent")
    (species . sp_sea_serpent)
    (sprite . s_sea_serpent)
    (ai . 'sea-serpent-ai)
    (faction . faction-monster)))

(define wolf            
  '((name . "wolf")
    (species . sp_wolf)
    (sprite . s_wolf)
    (ai . 'wolf-ai)
    (faction . faction-monster)))

(define wisp            
  '((name . "wisp")
    (species . sp_wisp)
    (sprite . s_wisp)
    (effects . wisp-effects)
    (ai . 'wisp-ai)
    (faction . faction-monster)))
(define nixie-spearman  
  '((name . "nixie spearman")
    (species . sp_nixie)
    (occ . oc_warrior)
    (sprite . s_nixie_spear)
    (equip . nixie-1-equip)
    (ai . 'nixie-ai)
    (faction . faction-monster)))

(define nixie-swordsman 
  '((name . "nixie swordsman")
    (species . sp_nixie)
    (occ . oc_warrior)
    (sprite . s_nixie_sword)
    (equip . nixie-2-equip)
    (ai . 'nixie-ai)
    (faction . faction-monster)))

(define sludge-kraken   
  '((name . "sludge kraken")
    (species . sp_great_kraken)
    (sprite . s_great_kraken)
    (effects . sludge-kraken-effects)
    (ai . 'sludge-kraken-ai)
    (faction . faction-monster)))

(define sludge-tentacle 
  '((name . "sludge kraken tentacle")
    (species . sp_kraken_tentacle)
    (sprite . s_tentacle)
    (ai . 'sludge-tentacle-ai)
    (faction . faction-monster)))

(define griffin         
  '((name . "griffin")
    (species . sp_griffin)
    (sprite . s_griffin)
    (ai . 'griffin-ai)
    (faction . faction-monster)))

(define griffin-chick   
  '((name . "griffin chick")
    (species . sp_griffin_chick)
    (sprite . s_griffin_chick)
    (ai . 'griffin-ai)
    (faction . faction-monster)))

;; accursed

(define accursed-acolyte    
  '((name . "an accursed acolyte")
    (species . sp_human)
    (occ . oc_wizard)
    (sprite . s_shepherd)
    (stuff . accursed-1-equip)
    (ai . 'spell-sword-ai)
    (faction . faction-accursed)))

(define accursed-apprentice 
  '((name . "an accursed apprentice")
    (species . sp_human)
    (occ . oc_wizard)
    (sprite . s_shepherd)
    (stuff . accursed-2-equip)
    (ai . 'spell-sword-ai)
    (faction . faction-accursed)))

(define accursed-journeyman 
  '((name . "an accursed journeyman")
    (species . sp_human)
    (occ . oc_wizard)
    (sprite . s_wizard)
    (stuff . accursed-3-equip)
    (ai . 'spell-sword-ai)
    (faction . faction-accursed)))

(define accursed-master     
  '((name . "an accursed master")
    (species . sp_human)
    (occ . oc_wizard)
    (sprite . s_wizard)
    (stuff . accursed-3-equip)
    (ai . 'spell-sword-ai)
    (faction . faction-accursed)))

(define accursed-adept      
  '((name . "an accursed adept")
    (species . sp_human)
    (occ . oc_wizard)
    (sprite . s_wizard)
    (stuff . accursed-3-equip)
    (ai . 'spell-sword-ai)
    (faction . faction-accursed)))

(define accursed-guardian   
  '((name . "an accursed guardian")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_fighter)
    (stuff . accursed-4-equip)
    (ai . 'std-ai)
    (faction . faction-accursed)))

(define accursed-defender   
  '((name . "an accursed defender")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_knight)
    (stuff . accursed-5-equip)
    (ai . 'std-ai)
    (faction . faction-accursed)))

(define accursed-templar    
  '((name . "an accursed templar")
    (species . sp_human)
    (occ . oc_warrior)
    (sprite . s_avatar)
    (stuff . accursed-6-equip)
    (ai . 'std-ai)
    (faction . faction-accursed)))

;; npcs with odd alignments

(define gint-warrior-m 
  '((name . "gint warrior")
    (species . sp_gint)
    (occ . oc_warrior)
    (sprite . s_gint)
    (stuff . gint-warrior-equip)
    (ai . 'std-ai)
    (faction . faction-monster)))

(define trog-m         
  '((name . "trog")
    (species . sp_trog)
    (occ . oc_warrior)
    (sprite . s_male_trog)
    (stuff . trog-equip)
    (ai . 'std-ai)
    (faction . faction-monster)))

(define cave-goblin-slinger-m   
  '((name . "cave goblin slinger")
    (species . sp_cave_goblin)
    (occ . oc_warrior)
    (sprite . s_cgob_slinger)
    (stuff . slinger-equip)
    (ai . 'std-ai)
    (faction . faction-monster)))

(define cave-goblin-berserker-m 
  '((name . "cave goblin berserker")
    (species . sp_cave_goblin)
    (occ . oc_warrior)
    (sprite . s_cgob_berserk)
    (stuff . berserker-equip)
    (ai . 'std-ai)
    (faction . faction-monster)))

(define gint-mage-m 
  '((name . "gint mage")
    (species . sp_gint)
    (occ . oc_wizard)
    (sprite . s_gint_mage)
    (stuff . wizard-equip)
    (ai . 'shaman-ai)
    (faction . faction-monster)))

(define trog-geomancer-m 
  '((name . "trog geomancer")
    (species . sp_trog)
    (occ . oc_wizard)
    (sprite . s_female_trog)
    (stuff . geomancer-equip)
    (ai . 'std-ai)
    (faction . faction-monster)))

;;----------------------------------------------------------------------------
;; Type queries
;;----------------------------------------------------------------------------
(define (is-species? kchar species)
  (eqv? (kern-char-get-species kchar) species))

(define (is-occ? kchar occ)
  (eqv? (kern-char-get-occ kchar) occ))

(define (is-yellow-slime? kchar)
  (is-species? kchar sp_yellow_slime))

(define (is-green-slime? kchar)
  (is-species? kchar sp_green_slime))

(define (is-spider? kchar)
  (or (is-species? kchar sp_spider)
      (is-species? kchar sp_queen_spider)))

(define (is-trog? kchar)
  (is-species? kchar sp_trog))

(define (is-goblin? kchar)
  (or (is-species? kchar sp_cave_goblin)
      (is-species? kchar sp_forest_goblin)))

(define (is-skeleton? kchar)
  (is-species? kchar sp_skeleton))

(define (is-death-knight? kchar)
  (and (is-species? kchar sp_skeleton)
       (is-occ? kchar oc_warrior)))

(define (is-bandit? kchar)
  (is-occ? kchar oc_wrogue))

(define (is-halberdier? kchar)
  (is-guard-type? kchar 'halberdier))

(define (is-crossbowman? kchar)
  (is-guard-type? kchar 'crossbowman))

(define (is-gint? kchar)
  (is-species? kchar sp_gint))

(define (is-forest-goblin-shaman? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_wizard)))

(define (is-forest-goblin-hunter? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_warrior)))

;; fixme -- same as forest-goblin-hunter above
(define (is-forest-goblin-stalker? kchar)
  (and (is-species? kchar sp_forest_goblin)
       (is-occ? kchar oc_warrior)))

(define (is-skeletal-warrior? kchar)
  (let ((gob (kobj-gob-data kchar)))
    (and (not (null? gob))
         (eq? (car gob)
              'skeletal-warrior))))

(define (post-guard kguard x y)
  (npcg-set-post! (gob kguard) (list x y))
  kguard)

(define (mk-bull)
  (mk-npc 'bull 8))

(define (is-undead? kchar)
  (or (is-species? kchar sp_skeleton)
      (is-species? kchar sp_lich)))
      
(define (is-snake? kchar)
  (is-species? kchar sp_snake))

(define (is-rat? kchar)
  (or (is-species? kchar sp_rat)
      (is-species? kchar sp_ratling)))

(define (is-sludge-tentacle? kchar)
  (kbeing-is-npc-type? kchar 'sludge-tentacle))

(define (can-fly? kobj)
  (eqv? (kern-obj-get-movecost kobj pclass-canfly) norm))

(define (can-phase? kobj)
  (eqv? (kern-obj-get-mmode kobj)
        mmode-phase))

(define (is-abstract? kobj)
  (null? (kern-obj-get-name kobj)))
