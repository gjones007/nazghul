(load "naz.scm")
(kern-load "game.scm")
(load "tests/basic-time.scm")
(load "tests/test-map-1.scm")

(define conv-a
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Dunno.")))
       (method 'hail (lambda (knpc kpc) (say knpc "Hi.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Bye.")))
       (method 'job (lambda (knpc kpc) (say knpc "Following you around.")))
       (method 'name (lambda (knpc kpc) (say knpc "Forget me already?")))
       ))

(define conv-b
  (ifc basic-conv
       ;; default if the only "keyword" which may (indeed must!) be longer than
       ;; 4 characters. The 4-char limit arises from the kernel's practice of
       ;; truncating all player queries to the first four characters. Default,
       ;; on the other hand, is a feature of the ifc mechanism (see ifc.scm).
       (method 'default (lambda (knpc kpc) (say knpc "Huh?")))
       (method 'hail (lambda (knpc kpc) (say knpc "Howdy.")))
       (method 'bye (lambda (knpc kpc) (say knpc "Sigh-yo-nara!")))
       (method 'job (lambda (knpc kpc) (say knpc "Whatever you like.")))
       (method 'name (lambda (knpc kpc) (say knpc "Call me Slim.")))
       ))

(kern-mk-place 'p_test
               "Test Place"
               nil          ; sprite
               m_test_1
               #f  ;; wraps
               #f  ;; underground
               #f  ;; wilderness
               #f  ;; tmp combat place
               nil ;; subplaces
               nil ;; neighbors
               ;; objects:
               (list
                (list (kern-mk-char 'ch_shroom ; tag
                                    "Shroom"              ; name
                                    sp_human              ; species
                                    oc_druid              ; occ
                                    s_companion_druid     ; sprite
                                    align-town            ; starting alignment
                                    0 10 0                ; str/int/dex
                                    0 0                   ; hp mod/mult
                                    0 0                   ; mp mod/mult
                                    0 0                   ; hit mod def mod
                                    0 0                   ; dam mod arm mod
                                    30 0 9 9              ; hp/xp/mp/lvl
                                    'conv-a          ; conv
                                    nil            ; sched
                                    nil                   ; special ai
                                    (list t_dagger))      ; readied
                      9 9)

                (list (kern-mk-char 'ch_broom ; tag
                                    "Broom"              ; name
                                    sp_human              ; species
                                    oc_druid              ; occ
                                    s_companion_druid     ; sprite
                                    align-town            ; starting alignment
                                    0 10 0                ; str/int/dex
                                    0 0                   ; hp mod/mult
                                    0 0                   ; mp mod/mult
                                    0 0                   ; hit mod def mod
                                    0 0                   ; dam mod arm mod
                                    30 0 9 9              ; hp/xp/mp/lvl
                                    'conv-a          ; conv
                                    nil            ; sched
                                    nil                   ; special ai
                                    (list t_dagger))      ; readied
                      10 9)

                (list (kern-mk-char 'ch_olin ; tag
                                    "Olin the Ghast"      ; name
                                    sp_ghast              ; species
                                    nil                   ; occ
                                    s_ghost               ; sprite
                                    align-town            ; starting alignment
                                    0 10 2                ; str/int/dex
                                    0 1                   ; hp mod/mult
                                    10 5                  ; mp mod/mult
                                    0 0                   ; hit mod def mod
                                    0 0                   ; dam mod arm mod
                                    240 0 8 8             ; hp/xp/mp/lvl
                                    'conv-b          ; conv
                                    nil                   ; sched
                                    nil                   ; special ai
                                    nil)                  ; readied
                      8 9)
                )
               nil ;; hooks
               )

(kern-mk-player 'player                     ; tag
                s_companion_fighter         ; sprite
                "Walk"                      ; movement description
                sound-walking               ; movement sound
                1000                        ; food
                500                         ; gold
                nil                         ; formation
                nil                         ; campsite map
                nil                         ; campsite formation
                nil                         ; vehicle
                ;; inventory
                (kern-mk-container
                 nil ;; type
                 nil ;; trap
                 ;; contents:
                 (list
                  (list 1 t_poison_immunity_potion)
                  (list 1 the-goblin-lexicon)
                  )

                 )
                nil ;; party members
                )

(kern-party-add-member player ch_olin)
(kern-party-add-member player ch_shroom)
(kern-party-add-member player ch_broom)
