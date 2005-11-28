;;----------------------------------------------------------------------------
;; Ability "class"
;;----------------------------------------------------------------------------
(define (mk-ability name level mana ap proc)
  (list name level mana ap proc))

(define (ability-name ability) (car ability))
(define (ability-level-required ability) (cadr ability))
(define (ability-mana-cost ability) (caddr ability))
(define (ability-ap-cost ability) (cadddr ability))
(define (ability-proc ability) (list-ref ability 4))

(define (can-use-ability? ability kchar)
  (println " can-use-ability?" display ability)
  (and (>= (kern-char-get-mana kchar)
           (ability-mana-cost ability))
       (>= (kern-char-get-level kchar)
           (ability-level-required ability))))

(define (use-ability ability kchar . args)
  (println "use-ability:" ability)
  (kern-char-dec-mana kchar (ability-mana-cost ability))
  (kern-obj-dec-ap kchar (ability-ap-cost ability))
  (let ((result (apply (ability-proc ability) (cons kchar args))))
    (if (<= (kern-char-get-mana kchar))
        (kern-log-msg (kern-obj-get-name kchar) " is exhausted!"))
    result))


;;----------------------------------------------------------------------------
;; Ability procedures
;;----------------------------------------------------------------------------

(define (vampiric-touch-proc kchar ktarg)
  (let ((amount (min (* (kern-dice-roll "1d3")
                        (kern-char-get-level kchar))
                     (kern-char-get-hp ktarg))))
    (kern-obj-inc-ref ktarg)
    (kern-obj-apply-damage ktarg "life drained" amount)
    (kern-obj-heal kchar amount)
    (kern-log-msg (kern-obj-get-name kchar)
                  " drains life from "
                  (kern-obj-get-name ktarg)
                  "!")
    (kern-obj-dec-ref ktarg))
  #t)

(define (disease-touch-proc kchar ktarg)
  (if (kern-obj-add-effect ktarg ef_disease nil)
      (kern-log-msg (kern-obj-get-name kchar)
                    " inflicts "
                    (kern-obj-get-name ktarg)
                    " with Disease!"))
  #t)

(define (disarm kchar ktarg)
  (let ((readied (kern-char-get-readied-weapons ktarg)))
    (if (null? readied)
        #f
        (if (> (kern-char-get-level kchar)
               (+ (kern-dice-roll "1d3-1")
                  (kern-char-get-level ktarg)))
            (let ((ktype (random-select readied)))
              (kern-log-msg (kern-obj-get-name kchar)
                            " disarms "
                            (kern-obj-get-name ktarg))
              (kern-char-unready ktarg ktype)
              (kern-obj-remove-from-inventory ktarg ktype 1)
              (kern-obj-add-to-inventory kchar ktype 1))
            (kern-log-msg  (kern-obj-get-name kchar)
                           " fails to disarm "
                           (kern-obj-get-name ktarg))
            #t))))

(define (heal-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a healing spell on "
                (if (eqv? kchar ktarg)
                    "self"
                    (kern-obj-get-name ktarg)))
  (kern-obj-heal ktarg (kern-dice-roll "1d20+5")))

(define (great-heal-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a great healing spell on "
                (if (eqv? kchar ktarg)
                    "self"
                    (kern-obj-get-name ktarg)))
  (kern-obj-heal ktarg (kern-dice-roll "4d20+20")))

;;----------------------------------------------------------------------------
;; field spells
(define (cast-field-proc kchar loc ktype)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts a field spell")
  (kern-obj-put-at (kern-mk-obj ktype 1) loc))
  
(define (cast-fire-field-proc kchar ktarg)
  (cast-field-proc kchar 
                   (kern-obj-get-location ktarg)
                   F_fire))
  
(define (cast-poison-field-proc kchar ktarg)
  (cast-field-proc kchar 
                   (kern-obj-get-location ktarg)
                   F_poison))
  
(define (cast-sleep-field-proc kchar ktarg)
  (cast-field-proc kchar 
                   (kern-obj-get-location ktarg)
                   F_sleep))
  
(define (cast-energy-field-proc kchar ktarg)
  (cast-field-proc kchar 
                   (kern-obj-get-location ktarg)
                   F_energy))
  
;;----------------------------------------------------------------------------
;; missile spells
(define (cast-missile-proc kchar ktarg ktype)
  (kern-fire-missile ktype
                     (kern-obj-get-location kchar)
                     (kern-obj-get-location ktarg)))
  
;; cast-magic-missile-proc -- damage goes up with level of caster
(define (cast-magic-missile-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " fires magic missile at "
                (kern-obj-get-name ktarg))
  (if (cast-missile-proc kchar ktarg t_arrow)
      (kern-obj-apply-damage ktarg
                             "ouch"
                             (* (kern-dice-roll "1d3")
                                (kern-char-get-level kchar)))))

(define (cast-poison-missile-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " hurls poison missile at "
                (kern-obj-get-name ktarg))
  (cast-missile-proc kchar ktarg t_poison_bolt))

(define (cast-fireball-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " hurls fireball at "
                (kern-obj-get-name ktarg))
  (cast-missile-proc kchar ktarg t_fireball))

(define (cast-kill-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " casts kill at "
                (kern-obj-get-name ktarg))
  (cast-missile-proc kchar ktarg t_deathball))

(define (cast-acid-missile-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " hurls acid missile at "
                (kern-obj-get-name ktarg))
  (cast-missile-proc kchar ktarg t_acid_bolt))

(define (web-spew-proc kchar ktarg)
  (kern-log-msg (kern-obj-get-name kchar)
                " spews web at "
                (kern-obj-get-name ktarg))
  (define (spew-in-dir dir)
    (define (ensnare-loc loc)
      (kern-obj-put-at (kern-mk-obj web-type 1) loc))
    (let ((loc (kern-obj-get-location kchar)))
      (cast-wind-spell2 loc
                        ensnare-loc
                        dir
                        (kern-char-get-level kchar))))
  (let* ((v (loc-diff (kern-obj-get-location ktarg)
                      (kern-obj-get-location kchar)))
         (dir (loc-to-cardinal-dir v)))
    (spew-in-dir dir)))

(define (teleport-proc kchar loc)
  (kern-log-msg (kern-obj-get-name kchar)
                " teleports")
  (kern-obj-relocate kchar loc nil))

;;----------------------------------------------------------------------------
;; summoning
(define (cast-summon-proc kchar gen-npct)
  (println "cast-summon-proc")
  (define (run-loop count)
    (println " run-loop " count)
    (cond ((<= count 0) 0)
          (else
           (let* ((lvl (+ (kern-dice-roll "1d2") (/ (kern-char-get-level kchar) 2)))
                  (knpc (spawn-npc (gen-npct) lvl))
                  (loc (pick-loc (kern-obj-get-location kchar) knpc))
                  )
             (println " loc=" loc)
             (cond ((null? loc) 
                    (kern-obj-dec-ref knpc)
                    0)
                   (else
                    (kern-being-set-base-faction knpc (kern-being-get-base-faction kchar))
                    (kern-obj-set-temporary knpc #t)
                    (kern-obj-put-at knpc loc)
                    (+ 1 (run-loop (- count 1)))))))))
  (cond ((> (run-loop (/ (kern-char-get-level kchar) 2))
            0)
         (kern-log-msg (kern-obj-get-name kchar) " summons help")
         #t)
        (else
         (kern-log-msg (kern-obj-get-name kchar) " fails to summon help")
         #f)))

(define (summon-skeleton-proc kchar)
  ;;(println "summon-skeleton-proc")
  (cast-summon-proc kchar
                    (lambda () 
                      (random-select (list 'skeletal-warrior 'skeletal-spear-thrower)))))
                    
(define (summon-slime-proc kchar)
  (println "summon-slime-proc")
  (cast-summon-proc kchar
                     (lambda () 'green-slime)))

;;----------------------------------------------------------------------------
;; Ability declarations
;;----------------------------------------------------------------------------

(define vampiric-touch      (mk-ability "vampiric touch" 3 3 2 vampiric-touch-proc))
(define disease-touch       (mk-ability "disease touch" 6 6 1 disease-touch-proc))
(define disarm              (mk-ability "disarm" 4 2 2 disarm))
(define heal-ability        (mk-ability "heal" 1 1 1 heal-proc))
(define great-heal-ability  (mk-ability "great heal" 4 4 2 great-heal-proc))
(define cast-fire-field     (mk-ability "cast fire field" 3 3 2 cast-fire-field-proc))
(define cast-poison-field   (mk-ability "cast poison field" 3 3 2 cast-poison-field-proc))
(define cast-sleep-field    (mk-ability "cast sleep field" 3 3 2 cast-sleep-field-proc))
(define cast-energy-field   (mk-ability "cast energy field" 4 4 2 cast-energy-field-proc))
(define cast-magic-missile  (mk-ability "cast magic missile" 1 1 1 cast-magic-missile-proc))
(define cast-poison-missile (mk-ability "cast poison missile" 2 2 1 cast-poison-missile-proc))
(define cast-fireball       (mk-ability "cast fireball" 3 3 1 cast-fireball-proc))
(define cast-kill           (mk-ability "cast kill" 7 7 2 cast-kill-proc))
(define cast-acid-missile   (mk-ability "cast acid missile" 4 4 1 cast-acid-missile-proc))
(define web-spew            (mk-ability "spew web" 4 4 2 web-spew-proc))
(define teleport            (mk-ability "teleport" 6 6 2 teleport-proc))
(define summon-skeleton     (mk-ability "summon skeleton" 6 6 4 summon-skeleton-proc))
(define summon-slimes       (mk-ability "summon slimes"   2 2 4 summon-slime-proc))

;;----------------------------------------------------------------------------
;; Abilities listed by various attributes
;;----------------------------------------------------------------------------

(define melee-spells
  (list cast-fire-field
        cast-sleep-field
        cast-poison-field
        cast-energy-field))

;; ranged-spells -- damaging spells which take a target kchar as an arg.
(define ranged-spells
  (list (cons cast-magic-missile 8)
        (cons cast-poison-missile 6)
        (cons cast-fireball 6)
        (cons cast-kill 4)))
