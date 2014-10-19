;;----------------------------------------------------------------------------
;; Generic conversation
;;----------------------------------------------------------------------------

;; Macro to emit boilerplate for simple reply statements. Turns this:
;;   (reply 'hello "Hello, yourself!")
;; Into this:
;;   (method 'hello (lambda knpc kpc) (say knpc "Hello, yourself!"))
(macro (reply form)
  `(method ,(cadr form) (lambda (knpc kpc) (say knpc ,@(cddr form)))))

;; Macro to emit boilerplate for more complex reply statements. Turns this:
;;   (react 'hello
;;          (if (in-player-party? 'ch_nate)
;;              (say knpc "Well, well, my old nemesis.")
;;              (say knpc "You seen Nate?")))
;; Into:
;;   (method 'hello
;;           (lambda (knpc kpc)
;;             (if (in-player-party? 'ch_nate)
;;                 (say knpc "Well, well, my old nemesis.")
;;                 (say knpc "You seen Nate?"))))
(macro (react form)
  `(method ,(cadr form) (lambda (knpc kpc) ,@(cddr form))))

;; fundamentals
(define (generic-hail knpc kpc)
  (say knpc "Well met"))

(define (generic-unknown knpc kpc)
  (say knpc "I can't help you with that"))

(define (generic-bye knpc kpc)
  (say knpc "Farewell")
  (kern-conv-end))

(define (generic-join knpc kpc)
  (say knpc "I cannot join you."))

(define (generic-leav knpc kpc)
  (cond ((is-player-party-member? knpc)
         (cond ((is-only-living-party-member? knpc)
                (say knpc "Maybe I should resurrect the Wanderer first... "
                     "or sell his body parts to a thaumaturge, at least."))
               (else
                (say knpc "Do you want me to leave your party now?")
                (cond ((yes? kpc)
                       (cond ((leave-player knpc)
                              (say knpc "If you change your mind I'll be here waiting.")
                              (kern-conv-end)
                              )
                             (else 
                              (say knpc "I can't leave right now!"))))
                      (else
                       (say knpc "You made me nervous there for a minute."))))))
         (else
          (say knpc "I'm not a member of your party!"))))

;; wise
(define (basic-ench knpc kpc)
  (say knpc "The Enchanter is the Wise Wizard. "
       "He lives in a white tower by the Fens."))

;; towns

;; Return a string describing distance and direction from the knpc to the
;; kplace.
(define (conv-directions knpc kplace)
  (if (place-location-is-unknown? kplace)
      nil
      (let* ((kfrom (get-place knpc))
	     (from (kern-place-get-location kfrom))
	     (to (kern-place-get-location kplace)))
	(if (null? to)
	    nil
	    (let* ((diff (loc-diff from to))
		   (dir-str (loc-to-dir-string diff))
		   (distance (loc-grid-distance from to))
		   (turns (* distance 32))  ;; 32 is wilderness scale
		   (distance-str (cond ((> turns (* turns-per-hour 18)) "several days")
				       ((> turns (* turns-per-hour 12)) "over a day")
				       ((> turns (* turns-per-hour 8)) "a day")
				       ((> turns (* turns-per-hour 4)) "a half day")
				       ((> turns (* turns-per-hour 2)) "a couple of hours")
				       ((> turns turns-per-hour) "an hour")
				       ((> turns (* turns-per-minute 30)) "half an hour")
				       ((> turns (* turns-per-minute 15)) "a few minutes")
				       (else "nearby"))))
	      (string-append distance-str " to the " dir-str)
	      )))))

;; If kplace is in a region, return a string description.
(define (conv-setting knpc kplace)
  (if (place-location-is-unknown? kplace)
      nil
      (let ((loc (kern-place-get-location kplace)))
	(if (null? loc)
	    nil
	    (let ((region (get-region-by-loc (kern-place-get-location kplace))))
	      (if (null? region)
		  nil
		  (let* ((here
			  (get-region-by-loc
			   (kern-place-get-location
			    (loc-place (kern-obj-get-location knpc)))))
			 (demonstrative (if (equal? here region) " here" " over")))
		    (string-append demonstrative " " (region-preposition region) " "
				   (region-name region))
		    )))))))

;; Combine results of directions and setting.
(define (conv-directions-and-setting knpc kplace)
  (if (equal? (loc-place (kern-obj-get-location knpc))
	      kplace)
      "where you are now"
      (let ((directions (conv-directions knpc kplace))
	    (setting (conv-setting knpc kplace)))
	(if (null? directions)
	    (if (null? setting)
		nil
		(string-append setting))
	    (if (null? setting)
		(string-append directions)
		(string-append directions setting)
		)))))
	    

;; Describe common knowledge about a place and how to get there.
(define (conv-describe-place knpc kpc kplace)
  (let ((name (kern-place-get-name kplace))
	(gob (kern-place-get-gob kplace))
	(dir-and-set (conv-directions-and-setting knpc kplace))
	)
    (if (null? gob)
	(if (null? dir-and-set)
	    (say knpc "I only know rumours.")
	    (say knpc name " is " dir-and-set ".")
	    )
	(let ((description (tbl-get gob 'description)))
	  (if (null? dir-and-set)
	      (say knpc name " is " description ".")
	      (say knpc name " is " description "."
		   " It's " dir-and-set "."
		   ))))))

(define (conv-describe-region knpc kpc name)
  (let* ((r (get-region-by-name name))
	 (d (region-description r))
	 (facts (region-facts r))
	 (f (if (null? facts) "" (random-select facts)))
	)
  (say knpc d " " f)
  ))

;; establishments
(define (basic-whit knpc kpc)
  (say knpc "The White Stag is in Green Tower."))

;; quests
(define (basic-thie knpc kpc)
  (say knpc "No, I don't know anything about a thief."))

(define (basic-rune knpc kpc)
  (say knpc "I don't know much about runes. Try asking one of the Wise."))

(define (basic-wise knpc kpc)
	(say knpc "The Wise have great influence over affairs in the Shard. Do you want to know their names?")
	(if (yes? kpc)
		(begin
			(say knpc "There's the Enchanter, the Necromancer, the Alchemist, the MAN, the Engineer and the Warritrix.")
			(map quest-wise-subinit
				(list 'questentry-enchanter 'questentry-warritrix  'questentry-alchemist
						'questentry-the-man 'questentry-engineer  'questentry-necromancer)
			)
		)
	))

(define (basic-shar knpc kpc)
  (say knpc "The Shard is what we call our world.")
  (quest-data-update 'questentry-whereami 'shard 1)
  )

(define (basic-warr knpc kpc)
  (say knpc "The Warritrix is the Wise Warrior. If you're looking for her try Glasdrin.")
  (quest-wise-subinit 'questentry-warritrix)
  (quest-data-update 'questentry-warritrix 'general-loc 1)
  )

(define (basic-engi knpc kpc)
  (say knpc "I've heard the Engineer is the greatest Wright in the land, "
       "but I don't know much about him.")
       (quest-wise-subinit 'questentry-engineer)
       (quest-data-update 'questentry-engineer 'common 1)
       )

(define (basic-man knpc kpc)
  (say knpc "The MAN is a master wrogue. Nobody knows where his hideout is. "
       "It's rumoured that he travels in disguise.")
       (quest-wise-subinit 'questentry-the-man)
       (quest-data-update 'questentry-the-man 'common 1)
       )

(define (basic-alch knpc kpc)
  (say knpc "The Alchemist is a Wise Wright who specializes in potions. "
       "You'll find his shop in Oparine.")
       (quest-wise-subinit 'questentry-alchemist)
       (quest-data-update 'questentry-alchemist 'general-loc 1)
       )

(define (basic-necr knpc kpc)
  (say knpc "The Necromancer is a Wise Wizard who specializes in death magic. "
       "I've heard he lives in a hidden cave.")
       (quest-wise-subinit 'questentry-necromancer)
       (quest-data-update 'questentry-necromancer 'general-loc 1)
       )

(define (basic-drag knpc kpc)
  (say knpc "Stories say a mighty dragon is terrorizing shipping on the "
       "east coast."))

(define (basic-fire knpc kpc)
  (say knpc "The Fire Sea? That's a volcano on an island off the east coast."))

(define basic-conv
  (ifc '()
       ;; fundamentals
       (method 'hail generic-hail)
       (method 'default generic-unknown)
       (method 'bye generic-bye)
       (method 'join generic-join)
       (method 'leav generic-leav)
       
       ;; wise
       (method 'ench basic-ench)
       (method 'wise basic-wise)
       (method 'warr basic-warr)
       (method 'man basic-man)
       (method 'engi basic-engi)
       (method 'alch basic-alch)
       (method 'necr basic-necr)

       ;; towns & regions
       (method 'absa (lambda (knpc kpc) (conv-describe-place knpc kpc p_absalot)))
       (method 'bole (lambda (knpc kpc) (conv-describe-place knpc kpc p_bole)))
       (method 'gree (lambda (knpc kpc) (conv-describe-place knpc kpc p_green_tower)))
       (method 'trig (lambda (knpc kpc) (conv-describe-place knpc kpc p_trigrave)))
       (method 'opar (lambda (knpc kpc) (conv-describe-place knpc kpc p_oparine)))
       (method 'westpass (lambda (knpc kpc) (conv-describe-place knpc kpc p_westpass)))
       (method 'eastpass (lambda (knpc kpc) (conv-describe-place knpc kpc p_eastpass)))
       (method 'glas (lambda (knpc kpc) (conv-describe-place knpc kpc p_glasdrin)))
       (method 'whit (lambda (knpc kpc) (conv-describe-place knpc kpc p_enchanters_tower)))

       (method 'fens (lambda (knpc kpc) (conv-describe-region knpc kpc "The North Fens")))
       (method 'isme (lambda (knpc kpc) (conv-describe-region knpc kpc "The Ismere Valley")))
       (method 'shar basic-shar)
       (method 'kurp (lambda (knpc kpc) (conv-describe-place knpc kpc p_kurpolis)))
       (method 'fire basic-fire)

       (method 'lost (lambda (knpc kpc) (conv-describe-place knpc kpc p_lost_halls_entrance)))

       ;; establishments
       (method 'whit basic-whit)

       ;; quests
       (method 'thie basic-thie)
       (method 'rune basic-rune)

       ;; monsters
       (method 'drag basic-drag)

       ))

;; Helper(s)
(define (say knpc . msg) (kern-conv-say knpc msg))
(define (yes? kpc) (kern-conv-get-yes-no? kpc))
(define (no? kpc) (not (kern-conv-get-yes-no? kpc)))
(define (reply? kpc) (kern-conv-get-reply kpc))
(define (ask? knpc kpc . msg)
  (kern-conv-say knpc msg)
  (kern-conv-get-yes-no? kpc))
(define (prompt-for-key)
  (kern-log-msg "^c+c<Hit any key to continue>^c-")
  (kern-ui-waitkey))
(define (meet msg)
  (kern-log-msg msg))
(define (get-gold-donation knpc kpc)
  (let ((give (kern-conv-get-amount kpc))
        (have (kern-player-get-gold)))
    (cond ((> give have)
           (say knpc "You don't have that much!")
           0)
          (else
           (kern-player-set-gold (- have give))
           give))))
(define (get-food-donation knpc kpc)
  (let ((give (kern-conv-get-amount kpc))
        (have (kern-player-get-food)))
    (cond ((> give have)
           (say knpc "You don't have that much!")
           0)
          (else
           (kern-player-set-food (- have give))
           give))))
(define (working? knpc)
  (string=? "working" (kern-obj-get-activity knpc)))

;; Not really an aside in the theatrical sense, this routine causes a party
;; member to interject something into the conversation. kpc is the character
;; being conversed with, mem-tag is either nil or the party member who should
;; do the interjection. If mem-tag is nil then a party member (other than the
;; speaker) will be chosen at random. msg is the text of the comment. If kpc is
;; the only member of the party then the aside will not do anything.
(define (aside kpc kchar-tag . msg)
  (if (null? kchar-tag)
      (let ((members (filter (lambda (kchar)
                               (not (eqv? kchar kpc)))
                             (kern-party-get-members (kern-get-player)))
                     ))
        (if (not (null? members))
            (let ((kchar (random-select members)))
              (say kchar msg)
              #t)
            #f)
        )
      (if (in-player-party? kchar-tag)
          (begin
            (kern-conv-say (eval kchar-tag) msg)
            #t)
          #f)
      ))
         
;;----------------------------------------------------------------------------
;; Quests
;;----------------------------------------------------------------------------
(define (mk-quest) (list #f #f #f))
(define (quest-offered? qst) (car qst))
(define (quest-accepted? qst) (cadr qst))
(define (quest-done? qst) (caddr qst))
(define (quest-offered! qst val) (set-car! qst val))
(define (quest-accepted! qst val) (set-car! (cdr qst) val))
(define (quest-done! qst val) (set-car! (cddr qst) val))


;;----------------------------------------------------------------------------
;; Ranger Conversation
;;----------------------------------------------------------------------------
(define (ranger-ranger knpc kpc)
  (say knpc "Rangers guard the borders between wilderness and "
       "civilization. We patrol the frontier and give aid where we can to the "
       "Wise."))

(define (ranger-wise knpc kpc)
  (say knpc "Rangers have an informal alliance with the Wise. They give us "
       "aid and hospitality. We give them news. Sometimes we serve "
       "as messengers and scouts."))

(define (ranger-join knpc kpc)
  (cond ((has? kpc t_ranger_orders 1)
         (say knpc "Let's see those orders... ok. Looks like we're partners "
              "for a while.")
         (take kpc t_ranger_orders 1)
         (join-player knpc)
         ;; NOTE: the following only permits one ranger at a time to join the
         ;; player!
         (kern-tag 'ch_ranger_merc knpc)
         (give kpc t_arrow 20)
         (kern-conv-end)
         )
        (else
         (say knpc "Sorry, I've got to get back to my patrol."))))

(define (ranger-band knpc kpc)
  (say knpc "When men get in trouble with the law, they flee to the woods. "
       "There are always bandits in the forest."))

(define ranger-conv
  (ifc basic-conv
       (method 'job (lambda (knpc kpc) (say knpc "I'm a ranger.")))
       (method 'join ranger-join)
       (method 'rang ranger-ranger)
       (method 'wise ranger-wise)
       (method 'band ranger-band)
       ))


;; Knight conversation -- used by Lord Froederick's troops
(define knight-conv basic-conv)

;; Glasdrin
(define (glasdrin-warr knpc kpc)
  (if (player-found-warritrix?)
      (say knpc "We all mourn her loss.")
      (say knpc "The Warritrix is the most cunning warrior of the age. I'm not sure where she is right now, ask the Steward or Commander Jeffries.")
  	)
  	(quest-data-update 'questentry-warritrix 'general-loc 1)
  )

(define (glasdrin-stew knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "Her name of the Stewardess is a curse among us now. The new Steward is Valus, a former commander.")
      (say knpc "The Steward is the keeper of the city and realms of Glasdrin. You can usually find her in the Citadel.")))

(define (glasdrin-jeff knpc kpc)
  (if (player-stewardess-trial-done?)
      (say knpc "At best Jeffries failed in his duties as commander to protect those under his command. "
           "At worst, he was an accomplice in the betrayal of the Warritrix. "
           "Our new commander is Janice.")
      (say knpc "Jeffries is the commander of the Glasdrin militia. He's usually at work in the Citadel.")
      ))

(define (glasdrin-kurp knpc kpc)
         (say knpc "It's just beyond the western range."))
(define (glasdrin-cita knpc kpc)
  (say knpc "The Citadel is the inner keep in the north part of the castle."))
(define (glasdrin-ghol knpc kpc)
  (say knpc "I seem to recall a man named Gholet was arrested for theft. You might check the Citadel's dungeon.")
   (quest-data-update 'questentry-ghertie 'gholet-dungeon 1)
   )
(define (glasdrin-kurp knpc kpc)
  (say knpc "The dungeon Kurpolis is where most of our troops are now. Follow the mountains west, you'll find the entrance in a canyon."))

(define (glasdrin-glas knpc kpc)
  (say knpc "Glasdrin is the city of the Paladins."))

(define (glasdrin-pala knpc kpc)
  (say knpc "The Paladins of Glasdrin are the greatest military force in the realm."))

(define glasdrin-conv
  (ifc basic-conv
       (method 'warr glasdrin-warr)
       (method 'stew glasdrin-stew)
       (method 'jeff glasdrin-jeff)
       (method 'kurp glasdrin-kurp)
       (method 'cita glasdrin-cita)
       (method 'ghol glasdrin-ghol)
       (method 'kurp glasdrin-kurp)
       (method 'glas glasdrin-glas)
       (method 'pala glasdrin-pala)
       (method 'jani 
               (lambda (knpc kpc) 
                 (if (player-stewardess-trial-done?)
                      (say knpc "The military council has elected Janice to replace Jeffries as commander of the militia.")
                      (say knpc "Jeffries is an able leader, but everyone knows that his assistant Janice has the brains."))))
       (method 'valu
               (lambda (knpc kpc)
                 (if (player-stewardess-trial-done?)
                     (say knpc "Valus was exonerated by the Stewardess's journal. "
                          "It is a shame we ever believed such lies about him. "
                          "We have elected him to be the new Steward.")
                     (say knpc "Valus has been imprisoned for shameful indecency. "
                          "It's a pity, really. He was a well-respected general."))))
       ))

;; Kurpolis
(define kurpolis-conv
  (ifc basic-conv
       ))

;; Green Tower
(define (gt-gobl knpc kpc)
  (say knpc "Since the goblin wars there's been an uneasy truce. Sometimes they trade here in town, but if you meet them in the forest be careful."))
(define (gt-towe knpc kpc)
  (say knpc "The tower that gives this town its name is now the Ranger headquarters."))
(define (gt-ruin knpc kpc)
  (say knpc "The old ruins are in the southwest corner of town."))
(define (gt-band knpc kpc)
  (say knpc "Ask Deric about bandits. "
       "He's the one who should be dealing with them."))


(define green-tower-conv
  (ifc basic-conv
       (method 'gree
               (lambda (knpc kpc)
                 (say knpc "Yes, this town gets its name from the old tower in its center.")))
       (method 'gobl gt-gobl)
       (method 'towe gt-towe)
       (method 'ruin gt-ruin)
       (method 'band gt-band)
       ))

;; Trigrave
(define trigrave-conv
  (ifc basic-conv
       (method 'thie 
               (lambda (knpc kpc) 
                 (say knpc "I don't know anything about a thief. Ask Gwen, maybe a traveler told her something.")))                       
       ))

;;----------------------------------------------------------------------------
;; Merchant

;; Indices into the merchant message list
(define merch-closed           0)
(define merch-buy              1)
(define merch-sell             2)
(define merch-trade            3)
(define merch-sold-something   4)
(define merch-sold-nothing     5)
(define merch-bought-something 6)
(define merch-bought-nothing   7)
(define merch-traded-something 8)
(define merch-traded-nothing   9)

(define (conv-trade knpc kpc menu msgs catalog)
  (if (and (not (string=? "working" (kern-obj-get-activity knpc)))
           (not (null? (list-ref msgs merch-closed))))
      (say knpc (list-ref msgs merch-closed) 
           " I'm " (kern-obj-get-activity knpc) " right now.")
      (cond ((string=? menu "buy")
             (say knpc (list-ref msgs merch-buy))
             (if (kern-conv-trade knpc kpc "buy" catalog)
                 (say knpc (list-ref msgs merch-sold-something))
                 (say knpc (list-ref msgs merch-sold-nothing))))
            ((string=? menu "sell")
             (say knpc (list-ref msgs merch-sell))
             (if (kern-conv-trade knpc kpc "sell" catalog)
                 (say knpc (list-ref msgs merch-bought-something))
                 (say knpc (list-ref msgs merch-bought-nothing))))
            (else
             (say knpc (list-ref msgs merch-trade))
             (if (kern-conv-trade knpc kpc "trade" catalog)
                 (say knpc (list-ref msgs merch-traded-something))
                 (say knpc (list-ref msgs merch-traded-nothing))))
            )))

