;;----------------------------------------------------------------------------
;; gregor.scm - read-only data for Gregor the Charcoal Burner
;;----------------------------------------------------------------------------
;;----------------------------------------------------------------------------
;; Schedule
;; 
;; At the shrine gate (moongate-clearing.scm)
;; His home is Gregor's Hut (gregors-hut.scm).
;;----------------------------------------------------------------------------
(kern-mk-sched 'sch_gregor
               (list 0  0  gh-gregors-bed   "sleeping")
               (list 6  0  gh-graveyard     "idle")
               (list 7  0  mgc-roadbend     "idle")
               (list 13 0  gh-table-2       "eating")
               (list 14 0  gh-pasture       "working")
               (list 17 0  gh-table-2       "eating")
               (list 18 0  gh-living-room   "idle")
               (list 20 0  gh-gregors-bed   "sleeping")
               )

;;----------------------------------------------------------------------------
;; Gob
(define (gregor-mk) (list (mk-quest)))
(define (gregor-quest gob) (car gob))


;;----------------------------------------------------------------------------
;; Conv
;; 
;; Gregor is an elderly charcoal burner, living near the Shrine Gate.
;; He tends the grounds of the shrine, and takes care of his grandaughter Ilya.
;; 
;; Gregor is the first NPC which the player is likely to encounter,
;; and has a variety of helpful responses for the starting character
;; and the first-time player.
;;----------------------------------------------------------------------------

(define (gregor-kill-nate knpc kpc)
  (say knpc "[He points a trembling finger at Nate] I have something for you.")
  (aside kpc 'ch_nate "What does this old fool want?")
  (prompt-for-key)
  (say knpc "[He takes out a scroll] It cost me a lot. I can't read it, but she told me what it says when I bought it.")
  (aside kpc 'ch_nate "Wait... is that a...?")
  (prompt-for-key)
  (say knpc "XEN CORP!")
  (cast-missile-proc knpc ch_nate t_deathball)
  (aside kpc 'ch_nate "Gack!")
  (prompt-for-key)
  (if (equal? kpc ch_nate)
      (kern-conv-end)
      (say knpc "You shouldn't travel with men like that. People might get the wrong idea about you.")
      ))

(define (gregor-hail knpc kpc)
  (if (in-player-party? 'ch_nate)
      (gregor-kill-nate knpc kpc)
      (if (in-inventory? kpc t_letter_from_enchanter)
          (say knpc 
	       "I see you got your stuff, and that letter from the Enchanter."
               " Don't forget to ready your weapons before leaving."
               " It's dangerous out there!")
          (say knpc
               "Welcome, Wanderer. I've been watching for you."
               " There's some things that belong to you, over in yonder cave."
               " Go in where the chest is, open it, and get the things inside."
               " It's all for you.")
	  )))

(define (gregor-camp knpc kpc)
  (say knpc "Use the 'k' key to kamp, err...camp, in the wilderness and heal up."))

(define (gregor-dead knpc kpc)
  (say knpc "Aye, it's a shame. My daughter and her husband were both killed by trogs."))

(define (gregor-hut knpc kpc)
  (say knpc "My hut's in the forest to the South and East. Just myself and my granddaughter living there now."))

(define (gregor-band knpc kpc)
  (let ((quest (gregor-quest (kobj-gob-data knpc))))
    (cond ((quest-accepted? quest)
           (say knpc "Have you found the bandits?")
           (cond ((yes? kpc)
                  (say knpc "The old gods be praised!")
                  (quest-done! quest #t)
                  )
                 (else 
                  (say knpc "Go to Green Tower and ask around about the bandits.")
                  )))
          (else
           (say knpc "Bandits are in the woods. "
                "They robbed me in my own hut. "
                "I tried to fight them, "
                "and now I walk with a limp and a cane.")
           (prompt-for-key)
           (say knpc "I have a granddaughter living with me now. "
                "She's just a little girl, but sometimes bad men don't care about that. "
                "I am afraid of what they will do the next time.")
           (prompt-for-key)
           (say knpc "I hate to ask, but they say Wanderers used to help folks. Will you help me now?")
           (cond ((yes? kpc)
                  (say knpc "Thank you."
                       " When you get your equipment, go to Green Tower."
                       " Ask there about bandits."
                       " Someone may know where to find them.")
                  (quest-data-assign-once 'questentry-bandits)
                  (quest-accepted! quest #t)
                  )
                 (else
                  (say knpc "[He turns away sadly]")
                  (kern-conv-end)
                  ))))))

(define gregor-conv
  (ifc basic-conv

       (reply 'default "Can't help you there.")

       (react 'hail
	      (if (in-player-party? 'ch_nate)
		  (gregor-kill-nate knpc kpc)
		  (if (in-inventory? kpc t_letter_from_enchanter)
		      (say knpc 
			   "I see you got your stuff, and that letter from the Enchanter."
			   " Don't forget to ready your weapons before leaving."
			   " It's dangerous out there!")
		      (say knpc
			   "Welcome, Wanderer. I've been watching for you."
			   " There's some things that belong to you, over in yonder cave."
			   " Go in where the chest is, open it, and get the things inside."
			   " It's all for you.")
		      )))
       (reply 'health "[cough] Well enough, my granddaughter helps take care of me.")

       (react 'bye
	      (let ((quest (gregor-quest (kobj-gob-data knpc))))
		(cond ((quest-accepted? quest)
		       (say knpc "Farewell, and be careful."))
		      (else
		       (say knpc "Wait! Before you go, I have a favor to ask you.")
		       (prompt-for-key)
		       (gregor-band knpc kpc)
		       ))))

       (reply 'job "I'm a charcoal burner. I also care for this shrine.")
       (reply 'join "Nope. Already got a job.")
       (reply 'name "Gregor's my name.")
       (reply 'cave "There, that little trail that leads off the main path to the South and West."
	      " Follow it on in. Open the chest. Get the stuff. "
	      " Come back and we'll talk again, if you have more questions.")
       (reply 'ches "Go ahead and open it and get the stuff inside.")
       (reply 'stuf "The common folk made offerings in the cave, thinking one day a Wanderer might come again.")

       (reply 'dang "Very dangerous! If you need healing, a town inn is the safest place."
	      " You can camp in the wilderness but it's dangerous when you're alone and have no one to keep watch. "
	      " Of course, there are spells and potions for healing, too.")

       (method 'camp gregor-camp)
       (method 'kamp gregor-camp) ;; A synonym

       (method 'band gregor-band)

       (reply 'leav "If you want to leave just follow the trail south and step off the map.")
       (reply 'char "I take charcoal into town and sell it, and some folks come by my place to buy it.")
       (reply 'daug "Aye, she was a near-witch like her mother."
	      " Had the knack, but not enough to be among the Wise.")
       (method 'dead gregor-dead)

       (react 'ench 
	      (quest-data-assign-once 'questentry-calltoarms)
	      (say knpc "The Enchanter is one of the Wise."
		   " He told me to look out for a Wanderer like you."
		   " If I saw one I was to send him his way. "
		   " He lives in the White Tower.")
	      (quest-wise-subinit 'questentry-enchanter)
	      (quest-data-update 'questentry-enchanter 'common 1))

       (reply 'folk "There's homesteads scattered about in "
	      "the woods and the foothills.")
       (reply 'fore "Stay out of the deep woods. "
	      "Bandits, spiders and worse live there.")
       (reply 'gate "No one can predict when it will open, "
                                             "or if anything will come through if it does. "
                                             "I've heard of other gates in other parts of the land, "
                                             "and stories tell of others long forgotten now.")
       (reply 'gran "I've a granddaughter name of Ilya.")
       (reply 'help "There's always folks who need help. "
                                             "These are hard times in a hard land.")
       (reply 'hill "Trogs are always a threat in the foothills, "
                                             "but more so of late.")
       (reply 'husb "My son-in-law was a simple farmer. "
                                             "Why the trogs attacked I don't know. "
                                             "Maybe they were driven out of the hills "
                                             "by something else.")
       (method 'hut gregor-hut)
       (reply 'ilya "Yep. She lives at my place now that her parents are dead.")

       (react 'offe 
	      (say knpc "There in the cave you'll find a chest. "
		   "Take what's inside. Wanderers enter this world with little, "
		   "and in the past some have done great good, "
		   "so folks leave stuff in good will for the next one.")
	      (quest-data-update 'questentry-whereami 'wanderer 2))

       (method 'pare gregor-dead)
       (method 'plac gregor-hut)

       (react 'shar 
	      (say knpc "The Shard?  That's what we call this land, Wanderer.")
	      (quest-data-update 'questentry-whereami 'shard 1))

       (react 'shri
	      (say knpc "This shrine is for those who come through the gate. "
		   "Wanderers like yourself. "
		   "Folks leave simple offerings here to help you on "
		   "your journey.")
	      (quest-data-update 'questentry-whereami 'wanderer 1))

       (reply 'spid
	      "Some spiders in the deep woods are monstrous -- big as oxen!"
	      " Children of Angriss, we call those.")
       (reply 'angr
	      "Angriss, Mother of all wood spiders."
	      " She's just a legend to scare the kids to keep them out of the woods.")
       (reply 'trog
	      "Trogs eat us, if they can."
	      " Even crack the bones and suck the marrow."
	      " Nothing left to bury.")
       (react 'wand 
	      (say knpc "We call those who come through the gate Wanderers. "
		   "No one knows where they come from or where they go. "
		   "You are the first to come through in a long, long time.")
	      (quest-data-update 'questentry-whereami 'wanderer 1))
       (reply 'wise
	      "The Wise are both strong and -mostly- good."
	      " They help the land, as they can, "
	      " and keep the Accursed at bay.")
       (reply 'accu
	      "The Accursed? People say they trade their souls for power."
	      " If not for the Wise they would overrun the Shard.")     
       
       (reply 'witc "Don't know of any witches in these parts any more, unless you count Hackle.")
       (reply 'hack
	      "Hackle is a mad old woman who lives in Bole."
	      " Some say she's a witch."
	      " Just crazy, I say.")
       ))

(define (mk-gregor)
  (bind 
   (kern-char-set-description
   (kern-mk-char
    'ch_gregor ; tag
    "Gregor"              ; name
    sp_human            ; species
    nil                 ; occ
    s_old_townsman          ; sprite
    faction-men         ; starting alignment
    0 10 5              ; str/int/dex
    0 0                 ; hp mod/mult
    0 0                 ; mp mod/mult
    max-health -1 max-health 0 2  ; hp/xp/mp/AP_per_turn/lvl
    #f                  ; dead
    'gregor-conv        ; conv
    sch_gregor          ; sched
    'townsman-ai                 ; special ai
    nil                 ; container
    (list t_axe
	  t_armor_leather
	  )              ; readied
    )
   "grizzled old peasant")
   (gregor-mk)
   ))
