;;----------------------------------------------------------------------------
;; Abe is a scholar who knows much of the runes. He fled from Absalot with the
;; Alchemist, and now lives in Green Tower.
;; ----------------------------------------------------------------------------

(let ((bed gt-abe-bed)
      (mealplace gt-ws-tbl2)
      (workplace gt-ruins)
      (leisureplace gt-ws-hall)
      )
  (kern-mk-sched 'sch_abe
		 (list 0  0 bed          "sleeping")
		 (list 7  0 mealplace    "eating")
		 (list 8  0 workplace    "working")
		 (list 12 0 mealplace    "eating")
		 (list 13 0 workplace    "working")
		 (list 18 0 mealplace    "eating")
		 (list 19 0 leisureplace "idle")
		 (list 22 0 bed          "sleeping")
		 ))


(define abe-conv
  (ifc green-tower-conv

       (reply 'default "I'll look that up in the archives when I get a chance.")

       (react 'hail
	      (if (kern-char-is-known? knpc)
		  (say knpc "Hello again.")
		  (begin
		    (kern-char-set-known knpc #t)
		    (say knpc "Hello. Say, aren't you a Wanderer?")
		    (if (yes? kpc)
			(say knpc "I am most honored!"
			     " I can't believe my good fortune."
			     " I have so many questions for you."
			     " When you get the time."
			     " If you don't mind.")
			(say knpc "Oh, of course not."
			     " Sorry. I just thought... never mind.")))))

       (reply 'bye "Let me know if you find any ruins!")

       (react 'job
	      (say knpc "I'm a scholar."
		   " I'm studying the ruins here in Green Tower."
		   " Have you examined them?")
	      (if (no? kpc)
		  (say knpc "They're in the southwest corner of town."
		       " Fascinating.")
		  (begin
		    (say knpc "Did you know there are more below the surface?")
		    (yes? kpc)
		    (say knpc "Yes, just like Absalot!"))))

       (reply 'name "Oh. Yes. I'm Abe.")

       (reply 'join "Oh, no, I couldn't possibly..."
	      " I'm not really that sort of person.")

       (reply 'absa
	      "Not many know that beneath Absalot is an older city."
	      " The ruins there are similar to the ruins here in Green Tower."
	      " I am certain they were built by the same civilization!")

       (react 'rune
	      (if (any-in-inventory? kpc rune-types)
		  (begin
		    (say knpc "[He whistles softly]"
			 " You have some of the Eight Keys to the Demon Gate?"
			 " I shall examine them for you!")
		    (quest-data-update 'questentry-runeinfo 'abe 1)
		    (quest-data-update-with 'questentry-runeinfo 'keys 1
					    (quest-notify
					     (grant-party-xp-fn 20)))
		    (if (any-in-inventory? kpc (list t_rune_k))
			(say knpc
			     "[He examines a rune] This is the"
			     " Rune of Knowledge!")
			)
		    (if (any-in-inventory? kpc (list t_rune_p))
			(say knpc "[He examines a rune] This is the"
			     " Rune of Power!")
			)
		    (if (any-in-inventory? kpc (list t_rune_s))
			(say knpc "[He examines a rune] This is the"
			     " Rune of Skill!")
			)
		    (if (any-in-inventory? kpc (list t_rune_c))
			(say knpc "[He examines a rune] This is the"
			     " Rune of Curiousity!")
			)
		    (if (any-in-inventory? kpc (list t_rune_f))
			(say knpc "[He examines a rune] This is the"
			     " Rune of Freedom!")
			)
		    (if (any-in-inventory? kpc (list t_rune_w))
			(say knpc "[He examines a rune] This is the"
			     " Rune of Wisdom!")
			)
		    (if (any-in-inventory? kpc (list t_rune_d))
			(say knpc "[He examines a rune] This is the"
			     " Rune of Discretion!")
			)
		    (if (any-in-inventory? kpc (list t_rune_l))
			(say knpc "[He examines a rune] This is the"
			     " Rune of Leadership!")
			)
		    (if (has-all-runes? kpc)
			(say knpc
			     "This is incredible!"
			     " You have all Eight Keys to the Demon Gate!!!"
			     " What do you intend to do with them?")
			)
		    )
		  (say knpc "There are many runes."
		       " Perhaps if you brought me an example...?")))

       (react 'gate
	      (say knpc "The Demon Gate was sealed shut by the Wise long ago."
		   " Its location was blotted from all records,"
		   " but legend puts it somewhere to the north. ")
	      (quest-data-update 'questentry-runeinfo 'abe 1)
	      (quest-data-update 'questentry-runeinfo 'keys 1)
	      (quest-data-update-with 'questentry-runeinfo 'gate 1
				      (quest-notify (grant-party-xp-fn 30)))
	      )

       (reply 'keys
	      "The Demon Gate was locked with eight locks,"
	      " and the keys were separated."
	      " Each key takes the form of a powerful rune."
	      " They've been lost or hidden for a long time.")

       (react 'eigh
	      (say knpc "Legend says that there are eight runes in all,"
		   " are you seeking the others?")
	      (if (yes? kpc)
		  (say knpc "The old stories speak of some,"
		       " such as King Clovis's charm, or the Void Temple.")
		  (say knpc "Just idle curiosity? Believe me, I understand."))
	      )

       (reply 'quee "I'm not sure what you are insinuating.")

       (react 'clov
	      (say knpc
		   "There's a legend that King Clovis carried one as a charm."
		   " He fell in battle during the goblin wars,"
		   " but it was never found on his body."
		   " Perhaps someone looted the battlefield and took it."
		   " It might even have been a goblin!.")
	      (quest-data-assign-once 'questentry-rune-f)
	      )
       
       (react 'temp
	      (say knpc "Somewhere in the Void there is a temple."
		   " No one can reach it now,"
		   " but legend says a rune was sealed there by the ancients.")
	      (quest-data-assign-once 'questentry-rune-d)
	      )

       (react 'void
	      (say knpc "The Shard, the moons and the stars all habitate a"
		   " great void."
		   " The ancients could sail across the void in ships,"
		   " the way we sail across a sea!")
	      (quest-data-update 'questentry-whereami 'shard 2)
	      )

       (react 'civi
	      (say knpc
		   "I don't know much about the people that built the ruins,"
		   " but clues indicate they were quite wicked!"
		   " Do you know what I mean?")
	      (if (yes? kpc)
		  (say knpc "Then I won't mention it!")
		  (say knpc "Human sacrifice, cannibalism, demon worship."
		       " Accursed practices!"))
	      )

       (reply 'accu
	      "Yes, the Accursed have a long history."
	      " They may be a convenient political bogeyman, "
	      " but there is enough evidence to suggest they do, or did,"
	      " exist.")

       (react 'sail
	      (say knpc "I know of the void ships, but not how they worked."
		   " Even the Master Wrights have lost the technique for"
		   " making them.")
	      (quest-data-update 'questentry-whereami 'shard 2)
	      )

       (reply 'wrig "Wrights specialize in the making of things."
	      " The Engineer is the greatest living Wright.")

       (reply 'alch "How is the secretive old rascal?"
	      " I haven't seen him since we were neighbors.")

       (reply 'neig "In Absalot. Before we had to flee.")

       (reply 'flee "[He sighs] It's a long story. Ask around."
	      " It doesn't matter anymore.")

       (reply 'gobl "Deric and Gen have a lot of experience with goblins.")

       ))

(define (mk-abe)
  (mk-townsman '((name . "Abe")
		 (desc . "bookish young man")
		 (sprite . s_companion_wizard)
		 (species . sp_human)
		 (faction . faction-men)
		 (tag  . 'ch_abe)
		 (lvl . 3)
		 (str . 2)
		 (int . 1)
		 (dex . 1)
		 (conv . 'abe-conv)
		 (sched . sch_abe)
		 (ai . 'townsman-ai)
		 (arms . (t_staff t_armor_leather))
		 (ctor . tbl-mk)
		 )))
