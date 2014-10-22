(kern-mk-sound 'sound-door-open "sounds/door-open.ogg")
(kern-mk-sound 'sound-door-close "sounds/door-close.ogg")
(kern-mk-sound 'sound-damage "sounds/damage.wav")
(kern-mk-sound 'sound-walking "sounds/walking.wav")
(kern-mk-sound 'sound-splashing "sounds/splashing.wav")
(kern-mk-sound 'sound-squishing "sounds/squish.ogg") ;; fixme
(kern-mk-sound 'sound-moongate-enter "sounds/moongate-enter.wav")
(kern-mk-sound 'sound-cannon-fire "sounds/cannon.wav")
(kern-mk-sound 'sound-clock "sounds/clock.wav")
(kern-mk-sound 'sound-clock-chime "sounds/clock-chime.wav")
(kern-mk-sound 'sound-river "sounds/river.ogg")
(kern-mk-sound 'sound-wind "sounds/wind.wav")
(kern-mk-sound 'sound-missile "sounds/swish.wav")
(kern-mk-sound 'sound-explode "sounds/boom.wav")
(kern-mk-sound 'sound-lightning "sounds/lightning.wav")
(kern-mk-sound 'sound-fireblast "sounds/fireblast.wav")
(kern-mk-sound 'sound-chest-open "sounds/chest-open.ogg")
(kern-mk-sound 'sound-get "sounds/get.ogg")
(kern-mk-sound 'sound-drop "sounds/drop.ogg")
(kern-mk-sound 'sound-money "sounds/money.ogg")
(kern-mk-sound 'sound-get-food "sounds/mmm.ogg")
(kern-mk-sound 'sound-get-drink "sounds/swallow.ogg")
(kern-mk-sound 'sound-ready "sounds/ready.ogg")
(kern-mk-sound 'sound-unready "sounds/unready.ogg")
(kern-mk-sound 'sound-portcullis-open "sounds/portcullis.ogg")
(kern-mk-sound 'sound-portcullis-close "sounds/portcullis.ogg")

(kern-mk-sound 'fanfare-quest-assigned
	       "music/audionetwork/ANW1857_28_The-Flag-13-(Sting).wav")
(kern-mk-sound 'fanfare-quest-updated
	       "music/audionetwork/ANW1520_66_This-Glorious-Land-9-(Sting).wav")

;; Aliases
(define sound-ship-move sound-splashing)

;; ambient sound 'object'
(define ambience-ifc
  (ifc '()
       (method 'exec
	       (lambda (ksound)
		 (kern-sound-play-ambient (eval (gob ksound))
					  (kern-obj-get-location ksound))
		 ))
       (method 'on-entry
	       (lambda (ksound)
		 (kern-sound-play-ambient (eval (gob ksound))
					  (kern-obj-get-location ksound))
		 ))
       ))

(mk-obj-type 't_ambience nil
             '()
             layer-mechanism ambience-ifc)

(define (mk-ambient-sound soundtag)
  (let ((ksound (kern-mk-obj t_ambience 1)))
    (bind ksound soundtag)
    ksound))

;;==========================================================================
;; music

(define default-music "music/audionetwork/ANW1082_10_Princes-In-The-Tower.mp3")

;; Helper to create lists of tracks
(define (music-list . entries)
  (if (null? entries)
      (list default-music)
      entries))

;; The track lists go here:
(load "music/music.scm")

;; Initialize music track list to empty. Initialize flags.
(define music nil)
(define music-playing? #f)
(define music-for-combat? #f)

;; Clear the current track list.
(define (music-reset!)
  (println "music-reset!")
  (set! music nil)
  (set! music-playing? #f)
  )

;; Start a track.
(define (music-play track)
  (println "music-play:" track)
  (kern-music-play track)
  (set! music-playing? #t)
  )

;; Add a track to the end of the list.
(define (music-append! track)
  (println "music-append! " track)
  (if (not (null? track))
      (if music-playing?
	  (set! music (append music (list track)))
	  (music-play track)
	  )))

;; Pull the next track from the head of the list.
(define (music-dequeue!)
  (println "music-dequeue!")
  (let ((top (car music)))
    (set! music (cdr music))
    top))

;; Randomly choose when to start another track.
(define (music-idle-timeout?)
  (= 0 (modulo (random-next) 2000)))

;; Callback from kernel. Called every tick while there are no music tracks
;; playing.
(define (on-music-done kplayer)
  (set! music-playing? #f)
  (if (not (null? music))
      (music-play (music-dequeue!))
      (if (music-idle-timeout?)
	  (let ((kplace (player-member-loc)))
	    (if (notnull? kplace)
		(let ((mgob (place-mgob kplace)))
		  (music-append! (music-select (mgob-normal mgob)))
		  ))))))

(kern-add-hook 'music_change_hook 'on-music-done)


;;==============================================================================
;; interactive music handler
;;
;; Most places should have a t_sounddata object and an on-entry hook that
;; activates it. The (mk-place-music ...) method is the simplest way to set
;; this up
;;
;; The t_sounddata object has 4 lists of music tracks:
;;   1. Ambient music to play when there is no combat
;;   2. Fanfares to play at the start of combat
;;   3. Music to play during combat
;;   4. Fanfare to play at the end of combat
;;
;; The (mk-place-music ...) method takes the first list as an argument and uses
;; standard music lists for the remaining three.

(define (mgob-new normal engagement combat victory)
  (list #t normal engagement combat victory))
(define (mgob-not-in-combat? mgob) (car mgob))
(define (mgob-not-in-combat! mgob v) (set-car! mgob v))
(define (mgob-normal mgob) (list-ref mgob 1))
(define (mgob-engagement mgob) (list-ref mgob 2))
(define (mgob-combat mgob) (list-ref mgob 3))
(define (mgob-victory mgob) (list-ref mgob 4))


(mk-obj-type 't_sounddata nil nil layer-none nil)


;; Given a symbol representing a list of music tracks, randomly select one.
(define (music-select symbol-list)
  (random-select (safe-eval symbol-list)))


;; Interrupt the current music, queuing up the fanfare and combat soundtrack.
(define (music-start-combat mgob)
  (println "music-start-combat:" mgob)
  (music-reset!)
  (set! music-for-combat? #t)
  (music-append! (music-select (mgob-engagement mgob)))
  (music-append! (music-select (mgob-combat mgob)))
  )

;; Interrupt the current music, queuing up a normal track.
;; FIXME: using the normal track
(define (music-start-defeat mgob)
  (println "music-start-defeat:" mgob)
  (music-reset!)
  (set! music-for-combat? #f)
  (music-append! (music-select (mgob-normal mgob)))
  )

(define (music-start-victory mgob)
  (println "music-start-victory:" mgob)
  (music-reset!)
  (set! music-for-combat? #f)
  (music-append! (music-select (mgob-victory mgob)))
  )

;; Get the music gob for a place
(define (place-mgob kplace)
  (let ((kobjs (kplace-get-objects-of-type kplace t_sounddata)))
    (cond ((null? kobjs) nil)
	  (else (gob (car kobjs))))))


;; use kern-set-combat-state-listener to call this
;; do it on system startup too (kern-set-gamestart-hook)
(define (music-on-combat-change kplayer event)
  (println "music-on-combat-change:" event)
  (let ((kplace (player-member-loc)))
    (if (notnull? kplace)
	(let* ((mgob (place-mgob kplace)))
	  (cond ((equal? event 'start)
		 (music-start-combat mgob)
		 )
		((equal? event 'victory)
		 (music-start-victory mgob)
		 )
		((equal? event 'defeat)
		 (music-start-defeat mgob)
		))))))

(define (music-on-place-entry kplace kplayer)
  (println "music-on-place-entry")
  (if (not music-playing?)
      (let* ((mgob (place-mgob kplace))
	     )
	(if (not (null? mgob))
	    (begin
	      ;; Only play ambient music if nothing is already playing.
	      (println "nothing playing => start")
	      (println "mgob:" mgob)
	      (music-append! (music-select (mgob-normal mgob)))
	      )))))

(define (music-on-session-start kplayer)
  (println "music-on-session-start")
  (let ((kplace (player-member-loc)))
    (if (notnull? kplace)
	(music-on-place-entry kplace kplayer)
	)))


;; use this to make data object
(define (mk-sounddata normal engagement combat victory)
  (bind (kern-obj-set-visible (kern-mk-obj t_sounddata 1) #f)
	(mgob-new normal engagement combat victory)
	))


;; normal combat music entries
(define (mk-basic-musicdata noncombatml)
  (mk-sounddata noncombatml 'ml-battle-intro 'ml-battle-music 'ml-battle-over))


;; world music entries dont use combat stuff
(define (mk-world-musicdata noncombatml)
  (mk-sounddata noncombatml nil noncombatml nil))


;; do-it-all method- adds an object and the hook to a place
(define (mk-place-music kplace noncombatml)
  (kern-obj-put-at (mk-basic-musicdata noncombatml) (list kplace 0 0))
  (kern-place-add-on-entry-hook kplace 'music-on-place-entry))

