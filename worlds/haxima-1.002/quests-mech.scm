;; Sets a quest to be complete, with notification if it is in progress
;; Note that if you set the quest to be complete before you assign it,
;; then the assignment notification will say that it has been immediately
;; completed, avoiding spamming the player with multiple notifications

(define (quest-complete quest)
  (if (and (quest-assigned? quest) 
	   use-quest-pane
	   (not (qst-complete? quest)))
      (kern-log-msg "^c+mQuest completed:^c-\n^c+m" (qst-title quest) "^c-")
      )
  (qst-complete! quest)
  )

(define (quest-complete? quest)
  (and (quest-assigned? quest) 
       use-quest-pane
       (qst-complete? quest)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; internal utility methods

(define (quest-data-add-child parent quest)
  (let ((childlist (quest-data-getvalue parent 'qchildren)))
    (if (not (in-list? quest childlist))
	(quest-data-update parent 'qchildren
			   (cons
			    quest
			    childlist
			    )
			   ))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quest assignment callbacks for use in quest definition

;; Called when a quest is assigned.
(define (quest-assign-notify quest target)
  (let ((notifytext (if (qst-complete? quest)
			"^c+mQuest completed:^c-\n^c+m"
			"^c+mNew quest:^c-\n^c+m"
			)))
    (println "quest-assign-notify")
    (kern-sound-play fanfare-quest-assigned)
    (kern-log-msg notifytext (qst-title quest) "^c-")
    #t))

;; ensures parent/subquest relation once quest is assigned
(define (quest-assign-subquest quest target)
  (let ((parent (quest-tbl-get quest 'qparent)))
    (if (not (null? parent))
	(quest-data-add-child parent (qst-tag quest))
	)
    #t
    ))

;; allows quest to proceed without any other action
(define (quest-assign-silent quest target)
  #t
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; quest display callbacks for use in quest definition

;; doesnt actually do anything
(define (quest-status-from-payload quest)
  "In progress"
  )

;; doesnt actually do anything
(define (quest-status-inprogress quest)
  "In progress"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interacting with the Quest Data Table
;;
;; The quest data table is a global storage location for fixed, plot
;; based quests that are created once and then activated at the
;; appropriate time
;;
;; Anything procedurally generated on the fly would need
;; to interface directly with the quest-sys module.
;;

;; retrieves a quest from the quest data table
(define (quest-data-get tag)
  (let* ((questdata (tbl-get (gob (kern-get-player)) 'questdata)))
    (tbl-get questdata tag)))

;; retrieves a value from a quest payload tbl, given the key for the quest
;; and for the value
(define (quest-data-getvalue quest tag)
  (let* ((qpayload (car (qst-payload (quest-data-get quest)))))
    (tbl-get qpayload tag)))

;; assigns a quest from the quest data table, while ensuring it is not
;; given out repeatedly
(define (quest-data-assign-once tag)
  (let ((questentry (quest-data-get tag)))
    (if (not (quest-assigned? questentry))
	(quest-assign questentry))))

;; checks if a quest from the quest data table has been assigned
(define (quest-data-assigned? tag)
  (quest-assigned? (quest-data-get tag)))

;; checks if a quest from the quest data table has been assigned and completed
(define (quest-data-complete? tag)
  (quest-complete? (quest-data-get tag)))

;; assuming quest in the QDT uses a tbl for payload, updates a key/value pair
(define (quest-data-update tag key value)
  (println "quest-data-update:" tag "," key "," value)
  (println "  quest-data:" (quest-data-get tag))
  (println "  payload:" (qst-payload (quest-data-get tag)))
  (let* ((qpayload (car (qst-payload (quest-data-get tag))))
	 (updatehook (tbl-get qpayload 'on-update)))
    (if (not (equal? (tbl-get qpayload key) value))
	(begin
	  (tbl-set! qpayload key value)
	  (if (quest-data-assigned? tag)
	      (kern-sound-play fanfare-quest-updated))
	  (if (not (null? updatehook))
		((eval updatehook)))
	  (qst-bump! (quest-data-get tag))))))

;; updates as per quest-data-update, but additionally triggers a passed in
;; function
(define (quest-data-update-with tag key value callback)
  (let* ((quest (quest-data-get tag))
         (qpayload (car (qst-payload quest))))
    (if (is-tbl? qpayload)
        (let ((updatehook (tbl-get qpayload 'on-update)))
          (if (not (equal? (tbl-get qpayload key) value))
              (begin			
                (tbl-set! qpayload key value)
                (callback quest)
		(if (quest-data-assigned? tag)
		    (kern-sound-play fanfare-quest-updated))
                (if (not (null? updatehook))
		      ((eval updatehook)))
                (qst-bump! (quest-data-get tag))))))))


;; sets the description for a quest in the QDT
(define (quest-data-descr! tag descr)
  (qst-set-descr! (quest-data-get tag) descr)
  )

;; sets the icon for a quest in the QDT
(define (quest-data-icon! tag icon)
  (qst-set-icon! (quest-data-get tag) icon)
  )	

;; sets a quest in the QDT to be complete, giving a notification if appropriate
;;    see the notes for quest-complete, above
(define (quest-data-complete tag)
  (quest-complete (quest-data-get tag))
  )

(define (quest-data-complete-with tag key value callback)
  (quest-data-update-with tag key value callback)
  (quest-complete (quest-data-get tag)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;; callbacks for quest-data-update-with

;; if appropriate, notifies the player about a change in quest state
;; can be chained to further functions
(define (quest-notify subfunction)
  (lambda (quest) 
    (if (and (quest-assigned? quest) use-quest-pane)
	(kern-log-msg "^c+mQuest updated:^c-\n^c+m" (qst-title quest) "^c-")
	)
    (if (not (null? subfunction))
	(subfunction quest))
    ))

;; grants the player a given amount of experience, using or adding to the bonus xp
;;          as appropriate
(define (grant-xp-fn amount)
  (lambda (quest) 
    (let* ((qpayload (car (qst-payload quest)))
	   (bonusxp (tbl-get qpayload 'bonus-xp))
	   (bonusxp (if (null? bonusxp)
			0 bonusxp))
	   (totalxp (+ bonusxp amount))
	   )
      (if (quest-assigned? quest)
	  (begin
	    (kern-char-add-experience (car (kern-party-get-members (kern-get-player))) totalxp)
	    (tbl-set! qpayload 'bonus-xp 0)
	    )
	  (tbl-set! qpayload 'bonus-xp totalxp)
	  )
      )
    ))

;; shares amongst the players party a given amount of experience,
;;       using or adding to the bonus xp as appropriate
(define (grant-party-xp-fn amount)
  (lambda (quest) 
    (let* ((qpayload (car (qst-payload quest)))
	   (bonusxp (tbl-get qpayload 'bonus-xp))
	   (bonusxp (if (null? bonusxp)
			0 bonusxp))
	   (totalxp (+ bonusxp amount))
	   (party (kern-party-get-members (kern-get-player)))
	   (xp-each (ceiling (/ totalxp (length party))))
	   )
      (if (quest-assigned? quest)
	  (begin
	    (map (lambda (kchar) (kern-char-add-experience kchar xp-each)) party)
	    (tbl-set! qpayload 'bonus-xp 0)
	    )
	  (tbl-set! qpayload 'bonus-xp totalxp)
	  )
      )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reconcile active and pregenned quests at game load to simplify
;; ingame tracking
;;
;; internal methods- will run automatically

(kern-add-hook 'new_game_start_hook 'reconcile-quests)
(kern-add-hook 'new_game_start_hook 'refresh-quests)

(define (reconcile-quests kplayer)
  (let ((questlist
         (tbl-get (gob
                   (kern-get-player)) 'quests))
        (questdata
         (tbl-get (gob 
                   (kern-get-player)) 'questdata))
        )
    (map 
     (lambda (quest)
       (let ((tag (qst-tag quest)))
         (if (and (not (null? tag))
                  (not (null? (tbl-get questdata tag))))
             (tbl-set! questdata tag quest))
         ))
     questlist)
    ))

(define (refresh-quests)
  (load "quests-data.scm"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; utilities

;; links a quest and subquest after they are already in-play
(define (quest-data-convert-subquest quest parent)
  (quest-data-update quest 'qparent parent)
  (quest-data-add-child parent quest))