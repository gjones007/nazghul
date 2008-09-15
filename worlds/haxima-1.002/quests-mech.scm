(define (quest-assign-always target)
	#t)
	
(define (quest-status-from-payload quest)
	"In progress"
	)

(define (quest-status-inprogress quest)
	"In progress"
	)

(define (quest-data-get title)
	(let* ((questdata (tbl-get (gob (kern-get-player)) 'questdata))
			(questinprog (quest-get title))
			)
		(if (null? questinprog)
			(tbl-get questdata title)
			questinprog
		)
	))

;; assuming quest uses a tbl for payload, updates a key/value	
(define (quest-data-update title key value)
	(tbl-set! (car (qst-payload (quest-data-get title))) key value) 
	)

(define (quest-data-descr! title descr)
	(qst-set-descr! (quest-data-get title) descr)
	)

(define (quest-data-icon! title icon)
	(qst-set-icon! (quest-data-get title) icon)
	)

	
;;----------------------------------------------------
;; this is a collection place for updates to quests
;;
		
(define (quest-whereami-update)
	(let* ((quest (quest-data-get "Where am I?"))
			(quest-tbl (car (qst-payload quest)))
			(qp-shard (tbl-get quest-tbl 'shard))
			(qp-wanderer (tbl-get quest-tbl 'wanderer)))
		(qst-set-descr! quest
			(append
;;1 where
(cond ((null? qp-shard)
		(list
			"You have found yourself in a world you have no"
			"knowledge of, with barest impressions of what"
			"might have gone before."
			""
		))
	((equal? 1 qp-shard)
		(list
			"You have found yourself in a world you have no"
			"knowledge of. The inhabitants refer to it as"
			"the Shard."
			""
		))
	(#t (list
			"You have found yourself on the Shard, a small"
			"fragment of a world, that floats surrounded by"
			"a great void."
			""
		))
	)


;; how
(if (and (null? qp-wanderer) (null? qp-shard))
		"Where are you?"
		nil
	)
			
(cond ((null? qp-wanderer)
		(list
			"How and why are you here?"
			"And what are you going to do now?"
		))
	((equal? 1 qp-wanderer)
		(list
			"Others like you have in the past been found"
			"stumbling into this world. The inhabitants"
			"know you as 'Wanderers'."
			""
			"Now you are here, what are you going to do?"
		))
	(#t (list
			"Wanderers like yourself, who are occasionally"
			"stumble upon this world, have in the past been"
			"responsible for great deeds."
			""
			"How will you make your place?"
		))
	)

				)
			)
		)
	)
