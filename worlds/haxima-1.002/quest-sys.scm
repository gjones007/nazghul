;; Defines the basic stuff for the haxima quest system
;;
;; Create a new quest.
;;
;; `title` is a string that will be shown in the quest log listing and at the
;; top of the quest pane
;;
;; `tag` is an optional tag (preferably unique) that can be used to retrieve
;; the quest.
;;
;; `descr` is a list of strings (i.e., a paragraph) that will be shown in the
;; quest pane
;;
;; `assign` is an optional symbol[1] for a procedure that will run when the
;; quest is assigned. of the form `(assign quest target)`, where `quest` is the
;; thing being created right here and `target` is the (scheme) object the quest
;; is being assigned to. Iff `assign` returns #t then the quest will be added
;; to the target's list of quests.
;;
;; `status` is an optional symbol [1] for a procedure that will be called by
;; the ztats pane, of the form `(status quest)`, when the quest details are
;; shown in the quest log.  It is called before the description is written, so
;; it may alter that if required.  The method should return a list of strings
;; to be appended to the description, or nil. Note that this should not be used
;; to update the icon or inprog/done/failed status, as they are used in the
;; preceeding panel.
;;
;; `icon` is a symbol [1] for the sprite to use for the quest UI
;;
;; `payload` is whatever you want for your particular quest (this is an
;; optional number of parms)
;;
;; (* optional = use nil to ignore)
;;
;; Example:
;;
;;   (qst-mk
;;     "Find 6 Foozles"
;;     'q_find_foozles
;;     (kern-ui-paginate-text 
;;       "Find 6 Foozles and Mr. Squeejie will give you an enchanted toothpick."
;;	 "" 
;;       "Seek them out in distant Foozleburg"
;;     )
;;     'find-foozle-assign
;;     'find-foozle-status
;;     's_quest_foozles
;;     0 ; payload tracks num foozles found so far
;;     )
;;
;; Notes:
;;
;; [1] The symbol of a procedure named foo is 'foo. You must use a symbol
;; because the name of the procedure must be saved with the game. It would be
;; nice if you could just pass in a lambda, but saving and reloading lambda
;; closures is left as an exercise for the advanced reader. BTW, this rule
;; applies within the payload lists as well.
;;

;; Constructor.
(define (qst-mk title tag descr assign status icon . payload)
  (if (or (not (symbol? assign))
          (not (symbol? status)))
      (error (string-append "qst-mk: 'assign' and 'status' must be the"
			    " symbols for procedures (ie, not the procedures"
			    " themselves)")))
  (list 'quest title tag descr assign status 'inprogress icon payload))

;; Accessors.
(define (qst-title qst) (list-ref qst 1))
(define (qst-tag qst) (list-ref qst 2))
(define (qst-descr qst) (list-ref qst 3))
(define (qst-done? qst) (list-ref qst 6))
(define (qst-complete? qst) (equal? (list-ref qst 6) 'complete))
(define (qst-failed? qst) (equal? (list-ref qst 6) 'failed))
(define (qst-icon qst) (list-ref qst 7))
(define (qst-payload qst) (list-ref qst 8))

;; Mutators.
(define (qst-set-title! qst title) (list-set-ref! qst 1 title))
(define (qst-set-descr! qst descr) (list-set-ref! qst 3 descr))
(define (qst-set-icon! qst icon) (list-set-ref! qst 7 icon))
(define (qst-done! qst result)
  (if (not (equal? (list-ref qst 6) result))
      (begin
	(list-set-ref! qst 6 result)
	(qst-bump! qst))))
(define (qst-complete! qst) (qst-done! qst 'complete))
(define (qst-failed! qst) (qst-done! qst 'failed))

;; bump the quest to the top of its appropriate list
(define (qst-bump! quest)
  (define (qst-bump-base! qst)
    (if (quest-assigned? qst)
	(begin
	  (quest-remove qst)
	  (quest-insert qst))))
  ;; if we have a parent quest, bump that first
  (let ((parent (quest-tbl-get quest 'qparent)))
    (if (not (null? parent))
	(let ((pqst (quest-get parent)))
	  (if (not (null? pqst))
	      (qst-bump! pqst)))))
  (qst-bump-base! quest)
  ;; if we have children, bump them
  (let ((childlist (quest-tbl-get quest 'qchildren)))
    (map (lambda (entry)
	   (let ((cqst (quest-get entry)))
	     (if (not (null? cqst))
		 (qst-bump-base! cqst))))
	 childlist)))

;; Applicators.
(define (qst-assign qst target) 
  (apply (eval (list-ref qst 4)) 
         (list qst target)))

(define (qst-status qst)
  (let ((statfn (list-ref qst 5)))
    (if (not (null? statfn))
	(apply (eval statfn) (list qst)))))


;; Higher-level layer.
(define (quest-assign qst)
  (let ((target (gob (kern-get-player))))
    (if (and (notnull? qst)
             (notnull? target)
             (qst-assign qst target))
        (begin
	  (quest-insert qst)
          ))))

(define (quest-assigned? qst)
  (let* ((target (gob (kern-get-player)))
	 (qstlist (tbl-get target 'quests)))
    (if (or (null? qst)
	    (null? qstlist))
	#f
	(in-list? qst qstlist))))


(define (quest-get tag)
  (safe-car
   (filter 
    (lambda (quest) (eq? (qst-tag quest) tag))
    (tbl-get (gob (kern-get-player)) 'quests)
    )
   ))

(define (quest-remove qst)
  ;; (cons a nil) = a; (cons nil b) != b;
  (define (quest-remove-helper qstlist)
    (if (null? qstlist) nil
	(let ((qhead (safe-car qstlist)))
	  (if (eq? qhead qst)
	      (cdr qstlist)
	      (cons
	       qhead
	       (quest-remove-helper (cdr qstlist))
	       )
	      )
	  )
	))
  (let* ((target (gob (kern-get-player)))
	 (trimmed  (quest-remove-helper (tbl-get target 'quests) qst))
	 )
    (if (null? trimmed)
	(tbl-rm! target 'quests)
	(tbl-set! target 'quests trimmed)
	)
    ))


(define (quest-insert qst)
  (let* ((target (gob (kern-get-player)))
	 (targlist (tbl-get target 'quests))
	 (inserttype (qst-done? qst))
	 (parent (quest-tbl-get qst 'qparent))
	 )
    (define (insert-here? testee)
      (cond ((eq? inserttype 'inprogress) #t)
	    ((eq? inserttype (qst-done? testee)) #t)
	    ((eq? 'failed (qst-done? testee)) #t)
	    (#t #f))
      )
    (define (quest-insert-helper qstlist)
      (if (null? qstlist) (list qst)
	  (let ((qhead (safe-car qstlist)))
	    (if (insert-here? qhead)
		(cons qst qstlist)
		(cons
		 qhead
		 (quest-insert-helper (cdr qstlist))
		 )
		)
	    )
	  ))
    (define (quest-insertchild-helper qstlist)
      (if (null? qstlist) (list qst)
	  (let ((qhead (safe-car qstlist)))
	    (if (or (not (equal? parent (quest-tbl-get qhead 'qparent)))
		    (insert-here? qhead))
		(cons qst qstlist)
		(cons
		 qhead
		 (quest-insertchild-helper (cdr qstlist))
		 )
		)
	    )
	  ))
    (define (quest-insert-findparent qstlist)
      (if (null? qstlist) (nil)
	  (let ((qhead (safe-car qstlist)))
	    (if (equal? parent (qst-tag qhead))
		(cons
		 qhead 
		 (quest-insertchild-helper (cdr qstlist))
		 )
		(cons
		 qhead
		 (quest-insert-findparent (cdr qstlist))
		 )
		)
	    )
	  ))
    (cond ((null? targlist) (tbl-append! target 'quests qst))
	  ((null? parent) (tbl-set! target 'quests
				    (quest-insert-helper targlist)))
	  (#t (tbl-set! target 'quests (quest-insert-findparent targlist)))
	  )
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some special handling for quests with tbl payloads

(define (quest-tbl? quest)
  (let ((qpayload (qst-payload quest)))
    (cond ((not (pair? qpayload)) #f)
	  ((not (pair? (car qpayload))) #f)
	  (#t (is-tbl? (car qpayload)))
	  )
    ))

(define (quest-tbl-get quest tag)
  (let ((qpayload (qst-payload quest)))
    (cond ((not (pair? qpayload)) nil)
	  ((not (pair? (car qpayload))) nil)
	  ((not (is-tbl? (car qpayload))) nil)
	  (#t (tbl-get (car qpayload) tag))
	  )
    ))
