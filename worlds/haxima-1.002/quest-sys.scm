;; Defines the basic stuff for the haxima quest system

;; Create a new quest.
;;
;; title - a string that will be shown in the quest log listing and at the top
;; of the quest pane
;;
;; tag - an optional tag (preferably unique) that can be used to retrieve the quest.
;;
;; descr - a list of strings (ie paragraph) that will be shown in the quest pane
;;
;; assign - an optional symbol[1] for a proc that will run when the quest is assigned;
;; of the form (assign quest target), where 'quest' is the thing being created right
;; here and 'target' is the (scheme) object the quest is being assigned to. Iff
;; 'assign' returns #t then the quest will be added to the target's list of
;; quests.
;;
;; status - an optional symbol [1] for a proc that will be called by the ztats pane, of
;; the form (status quest), when the quest details are shown in the quest log.
;; It is called before the description is written, so it may alter that if required.
;; The method should return a list of strings to be appended to the description, or nil
;; Note that this should not be used to update the icon or inprog/done/failed status, as
;; they are used in the preceeding panel.
;;
;; icon - symbol [1] for sprite to use for the quest UI
;;
;; payload - whatever you want for your particular quest (this is an optional
;; number of parms)
;;
;; (* optional = use nil to ignore)
;;
;; Example:
;;
;;   (qst-mk "Find 6 Foozles" 
;;           '( 
;;              "If you find 6 Foozles, Mr. Squeejie will give you an enchanted toothpick."
;;				"" 
;;              "Seek them out in distant Foozleburg"
;;            )
;;           'find-foozle-assign
;;           'find-foozle-status
;;			 's_quest_foozles
;;           0 ; payload tracks num foozles found so far
;;           )
;;
;; Notes:
;;
;; [1] The symbol of a proc named foo is 'foo. You must use a symbol because
;; the name of the procedure must be saved as part of an object's gob. It would
;; be nice if you could just pass in a lambda, but saving and reloading lambda
;; closures is left as an exercise for the advanced reader. BTW, this rule
;; applies within the payload lists as well.
;;
(define (qst-mk title tag descr assign status icon . payload)
  (if (or (not (symbol? assign))
          (not (symbol? status)))
      (error "qst-mk: 'assign' and 'status' must be the symbols for procedures (ie, not the procedures themselves)"))
  (list 'quest title tag descr assign status 'inprogress icon payload))
  
(define (qst-title qst) (list-ref qst 1))

(define (qst-tag qst) (list-ref qst 2))

(define (qst-descr qst) (list-ref qst 3))

(define (qst-assign qst target) 
  (println "qst-assign")
  (apply (eval (list-ref qst 4)) 
         (list qst target)))

(define (qst-status qst)
  (apply (eval (list-ref qst 5))
         (list qst)))

(define (qst-done? qst)
  (println "qst-done? qst=" qst)
  (list-ref qst 6))
  
(define (qst-done! qst result)
  ;;(kern-log-msg "^c+gYou have completed the quest ^c+w" (qst-title qst) "^c-!^c-")
  (list-set-ref! qst 6 result))

(define (qst-complete? qst)
	(equal? (list-ref qst 6) 'complete))
	
(define (qst-complete! qst)
  (list-set-ref! qst 6 'complete))
  
(define (qst-failed? qst)
	(equal? (list-ref qst 6) 'failed))

(define (qst-failed! qst)
  (list-set-ref! qst 6 'failed))
	
(define (qst-icon qst) (list-ref qst 7))
  
(define (qst-payload qst) (list-ref qst 8))

(define (quest-assign qst)
  (println "quest-assign")
  (let ((target (gob (kern-get-player))))
    (if (and (notnull? qst)
             (notnull? target)
             (qst-assign qst target))
        (begin
          (tbl-append! target 'quests qst)
          (println "quest-assign: " target)
          ;;(kern-log-msg "^c+gYou have a new quest: " (qst-title qst) "^c-")
          ))))
      
(define (head alist)
	(cond ((null? alist)
		nil)
		((pair? alist)
		(car alist))
		(#t alist))) 
          
(define (quest-get tag)
	(let* (
		(qlst (tbl-get (gob (kern-get-player)) 'quests))
		(matchlist (if (null? qlst) nil
			(filter (lambda (quest) (eq? (qst-tag quest) title)) qlst)))
		)
		(head matchlist)
	))

;; (cons a nil) = a; (cons nil b) != b;
(define (quest-remove-helper qstlist removee)
	(if (null? qstlist) nil
		(let ((qhead (head qstlist)))
			(println "rem? " (eq? qhead removee) " " )
			(if (eq? qhead removee)
				(if (pair? qstlist) 
					(quest-remove-helper (cdr qstlist) removee)
					nil)
				(cons
					qhead
   					(if (pair? qstlist) 
						(quest-remove-helper (cdr qstlist) removee)
						nil)
					)
				)
			)
	))

(define (quest-remove qst)
	(let* ((target (gob (kern-get-player)))
			(trimmed  (quest-remove-helper (tbl-get target 'quests) qst))
			)
		(if (null? trimmed)
			(tbl-rm! target 'quests)
			(tbl-set! target 'quests trimmed)
			)
	))
	
(define (qst-set-title! qst title) (list-set-ref! qst 1 title))
(define (qst-set-descr! qst descr) (list-set-ref! qst 3 descr))
(define (qst-set-icon! qst icon) (list-set-ref! qst 6 icon))

