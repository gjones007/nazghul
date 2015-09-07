;; A sign logs a message when the player handles it.

(define sign-ifc
  (ifc '()
       (method 'xamine 
	       (lambda (ksign khandler)
		 (let ((sign (kobj-gob-data ksign)))
		   (println "sign:" sign)
		   (kern-log-begin "+--------\n")
		   (for-each (lambda (msg) (kern-log-continue "|" msg "\n"))
			     sign)
		   (kern-log-end "+--------")
		   )))
       ))

(mk-obj-type 't_road_sign
	     "sign"
	     s_road_sign
	     layer-tfeat
	     sign-ifc)

(define (mk-road-sign . msg)
  (bind (kern-mk-obj t_road_sign 1)
	msg))
