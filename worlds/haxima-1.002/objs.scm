;; ----------------------------------------------------------------------------
;; objs.scm -- basic object types and hooks
;;----------------------------------------------------------------------------

;; Make the basic object interface which supports g)et
(define obj-ifc
  (ifc '()
       (method 'get kobj-get)
       (method 'on-drop kobj-on-drop)
       (method 'on-ready
	       (lambda (ktype kobj)
		 (println "on-ready")
		 (kern-sound-play sound-ready)))
       (method 'on-unready
	       (lambda (ktype kobj)
		 (println "on-unready")
		 (kern-sound-play sound-unready)))
       ))
