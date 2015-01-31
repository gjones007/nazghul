;; A drawbridge terrain feature.
;;
;; A drawbridge is meant to be controlled by a lever or similar mechanism. When
;; turned "on", it looks and is passable like a bridge. When "off" it is
;; impassable and invisible.
;;
;; Example:
;;
;;  (put (kern-tag 'my-drawbridge (mk-drawbridge north)) 32 33)
;;  (put (mk-lever 'my-drawbridge) 33 34)


;; Define a state change handler
(define (drawbridge-state on? kobj)
  (let* ((bim (gob-data (kobj-gob kobj))))
    (if on?
        (state-mk (bim-members bim) #f pclass-bridge 0)
        (state-mk nil #f pclass-none   0))))

;; Extend the bim interface and override the state handler
(define drawbridge-ifc
  (ifc bim-ifc
       (method 'signal bim-toggle)
       (method 'state drawbridge-state)
       ))

;; Make a kernel type
(mk-obj-type 'TF_drawbridge "drawbridge" nil layer-tfeat drawbridge-ifc)

;; Define a constructor
(define (mk-drawbridge dir)
  (if (or (eqv? dir north) (eqv? dir south))
     (bind (kern-mk-obj TF_drawbridge 1)
	   (bim-mk #f '() 's_ns_bridge))
     (bind (kern-mk-obj TF_drawbridge 1)
	   (bim-mk #f '() 's_ew_bridge))))
