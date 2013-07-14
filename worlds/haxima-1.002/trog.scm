; (define (trog-display . args)
;   (display (kern-get-ticks))
;   (display ":")
;   (apply display args))
; (define (trog-newline) (newline))

(define (trog-display . args) )
(define (trog-newline) )
(define trog-melee-weapon t_horns)

;;----------------------------------------------------------------------------
;; Troll AI
;;----------------------------------------------------------------------------
(define (trog-is-critical? ktrog)
  (< (kern-char-get-hp ktrog) 5))

(define (trog-wander ktrog) 
  (trog-display "trog-wander")(trog-newline)
  (wander ktrog))

(define (trog-flee ktrog) 
  (trog-display "trog-flee")(trog-newline)
  (flee ktrog))

(define (trog-foes-in-weapon-range ktrog karms kfoes)
  (trog-display "trog-foes-in-weapon-range")(trog-newline)
  (all-in-range (kern-obj-get-location ktrog) 
                (kern-arms-type-get-range karms)
                 kfoes))

(define (weaker? a b)
  (< (kern-char-get-hp a) (kern-char-get-hp b)))

(define (trog-pick-target ktrog foes)
  (trog-display "trog-pick-target")(trog-newline)
  (foldr (lambda (a b) (if (weaker? a b) a b))
         (car foes) 
         (cdr foes)))

(define (trog-pathfind-foe ktrog foes)
  (trog-display "trog-pathfind-foe")(trog-newline)
  (let ((ktarg (trog-pick-target ktrog foes)))
    (if (notnull? ktarg)
        (pathfind ktrog (kern-obj-get-location ktarg)))))

(define (trog-attack ktrog karms foes)
  (trog-display "trog-attack")(trog-newline)
  (kern-char-attack ktrog 
                    karms
                    (trog-pick-target ktrog 
                                       foes)))

;; Given an "origin" location and a list of locations, find the location in the
;; list closest to the coordinates.
(define (loc-closest origin lst)
  (if (null? lst) nil
      (foldr (lambda (a b) (if (loc-closer? a b origin) a b))
             (car lst)
             (cdr lst))))

(define (trog-stronger? ktrog foes)
  (> (kern-char-get-strength ktrog)
     (foldr (lambda (a b) (+ a (kern-char-get-strength b))) 
            0 
            foes)))

(define (trog-has-ranged-weapon? ktrog)
  (in-inventory? ktrog trog-ranged-weapon))

;; trog-get-ammo -- give trog a boulder and convert terrain to grass
(define (trog-get-terrain-ammo ktrog coords)
  (trog-display "trog-get-terrain-ammo")(trog-newline)
  (kern-obj-add-to-inventory ktrog trog-ranged-weapon 1)
  (kern-place-set-terrain coords t_grass)
  (kern-map-repaint)
  (kern-obj-dec-ap ktrog trog-ripup-boulder-ap)
  )

;; ----------------------------------------------------------------------------
;; trog-get-loose-ammo -- search the objects at the location for ammo and give
;; it to the th character
;; ----------------------------------------------------------------------------
(define (trog-get-loose-ammo ktrog loc)
  (trog-display "trog-get-loose-ammo")(trog-newline)
  (kobj-get-at ktrog loc trog-ranged-weapon))

;; ----------------------------------------------------------------------------
;; trog-terrain-is-ammo -- true iff the given location's terrain can be
;; converted by a trog into ammo
;; ----------------------------------------------------------------------------
(define (trog-terrain-is-ammo? coords)
  (eqv? t_boulder (kern-place-get-terrain coords)))

;; ----------------------------------------------------------------------------
;; trog-find-nearest-ammo -- return the closest location with ammo objects or
;; with terrain that can be converted to ammo objects.
;; ----------------------------------------------------------------------------
(define (trog-find-nearest-ammo ktrog)
  (trog-display "trog-find-nearest-ammo")(trog-newline)
  (define (scanobjlst lst)
    (foldr (lambda (a b) 
             (or a (eqv? (kern-obj-get-type b) trog-ranged-weapon)))
           #f
           lst))
  (define (check lst loc)
    (if (trog-terrain-is-ammo? loc)
        (cons loc lst)
        (if (scanobjlst (kern-get-objects-at loc))
            (cons loc lst)
            lst)))
  (let* ((loc (kern-obj-get-location ktrog))
         (rad (kern-obj-get-vision-radius ktrog))
         (coords (profile foldr-rect (loc-place loc)
                              (- (loc-x loc) (/ rad 2))
                              (- (loc-y loc) (/ rad 2))
                              (* 1 rad)
                              (* 1 rad)
                              check
                              nil)))
    (trog-display coords)(trog-newline)
    (profile loc-closest loc coords)))

(define (trog-find-nearest-ammo2 ktrog)
  (trog-display "trog-find-nearest-ammo2")(trog-newline)
  (let* ((loc (kern-obj-get-location ktrog))
         (rad (kern-obj-get-vision-radius ktrog))
         (coords (profile kern-search-rect (loc-place loc)
                                   (- (loc-x loc) (/ rad 2))
                                   (- (loc-y loc) (/ rad 2))
                                   (* 1 rad)
                                   (* 1 rad)
                                   t_boulder
                                   trog-ranged-weapon)))
    (profile loc-closest loc coords)))

(define (trog-find-nearest-ammo3 ktrog)
  (trog-display "trog-find-nearest-ammo3")(trog-newline)
  (define (scanobjlst lst)
    (foldr (lambda (a b) 
             (or a (eqv? (kern-obj-get-type b) trog-ranged-weapon)))
           #f
           lst))
  (define (check lst loc)
    (if (trog-terrain-is-ammo? loc)
        (cons loc lst)
        (if (scanobjlst (kern-get-objects-at loc))
            (cons loc lst)
            lst)))
  (let* ((loc (kern-obj-get-location ktrog))
         (rad (kern-obj-get-vision-radius ktrog))
         (coords (profile kern-fold-rect (loc-place loc)
                          (- (loc-x loc) (/ rad 2))
                          (- (loc-y loc) (/ rad 2))
                          (* 1 rad)
                          (* 1 rad)
                          check
                          nil)))
    (trog-display coords)(trog-newline)
    (profile loc-closest loc coords)))

(define (trog-find-nearest-ammo4 ktrog)
  (trog-display "trog-find-nearest-ammo4")(trog-newline)
  (let* ((loc (kern-obj-get-location ktrog))
         (rad (kern-obj-get-vision-radius ktrog))
         (terrain-coords (profile kern-search-rect-for-terrain (loc-place loc)
                                  (- (loc-x loc) (/ rad 2))
                                  (- (loc-y loc) (/ rad 2))
                                  (* 1 rad)
                                  (* 1 rad)
                                  t_boulder))
         (closest-terrain (profile loc-closest loc terrain-coords))
         (obj-coords (profile kern-search-rect-for-obj-type (loc-place loc)
                              (- (loc-x loc) (/ rad 2))
                              (- (loc-y loc) (/ rad 2))
                              (* 1 rad)
                              (* 1 rad)
                              trog-ranged-weapon))
         (closest-obj (profile loc-closest loc obj-coords)))
    (cond ((null? closest-obj) closest-terrain)
          ((null? closest-terrain) closest-obj)
          (else
           (if (loc-closer? closest-obj closest-terrain loc)
               closest-obj
               closest-terrain)))))

;; ----------------------------------------------------------------------------
;; trog-get-ammo -- given the location of an ammo object or terrain that can
;; be converted to ammo, have the trog get the ammo
;; ----------------------------------------------------------------------------
(define (trog-get-ammo ktrog loc)
  (trog-display "trog-get-ammo")(trog-newline)
  (if (trog-terrain-is-ammo? loc)
      (trog-get-terrain-ammo ktrog loc)
      (trog-get-loose-ammo ktrog loc)))

;; ----------------------------------------------------------------------------
;; trog-hunt-for-ammo2 -- find the nearest available ammo and pathfind to it
;; or pick it up. Returns false iff none available.
;; ----------------------------------------------------------------------------
(define (trog-hunt-for-ammo ktrog)
  (trog-display "trog-hunt-for-ammo")(trog-newline)
  (let ((nearest (profile trog-find-nearest-ammo2 ktrog))
        (kloc (kern-obj-get-location ktrog)))
    (trog-display "nearest=")(trog-display nearest)(trog-newline)
    (if (null? nearest)
        #f
        (begin
          (do-or-goto ktrog nearest trog-get-ammo)
          #t))))

(define (trog-display-objs lst)
  (if (null? lst)
      (trog-newline)
      (begin
        (trog-display (kern-obj-get-name (car lst)))
        (trog-display " ")
        (trog-display-objs (cdr lst)))))

(define (trog-no-hostiles ktrog)
  (trog-display "trog-no-hostiles")(trog-newline)
  (trog-wander ktrog))

(define trog-taunts
  (list
   "[primal howl]"
   "[hateful roar]"
   "[raging bellow]"
   ))

(define (trog-taunt ktrog ktarg)
  (taunt ktrog ktarg trog-taunts)
  (npcg-set-taunted! (gob ktrog) #t))

(define (trog-hostiles ktrog foes)
  (trog-display "trog-hostiles")(trog-newline)
  (if (trog-is-critical? ktrog) 
      (trog-flee ktrog)
      (let ((melee-targs (trog-foes-in-weapon-range ktrog 
                                                     trog-melee-weapon 
                                                     foes)))
        (trog-display "trog-ai:melee-targs=")
        (trog-display melee-targs)
        (trog-newline)
        (or (npcg-taunted? (gob ktrog))
            (trog-taunt ktrog (car foes)))
        (if (null? melee-targs)
            (if (trog-has-ranged-weapon? ktrog)
                (let 
                    ((ranged-foes 
                      (trog-foes-in-weapon-range ktrog
                                                  trog-ranged-weapon
                                                  foes)))
                  (trog-display "trog-ai:ranged-foes=")
                  (trog-display ranged-foes)
                  (trog-newline)
                  (if (null? ranged-foes)
                      (trog-pathfind-foe ktrog foes)
                      (trog-attack ktrog trog-ranged-weapon 
                                    ranged-foes)))
                (or (trog-hunt-for-ammo ktrog)
                    (trog-pathfind-foe ktrog foes)))
            (if (trog-stronger? ktrog melee-targs)
                (trog-attack ktrog trog-melee-weapon melee-targs)
                (evade ktrog melee-targs))))))

;; ----------------------------------------------------------------------------
;; trog-ai -- combat ai for a trog npc. Called repeatedly by the kernel on
;; the trog's turn until the trog is out of ap.
;; ----------------------------------------------------------------------------
(define (trog-ai ktrog)
  (trog-display "trog-ai")(trog-newline)
  (let ((foes (all-visible-hostiles ktrog)))
    (if (null? foes)
        (trog-wander ktrog)
        (trog-hostiles ktrog foes))
    #t))
