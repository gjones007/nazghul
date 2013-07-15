(define merciful-death-x 468)
(define merciful-death-y 525)

(define (raise-merciful-death)
  (let ((loc (mk-loc p_hring 
                     merciful-death-x 
                     merciful-death-y)))
  (kern-log-msg "From her watery grave...")
  (kern-log-msg "...THE MERCIFUL DEATH ARISES!")
  (shake-map 10)
  (kern-place-set-subplace p_merciful_death loc)
  (kern-map-set-dirty)
  ))
