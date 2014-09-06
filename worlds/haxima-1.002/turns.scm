(define turns-per-minute 5)
(define turns-per-hour (* turns-per-minute 60))
(define turns-per-day (* turns-per-hour 24))
(kern-set-turns-per-minute turns-per-minute)  ;; call before kern-set-clock
