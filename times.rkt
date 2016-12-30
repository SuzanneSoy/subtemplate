#lang racket
(require plot)
(parameterize ([plot-x-transform  log-transform]
               [plot-x-ticks      (log-ticks #:base 2)]
               [plot-y-transform  log-transform]
               [plot-y-ticks      (log-ticks #:base 2)])
  (plot
   #:x-min 1 #:x-max 3000
   #:y-min 1 #:y-max 3000
   (points '(#(16 16)
             #(17 25)
             #(20 26)
             #(24 29)
             #(28 31)
             #(32 35) ;; 20 with shared implementation & type, 22 shrd impl only
             #(33 60)
             #(40 67)
             #(48 77)
             #(56 80)
             #(64 92) ;; 46
             #(65 168)
             #(80 189)
             #(96 216)
             #(128 276)
             #(129 562)
             #(256 911)
             #(257 2078)
             #(512 3000) ;; rough estimation
             ))))

;; with shared implementation & type:
(parameterize ([plot-x-transform  log-transform]
               [plot-x-ticks      (log-ticks #:base 2)]
               [plot-y-transform  log-transform]
               [plot-y-ticks      (log-ticks #:base 2)])
  (plot
   #:x-min 1 #:x-max 600
   #:y-min 1 #:y-max 600
   (points '(#(16 11)
             ;#(17 25)
             ;#(20 26)
             ;#(24 29)
             ;#(28 31)
             #(32 20)
             ;#(33 60)
             ;#(40 67)
             ;#(48 77)
             ;#(56 80)
             #(64 46)
             ;#(65 168)
             ;#(80 189)
             ;#(96 216)
             #(128 120)
             ;#(129 562)
             #(256 363)
             ;#(257 2078)
             ;#(512 3000) ;; rough estimation
             ))))