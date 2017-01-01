#lang racket
(require plot)
(parameterize ([plot-x-transform  log-transform]
               [plot-x-ticks      (log-ticks #:base 2)]
               [plot-y-transform  log-transform]
               [plot-y-ticks      (log-ticks #:base 2)])
  (plot
   #:x-min 1 #:x-max 3000
   #:y-min 1 #:y-max 3000
   (list
    (lines #:color 1
            '(#(16 16)
              #(17 25)
              #(20 26)
              #(24 29)
              #(28 31)
              #(32 35) ; 20 with shared implementation & type, 22 shrd impl only
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
              ))
    ;; with shared implementation & type:
    (lines #:color 2
            '(#(16 11)
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
              #(512 1317)
              ))
    ;; further optimisations
    (lines #:color 3
            '(#(16 10)
              #(17 12)
              #(20 13)
              #(24 13)
              #(28 14)
              #(32 15)
              #(33 22)
              #(40 24)
              #(48 26)
              #(56 28)
              #(64 30)
              #(65 49)
              #(80 54)
              #(96 57)
              #(128 69)
              #(129 129)
              #(256 186)
              #(257 372)
              #(512 587)
              )))))

