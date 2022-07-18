#lang racket/base

#|
This is an example prototype program.
Showcases ideas used to create the bot
and prototype new functionality.
|#

(require "scb.rkt")

(define-bot ExBot
  (host "0.0.0.0")
  (port "2022")

  ; turn on debugging mode
  (debug-mode)
  
  ; define what happens when a user joins
  (on-join (λ (user writer ST)
             (writer (format "Hello ~a!" user))
             ST))

  ; create a basic action to print out Hello
  (command "!hello"
           (λ (msg writer ST) (void))))
  
; Run the bot with the run-bot function
(run-bot ExBot)

; end
