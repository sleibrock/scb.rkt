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
  (debug-mode)
  (command "!hello"
           (λ (msg writer ST) (void))))
  
; Run the bot with the run-bot function
(run-bot ExBot)

; end
