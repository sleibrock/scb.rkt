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
  (command "!hello"
           (λ (msg) (void))))
  

(displayln ExBot)

; end
