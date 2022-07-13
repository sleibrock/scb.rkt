#lang racket/base

#|
This is an example prototype program.
Showcases ideas used to create the bot
and prototype new functionality.
|#

(require "main.rkt")


; Define our basic connection information
(define conf
  (make-config #:host "0.0.0.0"
               #:user "ExBot"
               #:port "2022"
               ))


(define bot 0)


(run-bot bot)

; end
