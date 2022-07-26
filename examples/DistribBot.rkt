#lang racket/base

#|
DistribBot - a bot distributable program

This is an example bot using the define-program macro.
The define-macro generates a command-line stub
to be easily compile and distributable for sharing purposes.
The stub generated is equivalent to

(compose run-bot define-bot)

Except it creates intermediaries that allow the CLI
to configure the host/port/idfile/arguments
made to the SSH connection, so when writing a bot
using this, you don't need to define those values
inside the define-program macro. You can check
the output of the define-program macro using the
`expand` function.

Invoke this bot with

$ racket examples/DistribBot.rkt -H <your_host> --port 2022
|#


(require "../scb.rkt")

(define-program DistribBot
  (on-pm (lambda (usr msg writer st)
          (writer (pm usr "Hello!")))))

; end
