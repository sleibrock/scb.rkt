#lang racket/base

#|
Rootkicker - a way of removing users with the name 'root'

Sometimes people like to come in from a system that they
are currently the 'root' user of, which means their username
on their current system becomes their name on ssh-chat.

People do this frequently enough that it might be a nuisance
to have people constantly come in named 'root' and never
change their nicknames.

This is a bot to kick people named root so they are then
forced to change their nickname on connecting.
|#


(require "../scb.rkt")

(define-bot Rootkicker
  (host "0.0.0.0")
  (port "2022")

  ; use the on-join event to kick a user out
  ; this requires OP privileges from the admin user
  ; to use properly, connect Rootkicker, then /op Rootkicker
  (on-join
   (λ (usr writer _)
     (when (string=? "root" usr)
       (writer (kick usr "Come back with a different name"))))))

(run-bot Rootkicker)

; end 
