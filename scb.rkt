#lang racket/base


#|
The ssh-chat bot library

ssh-chat is a program designed over the SSH protocol
for users to chat. Bots can be easily created to
interact with users, however there is no stable API
for doing so easily.

A bot can be broken down into several aspects:
* a command-based bot, user invokes commands
* a response-based bot, bots perform actions on messages
* an interval bot, bot performs actions on a schedule

The three aspects define what possibilities a bot will have.
Thus, we provide a stable API for users to create bots with
some degree of simplicity.

The goal of this library is to provide a means to define
a bot, and create the core logic by overriding some functions.
|#


(require (only-in racket/contract -> ->* define/contract and/c or/c any/c)
         (only-in racket/match match)
         )

(provide (struct-out Config)
         (struct-out SshChatBot)
         )


(struct Config (host user port idfile args) #:transparent)
(struct SshChatBot (conf cmd-tasks sched-tasks) #:transparent)




(define (make-config #:host    [host "0.0.0.0"]
                     #:user    [user "user"]
                     #:port    [port "2222"]
                     #:keyfile [keyfile ""]
                     #:args    [args '()])
  (Config "host"
          "user"
          "port"
          "file"
          '()))


(define (make-bot conf)
  (SshChatBot )


; Create an SSH subprocess from a config struct
(define/contract (make-ssh-from-config conf)
  (-> Config? (values subprocess? input-port? output-port? (or/c #f input-port?)))
  (apply subprocess
         `(#f #f 'stdout
           ,(find-executable-path "ssh")
           ,(format "-p ~a" (Config-port conf))
           ,@(let ([idfile (Config-idfile conf)])
               (if (file-exists? idfile)
                   (list
                    (format "-i ~a" idfile)))
                  '()))
           ,(format "-o SetEnv TERM=bot")
           ,@(Config-args conf)
           ,(format "~a@~a"
                    (Config-user conf)
                    (Config-host conf))))
  


; Generate the i/o stub functions to interact with the subprocess
(define/contract (create-ssh-io conf)
  (-> Config? (values subprocess? procedure? procedure?))
  (define-values (S OUT IN ERR)
    (make-ssh-from-config conf))
  (define (read!)
    (read-line OUT 'return))
  (define (write! msg)
    (display (format "~a\r\n" msg) IN)
    (flush-output IN))
  (values S read! write!))
  



; \* (.*?) (joined|left)\..*  - #join/leave
; .*:.*                       - #plain msg
(define (run-bot conf)
  (define-values (sub read! write!)
    (create-ssh-io conf))
  (define (loop)
    (define msg (read!))
    (match (string-trim msg)
      ([regexp #rx"\\* (.*?) (left|joined)\\.*"] (displayln "Join/leave"))
      ([regexp #rx"(.*):(.*)"] (displayln "user message"))
      (else
       (displayln "uncategorized")))
    (displayln v)
    (sleep 1)
    (loop))
  (loop))



(module+ test
  (require rackunit)
  (check-equal? (+ 2 2) 4))

(module+ main
  (displayln "This is a library, not a main file"))

; end
