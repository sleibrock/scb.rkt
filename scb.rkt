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
         (only-in racket/string string-trim)
         )

(provide (struct-out SshBot)
         host
         port
         args
         id-file
         command
         define-bot
         )


(struct SshBot (user host port idfile args cmds) #:transparent)


(define (init-bot name)
  (SshBot (format "~a" name)
          "" "" "" '() '()))

(define (host new-host)
  (λ (old-state)
    (struct-copy SshBot old-state [host new-host])))

(define (port new-port)
  (λ (old-state)
    (struct-copy SshBot old-state [port new-port])))

(define (args new-args)
  (λ (old-state)
    (struct-copy SshBot old-state [args new-args])))

(define (id-file new-idfile)
  (λ (old-state)
    (struct-copy SshBot old-state [idfile new-idfile])))

(define (command keystr fun)
  (λ (old-state)
    old-state))


; Run a function on some state
(define (run-state fun scc)
  (fun scc))

; Fold a list of functions over some starting initial state
(define (fold-functions name functions)
  (foldl run-state (init-bot name) functions))


(define-syntax-rule (define-bot name fun ...)
  (define name
    (fold-functions (format "~a" 'name)
                    (list fun ...))))



; Create an SSH subprocess from a config struct
(define/contract (make-ssh-from-bot bot)
  (-> SshBot? (values subprocess? input-port? output-port? (or/c #f input-port?)))
  (apply subprocess
         `(#f #f 'stdout
           ,(find-executable-path "ssh")
           ,(format "-p ~a" (SshBot-port bot))
           ,@(let ([idfile (SshBot-idfile bot)])
               (if (file-exists? idfile)
                   (list
                    (format "-i ~a" idfile))
                  '()))
           ,(format "-o SetEnv TERM=bot")
           ,@(SshBot-args bot)
           ,(format "~a@~a"
                    (SshBot-user bot)
                    (SshBot-host bot)))))
  


; Generate the i/o stub functions to interact with the subprocess
(define/contract (create-ssh-io bot)
  (-> SshBot? (values subprocess? procedure? procedure?))
  (define-values (S OUT IN ERR)
    (make-ssh-from-bot bot))
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
    (displayln msg)
    (sleep 1)
    (loop))
  (loop))



(module+ test
  (require rackunit)
  (check-equal? (+ 2 2) 4))

(module+ main
  (displayln "This is a library, not a main file"))

; end
