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
         debug-mode
         define-bot
         run-bot
         )


(struct SshBot (user host port idfile args cmds debug?) #:transparent)


; These functions are used to initialize an ssh-chat bot
; These work by returning functions that manipulate the SshBot state
; using a struct-copy.
; In turn this is a little easier than having to remember long struct
; constructors with many args and provides an easier API for writing
(define (init-bot name)
  (SshBot (format "~a" name)
          "" "" "" '() '() #f))

; Change the host to a given host string (ex. "0.0.0.0" or "ssh.chat")
(define (host new-host)
  (λ (old-state)
    (struct-copy SshBot old-state [host new-host])))

; Change the port to a new given port (ex. "2022" or 22)
(define (port new-port)
  (λ (old-state)
    (struct-copy SshBot old-state [port new-port])))

; Change the arguments to supply to the ssh program
; (ex. ("-o SetTheme=mono" "-p 22"))
(define (args new-args)
  (λ (old-state)
    (struct-copy SshBot old-state [args new-args])))

; Change the id file to a provided one (ex. "~/.ssh/id_rsa")
(define (id-file new-idfile)
  (λ (old-state)
    (struct-copy SshBot old-state [idfile new-idfile])))

; Add a command for any users to invoke and execute a logic function
; TODO: check arguments to match whatever we need for invoking commands
; TODO: add a command hash to the bot to store commands
(define (command keystr fun)
  (λ (old-state)
    old-state))

; Turn on debug mode for developers
(define (debug-mode)
  (λ (old-state)
    (struct-copy SshBot old-state [debug? #t])))


; Once we have a series of functions, we then fold over all the functions
; by providing an initial "blank" bot struct, and each function then
; manipulates the state. This is done by using a language-level macro
; which will re-write the code into a foldl call (see define-bot macro below)
; and fold the state over the state-mutating functions used


; Run a function on some state (used by the foldl function)
(define (run-state fun scc)
  (fun scc))

; Fold a list of functions over some starting initial state
(define (fold-functions name functions)
  (foldl run-state (init-bot name) functions))


; Create a macro which can layer multiple rules to create an SSH bot
;
; ex:
; (define-bot SampleBot
;   (host "127.0.0.1")
;   (port "2022")
;   (command "!hello" (λ (msg) (void))))
(define-syntax-rule (define-bot name fun ...)
  (define name
    (fold-functions (format "~a" 'name)
                    (list fun ...))))


; Boilerplate macro - run code only while a subprocess is alive
(define-syntax-rule (while-alive S code ...)
  (if (eqv? 'running (subprocess-status S))
      (begin code ...)
      (error "ssh process closed early")))


; Create an SSH subprocess from a bot struct and create
; the general i/o function stubs to interact
(define/contract (start-ssh bot)
  (-> SshBot? (values subprocess? procedure? procedure?))
  (define-values (S OUT IN ERR)
    (let ([ssh (find-executable-path "ssh")])
      (if (eqv? #f ssh)
          (error "Cannot create an ssh bot - no ssh detected on the system")
          (apply subprocess
                 `(#f #f stdout
                   ,(find-executable-path "ssh")
                   ,(format "-p ~a" (SshBot-port bot))
                   ,@(let ([idfile (SshBot-idfile bot)])
                       (if (and (not (string=? "" idfile)) (file-exists? idfile))
                           (list
                            (format "-i ~a" idfile))
                           '()))
                   ,(format "-o SetEnv TERM=bot")
                   ,@(SshBot-args bot)
                   ,(format "~a@~a"
                            (SshBot-user bot)
                            (SshBot-host bot)))))))
  (define (read!)
    (while-alive S (read-line OUT 'return)))
  (define (write! msg)
    (while-alive S
                 (displayln (format "~a\r\n" msg))
                 (flush-output IN)))
  (values S read! write!))
  

; Main bot looping code to execute all logic
(define (run-bot bot)
  (define-values (sub read! write!)
    (start-ssh bot))

  (define (loop state)
    (define msg (read!))
    (when (eof-object? msg)
      (error "EOF received, terminating bot"))
    (match (string-trim msg)
      ([regexp #rx"\\* (.*?) (left|joined)\\.*"] (displayln "join/leave"))
      ([regexp #rx"(.*):(.*)"] (displayln "user msg"))
      (else
       (displayln "uncategorized")))
    (displayln msg)
    (loop state))
  (loop 0))



; Testing section for unit tests / etc
(module+ test
  (require rackunit)
  (check-equal? (+ 2 2) 4))

; Tell the user this isn't a bot
(module+ main
  (displayln "This is a library, not a main file"))

; end
