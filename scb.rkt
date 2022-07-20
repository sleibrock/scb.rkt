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
         "State.rkt"
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
         on-msg
         on-pm
         on-join
         on-leave
         add-scheduled-task
         pm
         )


; the SshBot struct interface
; user   - username used during the ssh session
; host   - the target domain for the ssh connection
; port   - the listen port on the target domain server (default: 22)
; idfile - path to an id file to use, nullable
; args   - a list of string arguments to provide to the ssh session
; cmds   - a hash of commands to bind to the bot
; debug? - debugging mode to print warnings/errors/verbose output
(struct SshBot (user host port idfile args msg-evt pm-evt cmds timers join-evt leave-evt debug?)
  #:transparent)


; These functions are used to initialize an ssh-chat bot
; These work by returning functions that manipulate the SshBot state
; using a struct-copy.
; In turn this is a little easier than having to remember long struct
; constructors with many args and provides an easier API for writing
(define (init-bot name)
  (SshBot (format "~a" name)
          ""                            ; host
          "22"                          ; port
          ""                            ; id file
          '()                           ; ssh arguments
          (λ (usr msg writer ST) ST)    ; message event
          (λ (usr msg writer ST) ST)    ; pm evt
          (make-immutable-hash '())     ; commands
          '()                           ; timer tasks
          (λ (usr writer ST) ST)        ; join event
          (λ (usr writer ST) ST)        ; leave event
          #f                            ; debug option
          ))


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


(define (on-msg fun)
  (λ (old-state)
    (struct-copy SshBot old-state [msg-evt fun])))

(define (on-pm fun)
  (λ (old-state)
    (struct-copy SshBot old-state [pm-evt fun])))


; Add a command for any users to invoke and execute a logic function
; TODO: check arguments to match whatever we need for invoking commands
; TODO: add a command hash to the bot to store commands
(define (command keystr fun)
  (λ (old-state)
    (let ([old-cmds (SshBot-cmds old-state)])
      (struct-copy SshBot old-state
                   [cmds (State:update old-cmds keystr fun)]))))


; Define an on-join function for when users enter the chat
(define (on-join fun)
  (λ (old-state)
    (struct-copy SshBot old-state [join-evt fun])))


; Define an on-leave function for when users leave the chat
(define (on-leave fun)
  (λ (old-state)
    (struct-copy SshBot old-state [leave-evt fun])))


(define (add-scheduled-task fun)
  (λ (old-state)
    (let ([old-timers (SshBot-timers old-state)])
      (struct-copy SshBot old-state
                   [timers (cons fun old-timers)]))))


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
;   (command "!hello" (λ (msg writer ST) ST))
(define-syntax-rule (define-bot name fun ...)
  (define name
    (fold-functions (format "~a" 'name)
                    (list fun ...))))


; Boilerplate macro - run code only while a subprocess is alive
; between function calls and looping, the ssh connection could
; randomly die, so it's important to track errors accordingly
; and branch to handle the if/else case
(define-syntax-rule (while-alive S code ...)
  (if (eqv? 'running (subprocess-status S))
      (begin code ...)
      (error "ssh process closed early")))


; Chat utility functions - helpers for generating proper ssh-chat commands
; Useful for designing bot commands and provides a simple interface for
; designing interactions and all sorts of programs
(define (names)
  "/names")

(define (ban target)
  (format "/ban target 24h"))


; Execute a kick command
(define (kick target)
  (format "/kick ~a" target))


; execute a whois command, but provides complex output
(define (whois target)
  (format "/whois ~a" target))


; Encapsulate this into a composable function with the writers
(define (pm target msg)
  (format "/msg ~a ~a" target msg))


; Create a macro to express the ability to run code every so often
(define-syntax-rule (every stx)
  (void))


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
                 (displayln (format "~a\r\n" msg) IN)
                 (flush-output IN)))
  (values S read! write!))
  

; Main bot looping code to execute all logic
(define (run-bot bot)
  ; start the ssh connection here
  (define-values (sub read! write!)
    (start-ssh bot))
  
  (define (loop state)
    ; read an input message
    (define msg (read!))
    
    ; check if we received EOF or not
    (when (eof-object? msg)
      (error (format "EOF received, terminating ~a" (SshBot-user bot))))
    
    ; trim it
    (define trimmed-msg (string-trim msg))
    
    ; begin parsing - if no message/text, loop back
    ; else, begin interpreting the message
    (if (string=? "" trimmed-msg)
        (loop state)
        (match trimmed-msg 
          ; process a join/leave event
          ; join - includes number of active users
          ; leave - shows time user spent connected
          ([regexp #rx"\\* (.*) (left|joined)\\.(.*)" (list _ user j/l _)]
           (cond
             ([string=? j/l "joined"]
              (begin
                (printf "[UJC] ~a joined the chat\n" user)
                (loop ((SshBot-join-evt bot) user write! state))))
             (else
              (begin
                (printf "[ULC] ~a left the chat\n" user)
                (loop ((SshBot-leave-evt bot) user write! state))))))
          
          ; handle a general "emote" action
          ; TODO: implement an emote handler
          ([regexp #rx"\\*\\* (.*?) (.*)" (list _ user emote-msg)]
           (begin
             (printf "[EMO] ~a did: ~a\n" user emote-msg)
             (loop state)))
          
          ; handle a direct message/private message
          ([regexp #rx"\\[PM from (.*)\\] (.*)" (list _ user priv-msg)]
           (begin
             (printf "[PM] ~a wrote: ~a\n" user priv-msg)
             (loop ((SshBot-pm-evt bot) user
                                        (string-trim priv-msg)
                                        write!
                                        state))))
          
          ; process a user message
          ([regexp #rx"(.*?):(.*)" (list _ user new-msg)]
           (begin
             (printf "[MSG] ~a wrote:~a\n" user new-msg)
             (loop ((SshBot-msg-evt bot) user
                                         (string-trim new-msg)
                                         write!
                                         state))))
          
          ; blank or malformed msg caught
          (else
           (begin
             (displayln "uncategorized")
             (loop state))))))
  (loop (make-immutable-hash '())))


; Testing section for unit tests / etc
(module+ test
  (require rackunit)
  (check-equal? (+ 2 2) 4))

; Tell the user this isn't a bot
(module+ main
  (displayln "This is a library, not a main file"))

; end
