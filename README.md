ssh-chat Bot Framework
===
A Racket library for developing [ssh-chat](https://github.com/shazow/ssh-chat) bots.

Goals:
* create a bot that is easy to bind with chat-invoked functions
* create a bot that is easy to schedule functions on intervals
* create a bot capable of responding to messages automatically

The API is not stable in it's current state and the design is subject to change.

## Writing a Bot

A "bot", short for robot, is a program that accepts some input and returns some kind of output. It's a mapping of commands and logic to input from users. A bot is like a computer program, but when writing a bot, it should be possible to write it without having to worry about the networking/byte reading side of things.

The ssh-chat Bot Framework is a Racket library designed at writing chat bots for `ssh-chat` in a way that is simple and painless. There are a myriad of options and values you can pass to an `ssh` connection, and the ssh-chat Bot Framework aims to reduce the amount of boilerplate and heavy backend lifting.

### Example Bots

The repository contains a few use-case examples of bots. Check the `/examples` folder at the root of this project.

### Basic Connection Bot

Our library `scb.rkt` provides a way of defining a bot in a clean and simple manner. Using the provided `define-bot` macro, we can define the settings we want our bot to adhere to when connecting to a remote server. In the simplest case, the only thing that's needed to start a bot is the following:

```racket
(require "scb.rkt")

(define-bot MyBot
  (host "192.168.1.1"))

(run-bot MyBot)
```

This bot will connect to the `192.168.1.1` address using the default port `22`. The port was not implied, and as such the library will assume the default value of `22`. Many settings are implied if they are left empty, but some need an explicit definition (like the host address).

If you were to run an `ssh-chat` server not on the default port, that can be modified by supplying the correct port, if you were for instance to use it on `2022` (`ssh-chat`'s default port binding).

```racket
(define-bot MyBot
  (host "192.168.1.1")
  (port "2022"))

(run-bot MyBot)
```

### Adding Logic

A bot, however is not super useful unless it has some degree of logic to execute on certain events. In order to better design a bot, we must support the different events that can occur within `ssh-chat`. Such as:

* user join/leave events
* message received events
* direct message/private message events
* emote action events
* system-level commands like `/names` or `/whois`

It should be possible to create logic that triggers when certain things happen. We can do this by processing each message from `ssh-chat` and executing based on the pattern of the text received.

When a bot is created, it is simply a program that interacts with an `ssh` connection. It does not compile or use any kind of `ssh` C libraries like OpenSSL, but it requires an `ssh` binary on the system to be discoverable. The bot program creates an `ssh` connection and collects its output, and when the bot is killed, the `ssh` connection is also killed.

The bot will manage the `ssh` subprocess on the system, and will create three reader ports to read `stdin`, `stdout`, and `stderr`, but as far as `stderr` is concerned, that is grouped together with `stdout` for the most part. A bot program has two input/output functions called the `reader` and `writer` functions, and when executing special bot logic, we need the `writer` command to send data to the `ssh` subprocess.

To add logic, we must include a function the bot can defer information to. To do this, let's try using the `on-msg` event.

```racket
(define-bot MyBot
  (host "192.168.1.1")
  (port "22")
  (on-msg
   (λ (usr msg writer ST)
     (writer "Hello!")
     ST)))

(run-bot MyBot)
```

This creats a bot with an event trigger called `on-msg`, which is called when a bot receives any kind of plain text generic message in the chatroom.

The four variables passed are important to note:

* the user who sent the message
* the message itself with the username stripped
* the reference to the `writer` function
* the reference to the bot's internal state storage

The function we write looks to return the `ST` variable. But why? That's because when a message is received, it loops with new state, that way the bot can add and remove information from the state as it sees fit based on it's customizable logic. By not passing the `ST` variable at the end of the function, the bot will "erase" it's running state because it received no continuing information.

Bottom line: if you want to interact with the state, remember to return it. If you don't want any kind of state, you don't have to worry then, the internal state will be simply `void`.

Other events included and their arguments:

* `on-join (usr writer state)` - for when a user joins the channel
* `on-leave (usr writer state)` - for when a user leaves the channel
* `on-pm (usr msg writer state)` - for when someone privately messages your bot

### Executing System Commands

There are many commands on `ssh-chat` that users can use, like `/whois` to check the identity of fellow users on the chat. Commands are also how you can directly message users in private.

If, for example, you were to design a function to work with the `on-pm` event, the `writer` function has no knowledge of that directly, and as such, if you were to use `writer` to write back, it would by itself not be a direct message to that user. Instead, we must encapsulate that by writing out the message `/msg <user> <msg>`. That way `ssh-chat` can interpret your action correctly.

Our library will also provide some quick string formatters to help you write these actions out. If you want to write a function that replies to a user with "hello" any time they privately message you, you can do so like this.

```racket
(require "scb.rkt")

(define-bot RepliesHello
  (host "192.168.1.1")
  (port "22")
  (on-pm
    (λ (usr msg writer ST)
      (writer (pm usr "Hello!"))
      ST)))

(run-bot RepliesHello)
```

We used the `pm` function to format a string that represents the proper `ssh-chat` action to message the user back.

The question is: why not modify the `writer` function to automatically accept text that then replies back to whatever user? We could compose the `writer` function with the `pm` function to make it that easy.

The reality is that there could exist scenarios where you don't always want the bot to reply through a private message. You could create a bot that users private message to execute some logic, and maybe the bot can announce things in public. It isn't useful to override the `writer` to automatically encapsulate text, and is better to separate the functionality.

```racket
(define-bot RevealsPMs
  (host "192.168.1.1")
  (port "22")
  (on-pm
    (λ (usr msg writer ST)
      (writer (format "~a messaged me '~a'" usr msg))
      ST))))

(run-bot RevealsPMs)
```

### Manipulating State

State, the internal running data structure the bot keeps, is a hash map that is passed to the next frame for every step of the bot program. State is manipulated by returning and passing the updated state to the next recursive call.

A hash map in Racket is a simple data structure to use, but has some hang-ups. As such, the `scb.rkt` library provides a small hash map library called `State.rkt` made up of some macros to make it easier to mainpulate state. There is more functionality to add to it still, but it's not difficult to add more features.

```racket
(define-bot LastBot
  (host "192.168.1.1")
  (port "22")
  (on-msg
    (λ (usr msg writer ST)
      (writer (format "~a is the latest person to message"))
	  (State:update ST 'last usr))))
```

A hash is a relational map between a series of keys and a series of values where `K -> V`. Macros are used to work with hashes in Racket since trying to index a key that does not exist will generate an error, unless you properly use `hash-has-key?` to check the existence of the key in the hash. Because of that, macros are used to help reduce boilerplate and modify/access the hash easier.

### Custom Command Actions

A custom action is a derivative of a general `on-msg` style bot. The difference is that the user shouldn't have to handle the message parsing aspect, and as such makes it easier to bind new functionality into a bot.

A custom action is a relation between a key and a function, where the key is the invoke string, and the function is pretty much just the functions we've been using with `on-msg` and such.


```racket
(define-bot HelloBot
  (host "192.168.1.1")
  (port "22")
  (command "!hello"
    (λ (usr args writer ST)
      (writer (format "Hello ~a!" usr))
      ST)))

(run HelloBot)
```

The difference is that we do not parse a whole message, but instead take a list of arguments. The list is a list of words split up by spaces, so you can iterate over it, grab the head of the list, reconstruct the message, or do whatever you so please. For example, if the command was `"!hello world"`, `args` would be set to `'("world")`.

So instead of creating a long-winded `on-msg` or `on-pm` function with a lot of string checking, you can simply use the `command` to insert new actions into the bot to execute the logic for. The command string can be anything, but special characters are usually preferred for invoking programs.

### Scheduled Tasks

(WIP) A scheduled task is a function to be executed on a regular interval. This is a bit different from the main bot loop, because scheduled tasks require a timer to know when to fire functions off.

This has problems because a bot, from what we have seen, reactively applies logic based on message input from `ssh-chat`, whereas a scheduled task program requires a schedule to fire off the logic. How does this work?

By design, the main `loop` cycle of the bot executes each time a message is received and recursively listens for new messages. In turn, this does not work for a timer-based program, because no user input might ever be received, meaning this is fairly unreliable to depend on.

An idea is to use an internal threading system using Racket's `thread` to take work off the main thread. If we simply fired up a thread, told it to wait for `N` seconds based on the scheduled task, then it can execute it's logic and the thread will have some information shared with it from the bot program.

An issue arises here, because the state will not properly be shared, nor does the main bot loop logic know how to communicate with another thread. It simply checks for message input, then defers it's logic to the appropriate branch.

A workaround is to spawn a secondary thread that acts as a communication between other threads. The goal of this thread is to simply act and wait for information. Each time the internal bot loop activates, it can send a message to this secondary observer thread, watching for the change in state (using `thread-wait` to observe new state).

A scheduled task can be generated away from the main bot thread and this observer thread, and can then make requests from the observer.

```
(thread
  (define (loop ST)
  0))
```

### Magic Behind `define-bot`

The reasoning for the `define-bot` macro is to make it as dead-simple as possible to make a custom `ssh-chat` bot. Due to the inherent nature of Racket, mutating data is generally a bad idea, and where possible, we want to try to avoid mutating data, and would instead copy data and add or subtract additional data.

That means, in order to define a regular bot, you would have to somehow pack a whole lot of information into a single (or more) `struct` data types, making it rather difficult to pass arguments into without frustrating users.

In this case, this might work:

```racket
(struct SshBot (user host port args commands) #:transparent)

(define MyBot "MyBot" "127.0.0.1" "22" '()
  (make-immutable-hash
    '(("!hello" . (lambda (usr args writer ST)
                    (writer (format "Hello ~a!" usr))
                    ST)))))
```

This works, but is very hard to extend. The more information you start putting in, the clumsier it gets and it will fall apart faster. It would be better if we could somehow start with an empty struct and modify the information as we go, making it easier for the user to decide what elements they want and don't want.

```racket
(struct SshBot (user host port args commands) #:transparent)

(define MyBot "MyBot" "" "" '() #hash())

(set! MyBot host "127.0.0.1")
```

This syntax does not exist, because `set!` can only work on variables, and there's no way to mutate the data of the struct with `set!` for the most part. You would have to use `struct-copy` to copy the contents of the original struct into a new binding.

This is where a `foldl` strategy comes into play. If we were to convert the named fields into functions that returned another function that runs the `struct-copy`, we could fold over an empty struct and add new data as we go along, and it would certainly look a lot nicer.

```racket
(define (init-bot name)
  (SshBot name "" "" '() #hash()))

(define (host new-host)
  (lambda (old-state)
    (struct-copy SshBot old-state [host new-host])))

(define (run-state f s)
  (f s))

(define MyBot
  (foldl run-state (init-bot "MyBot")
    (list
      (host "127.0.0.1"))))
```

Coincidentally, it's even easier to do this with a macro. After all, we're declaring our name twice here in the bot, once with the `define`, the other internally with the `struct`'s initiation. A macro could take care of this for us, and it'll look a lot cleaner.

```racket
(define-syntax-rule (define-bot name fun ...)
  (define name
    (foldl run-state (init-bot (format "~a" name)
          (list funs ...))))
```

Which then gives us:

```racket
(define-bot MyBot
  (host "192.168.1.1")
  (port "22")
  (on-msg 
    (λ (usr msg writer ST)
      (writer "Hello!")
      ST)))
```

The collection of functions like `host`, `port` and `on-msg` are function factories for modifying the original state of the struct we defined. This ends up being cleaner, with less redundancies, and easier to write overall.

### Multiple Bots, One Program

It is possible to run multiple bots within the scope of one program. This would make it easier to set up multiple bots whose scope is similar.

The goal here would be to set them up with their own thread, isolating their interactions far from each other. Using some basic Racket code, it's possible to create a list of threads for each bot definition, then you make the main thread wait for all the bots.

```
(define-bot Bot1
  (host "0.0.0.0"))

(define-bot Bot2
  (host "0.0.0.0"))
  
(define threads
 (map
  (lambda (B)
   (thread
    (lambda ()
     (run-bot B))))
 (list Bot1 Bot2)))

(for ([T threads])
  (thread-wait T))
```

The bots will all run at the same time and the main thread will block until all threads are done.

### Building, Distributing and Sharing

(WIP) Converting Racket programs into executables is easy enough with the `raco exe` command. Going a step further, to cross-compile programs across systems, it might be better to use the command `raco cross` based off the `raco-cross` package.

Converting a bot is easy enough because it's trivial to convert any of the above bot code into a binary. However, distributing a bot is similar to that of distributing any other program - making it customizable will go much further.

Permanently sealing details like the `host` or the `port` means that distributing the bot program isn't effective - the connection details will be effectively sealed forever. For a user to download your fully-compiled bot program and run it, the configuration when compiled is not going to be useful for everyone.

To make the bot flexible, the bot library comes with a macro to convert a given bot into a basic program that users can change the options at runtime via supplied program arguments. This exists as a macro because we want to design the program to be easy to use, and the macro will provide the abstraction for the command line processing by supplying code from `racket/cmdline`.

```racket
(define-bot CLIBot
  (host "0.0.0.0")
  (port "22")
  (on-msg
    (λ (usr msg writer ST)
	  (writer "Hello!")
	  ST)))

(run-bot CLIBot)
```

Compiling this will forever target `0.0.0.0`, which isn't ideal and makes it hard to distribute and share your compiled programs if you were to ever become an `ssh-chat` bot merchant. If we were to leave these details out, however, then it would look a little different.

```racket
(define-bot CLIBot
  (on-msg
    (λ (usr msg writer ST)
	  (writer "Hello!")
	  ST)))

(run-bot CLIBot)
```

The bot now includes no concrete details, and instead when the bot is created relies on parameters. Parameters are a special construct in Racket that are mutable and easy to preserve information across threads by simply preserving past states before mutation.

By default, the Racket standard library provides `racket/cmdline` as a means of reading command line arguments and providing many interactions to configure a program. Making an interactive program where users can change the configuration of the bot requires a small `command-line` setup.

```racket
(command-line
 #:program "MyBot"
 #:once-each
```

TODO: needs more work
