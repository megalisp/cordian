#!/usr/bin/env racket
#lang racket

(require "main.rkt")

(define bot-client
  (bot #:startup-message #( "Hello! Welcome to the example bot for Cordian"
                            "This bot requires you to do a few things to try it out."
                            "1. You need to make sure you set the BOT_TOKEN environment"
                            "2. You need to well... have Racket in your path / installed lol"
                            "3. You need to make sure example.rkt is executable then ./example.rkt")
       #:token (getenv "BOT_TOKEN") ;; You should never store this in you code / repo. 
       #:intents (list intent-guilds
                       intent-guild-messages
                       intent-message-content
                       intent-guild-message-reactions
                       intent-guild-members)
       #:debug-bot-token #f ;; hide by default. 
       #:debug-event-logger #t
       #:debug-responses #t))

(start-command-dispatcher bot-client)


(set-cmd-prefix! "🪗")


(cmd server ()
  #:desc "Shows server name and ID."
  #:exam "server"
  #:logic (begin
           (let ([guild (guild-info ctx)])
             (reports "server command by ~a (~a)" (hash-ref (author ctx) 'username) (hash-ref (author ctx) 'id))
             (respond (format "Server name: ~a\nServer ID: ~a"
                              (hash-ref guild 'name)
                              (hash-ref guild 'id))))))


(cmd ping ()
  #:desc "Replies with Pong!"
  #:exam "ping"
  #:logic (begin
           (reports "ping command by ~a (~a)" (hash-ref (author ctx) 'username) (hash-ref (author ctx) 'id))
           (respond "Pong!")))


(cmd help ()
  #:desc "Lists all commands."
  #:exam "help"
  #:logic (begin
           (reports "help command by ~a (~a)" (hash-ref (author ctx) 'username) (hash-ref (author ctx) 'id))
           (respond
            (string-join
             (for/list ([cmd (in-list (commands ctx))])
               (format "~a~a" cmd-prefix cmd))
             "\n"))))


(cmd avatar ()
  #:desc "Shows user's avatar URL."
  #:exam "avatar"
  #:logic (begin
           (let* ([user (author ctx)]
                  [id (hash-ref user 'id)]
                  [avatar (hash-ref user 'avatar #f)])
             (reports "avatar command by ~a (~a)" (hash-ref user 'username) id)
             (respond (if avatar
                          (format "https://cdn.discordapp.com/avatars/~a/~a.png" id avatar)
                          "User has no custom avatar.")))))

(cmd echo ()
  #:desc "Repeats your message."
  #:exam "echo hello world"
  #:logic (begin
           (reports "echo command by ~a (~a)" (hash-ref (author ctx) 'username) (hash-ref (author ctx) 'id))
           (respond (string-join args " "))))



(cmd say ()
  #:desc "Repeats your message and deletes the original."
  #:exam "say hello world"
  #:logic (begin
           (reports "say command by ~a (~a)" (hash-ref (author ctx) 'username) (hash-ref (author ctx) 'id))
           (respond (string-join args " "))
           (when (and (hash-ref ctx 'channel_id #f) (hash-ref ctx 'id #f))
             (delete-message ctx))))

(cmd 8ball ()
  #:desc "Ask the magic 8-ball a question."
  #:exam "8ball Will I win?"
  #:logic (begin
           (let* ([answers (vector
                            "It is certain."
                            "It is decidedly so."
                            "Without a doubt."
                            "Yes – definitely."
                            "You may rely on it."
                            "As I see it, yes."
                            "Most likely."
                            "Outlook good."
                            "Yes."
                            "Signs point to yes."
                            "Reply hazy, try again."
                            "Ask again later."
                            "Better not tell you now."
                            "Cannot predict now."
                            "Concentrate and ask again."
                            "Don't count on it."
                            "My reply is no."
                            "My sources say no."
                            "Outlook not so good."
                            "Very doubtful.")]
                  [choice (vector-ref answers (random (vector-length answers)))])
             (reports "8ball command by ~a (~a)" (hash-ref (author ctx) 'username) (hash-ref (author ctx) 'id))
             (respond choice))))

(cmd flip ()
  #:desc "Flip a coin N times. Usage: flip [times] (default 1, max 1000)."
  #:exam "flip 10"
  #:logic (begin
           (let* ([times-raw (if (and (pair? args) (string->number (car args))) (string->number (car args)) 1)])
             (if (or (not (integer? times-raw)) (< times-raw 1) (> times-raw 1000))
                 (respond "You can only flip between 1 and 1000 times (default is 1).")
                 (let* ([times times-raw]
                        [flips (for/list ([i (in-range times)]) (random 2))]
                        [heads (count zero? flips)]
                        [tails (- times heads)]
                        [results (list->string (map (lambda (x) (if (zero? x) #\H #\T)) flips))]
                        [summary (format "\nHeads Total: ~a\nTails Total: ~a" heads tails)])
                   (respond (string-append results summary)))))))


(cmd shoot ()
  #:desc "Play rock-paper-scissors with the bot. Usage: shoot [rock|paper|scissors]"
  #:exam "shoot rock"
    #:logic (begin
       (let* ([choices '("rock" "paper" "scissors")]
           [user (if (null? args) "" (string-downcase (car args)))]
           [bot (list-ref choices (random 3))])
         (cond
        [(not (member user choices)) (respond "Choose rock, paper, or scissors!")]
        [else
         (let* ([result (cond
                [(equal? user bot) "It's a tie!"]
                [(or (and (equal? user "rock") (equal? bot "scissors"))
                  (and (equal? user "paper") (equal? bot "rock"))
                  (and (equal? user "scissors") (equal? bot "paper"))) "You win!"]
                [else "You lose!"])])
           (respond (format "You chose: ~a\nBot chose: ~a\n~a" user bot result)))])
       )))

(cmd choose ()
  #:desc "Randomly picks one of the provided options."
  #:exam "choose pizza burger sushi"
    #:logic (begin
             (if (null? args)
                 (respond "Please provide options to choose from.")
                 (respond (list-ref args (random (length args)))))))

(cmd mock ()
  #:desc "Returns the input text in alternating upper/lowercase."
  #:exam "mock this is silly"
    #:logic (begin
             (let* ([text (string-join args " ")]
                    [mocked (list->string
                             (for/list ([c (in-string text)] [i (in-naturals)])
                               (if (even? i) (char-downcase c) (char-upcase c))))])
               (respond mocked))))

(cmd uwu ()
  #:desc "Converts the input text to uwu speak."
  #:exam "uwu hello friend"
    #:logic (begin
             (let* ([text (string-join args " ")]
                    [uwu (regexp-replace* #rx"[lr]" (regexp-replace* #rx"[LR]" text "W") "w")]
                    [uwu (regexp-replace* #rx"n([aeiou])" uwu "ny\\1")])
               (respond (string-append uwu " uwu")))))

(cmd shout ()
  #:desc "Returns the input text in all caps with spaces between each letter."
  #:exam "shout hello world"
    #:logic (begin
             (let ([text (string-join args " ")])
               (respond (string-join (map string (string->list (string-upcase text))) " ")))))

(cmd spoiler ()
  #:desc "Wraps the input in Discord spoiler tags and deletes the original message."
  #:exam "spoiler Darth Vader is Luke's father"
    #:logic (begin
             (let ([text (string-join args " ")])
               (respond (string-append "||" text "||"))
               (when (and (hash-ref ctx 'channel_id #f) (hash-ref ctx 'id #f))
                 (delete-message ctx)))))
(cmd clap ()
  #:desc "Adds 👏 between every word."
  #:exam "clap this is great"
    #:logic (begin
             (respond (string-join args " 👏 "))))


(cmd ship ()
  #:desc "Generates a ship name and compatibility percentage."
  #:exam "ship Alice Bob"
    #:logic (begin
             (if (< (length args) 2)
                 (respond "Please provide two names to ship.")
                 (let* ([name1 (car args)]
                        [name2 (cadr args)]
                        [shipname (string-append (substring name1 0 (max 1 (quotient (string-length name1) 2)))
                                                 (substring name2 (max 0 (- (string-length name2) (quotient (string-length name2) 2)))))]
                        [compat (+ 50 (random 51))])
                   (respond (format "~a ❤️ ~a\nShip name: ~a\nCompatibility: ~a%" name1 name2 shipname compat))))))


(cmd sarcasm ()
  #:desc "Randomly capitalizes letters for a sarcastic effect."
  #:exam "sarcasm this is so cool"
    #:logic (begin
             (let* ([text (string-join args " ")]
                    [s (list->string (for/list ([c (in-string text)])
                                      (if (zero? (random 2)) (char-upcase c) (char-downcase c))))])
               (respond s))))

(cmd yoda ()
  #:desc "Rearranges the input to sound like Yoda."
  #:exam "yoda you must learn the ways of the force"
    #:logic (begin
             (let* ([words (string-split (string-join args " "))]
                    [len (length words)]
                    [split (if (> len 3) (quotient len 2) 1)]
                    [yoda (string-append (string-join (drop words split) " ") ", " (string-join (take words split) " "))])
               (respond (string-append yoda ". Hmmm.")))))

(define (zalgo-char c)
  (define zalgo-above '(#\u030d #\u030e #\u0304 #\u0305 #\u033f #\u0311 #\u0306 #\u0310 #\u0352 #\u0357 #\u0351 #\u0307 #\u0308 #\u030a #\u0342 #\u0343 #\u0344 #\u034a #\u034b #\u034c #\u0303 #\u0302 #\u030c #\u0350 #\u0300 #\u0301 #\u030b #\u030f #\u0312 #\u0313 #\u0314 #\u033d #\u0309 #\u0363 #\u0364 #\u0365 #\u0366 #\u0367 #\u0368 #\u0369 #\u036a #\u036b #\u036c #\u036d #\u036e #\u036f #\u033e #\u035b #\u0346 #\u031a))
  (define zalgo-below '(#\u0316 #\u0317 #\u0318 #\u0319 #\u031c #\u031d #\u031e #\u031f #\u0320 #\u0324 #\u0325 #\u0326 #\u0329 #\u032a #\u032b #\u032c #\u032d #\u032e #\u032f #\u0330 #\u0331 #\u0332 #\u0333 #\u0339 #\u033a #\u033b #\u033c #\u0345 #\u0347 #\u0348 #\u0349 #\u034d #\u034e #\u0353 #\u0354 #\u0355 #\u0356 #\u0359 #\u035a))
  (define zalgo-mid '(#\u0315 #\u031b #\u0340 #\u0341 #\u0358 #\u0321 #\u0322 #\u0327 #\u0328 #\u0334 #\u0335 #\u0336 #\u034f #\u035c #\u035d #\u035e #\u035f #\u0360 #\u0362 #\u0338 #\u0337 #\u0361 #\u0489))
  (define (pick lst n) (apply string (for/list ([i (in-range n)]) (list-ref lst (random (length lst))))))
  (string-append (string c)
                 (pick zalgo-above (add1 (random 2)))
                 (pick zalgo-mid (random 2))
                 (pick zalgo-below (random 2))))

(cmd zalgo ()
  #:desc "Adds Zalgo (glitchy/creepy) diacritics to text."
  #:exam "zalgo this is cursed"
    #:logic (begin
             (let* ([text (string-join args " ")]
                    [z (list->string (apply append (map (lambda (c) (if (char-alphabetic? c) (string->list (zalgo-char c)) (list c))) (string->list text))))])
               (respond z))))



;; Heartbeat function: posts a message to a channel every 60 seconds
(define (start-heartbeat client channel-id)
  (thread
   (lambda ()
     (let loop ([i 0])
       (with-handlers ([exn:fail? (lambda (e) (printf "[HEARTBEAT ERROR] ~a\n" e))])
         (respond (vector-ref still-alive-lyrics i) #:client client #:channel channel-id)
         (sleep 60))
       (loop (modulo (add1 i) (vector-length still-alive-lyrics)))))))




(start-heartbeat bot-client "1403137124697509928") ;; #heart channel ID


(debug-log "BOT" "Starting bot client...")
(start-client bot-client)
(debug-log "BOT" "start-client returned (this should not happen unless the client stops).")
