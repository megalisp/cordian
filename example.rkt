#!/usr/bin/env racket
#lang racket

(require "main.rkt")
;;(require cordian)

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

(cmd ping ()
  #:desc "Replies with Pong!"
  #:exam "ping"
  #:logic (begin
           (reports "ping command by ~a (~a)" (hash-ref (author ctx) 'username) (hash-ref (author ctx) 'id))
           (respond "Pong!")))


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




(debug-log "BOT" "Starting bot client...")
(start-client bot-client)
(debug-log "BOT" "start-client returned (this should not happen unless the client stops).")
