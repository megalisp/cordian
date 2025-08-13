#lang racket

(require "main.rkt")

;; Minimal bot for CI/test: only ping command, test mode disables network
(define cordian-test-mode #t) ; always test mode in this file
(define bot-token "DUMMY_TOKEN_FOR_TESTING")

(define bot-client
	(bot #:startup-message #( "Cordian test bot (CI mode) loaded.")
			 #:token bot-token
			 #:intents (list intent-guilds intent-guild-messages intent-message-content)
			 #:debug-bot-token #f
			 #:debug-event-logger #f
			 #:debug-responses #f))

(set-cmd-prefix! "?!")

(cmd ping ()
	#:desc "Replies with Pong!"
	#:exam "ping"
	#:logic (respond "Pong!"))

;; Do not start the client in test mode (no network calls)
(when (not cordian-test-mode)
	(start-command-dispatcher bot-client)
	(start-client bot-client))

;; For CI, just loading this file without error is a pass
(displayln "Cordian test bot loaded (no network calls made)")
