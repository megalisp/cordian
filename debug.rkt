#lang racket

(require racket/date)

;; Debug flag parameters
(define debug-event-logger-enabled (make-parameter #t))
(define debug-responses-enabled (make-parameter #t))

(provide debug-event-logger-enabled debug-responses-enabled debug-log)


;; debug-type: formats the debug type tag

(define (debug-type type)
  (format "[DEBUG: ~a]" type))

;; debug-log: prints a timestamped debug message with subsystem and message
(define (debug-log subsystem fmt . args)
  (let ([now (current-date)])
    (printf "<~a-~a-~a AT ~a:~a:~a>\n~a "
            (date-year now)
            (date-month now)
            (date-day now)
            (date-hour now)
            (date-minute now)
            (date-second now)
            (debug-type subsystem))
    (apply printf fmt args)
    (newline)
    (newline)
    (flush-output)))

(provide debug-type)
