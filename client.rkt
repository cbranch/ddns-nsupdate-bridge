#!/usr/bin/racket
#lang racket/base
(require racket/port)
(require racket/tcp)
(require net/url)

(define tcp-port 7265)
(define dns-server "ns1.lutrasoft.com.")
(define key-name "tinytwin.lutrasoft.com.")
(define key-secret-file "secret.key")
(define record-name "tinytwin.lutrasoft.com.")

(define (write-update-request-line port)
  (let ([key-secret (call-with-input-file key-secret-file port->string)])
    (fprintf port "PUT ~a\r\n"
             (url->string
              (make-url #f #f #f #f #t
                        (list (make-path/param record-name '()))
                        `((key . ,key-name) (secret . ,key-secret))
                        #f)))))

(define (update-ddns)
  (with-handlers ([exn:fail:network? (lambda (e) (format "Unable to connect: ~a" (exn-message e)))]
                  [exn:fail? (lambda (e) (format "An error occurred: ~a" (exn-message e)))])
    (let-values ([(input output) (tcp-connect dns-server tcp-port)])
      (write-update-request-line output)
      (flush-output output)
      (let ([result (port->string input)])
        (close-output-port output)
        (close-input-port input)
        result))))

(display (update-ddns))