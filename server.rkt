#!/usr/bin/racket
#lang racket

;  Copyright 2012 Chris Branch
;
;  Licensed under the Apache License, Version 2.0 (the "License");
;  you may not use this file except in compliance with the License.
;  You may obtain a copy of the License at
;
;      http://www.apache.org/licenses/LICENSE-2.0
;
;  Unless required by applicable law or agreed to in writing, software
;  distributed under the License is distributed on an "AS IS" BASIS,
;  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;  See the License for the specific language governing permissions and
;  limitations under the License.

(require net/url)

(define tcp-port 7265)
(define key-file "/etc/ddns-nsupdate-bridge/Kermine.lutrasoft.com.+157+16851.private")
(define dns-server "ns1.lutrasoft.com")
(define dns-zone "lutrasoft.com")

; Initial handler for TCP connections
(define (tcp-handler input output)
  (let [(header-match (regexp-match #px"^(\\w+) ([^\r\n]+)\r?\n" input))]
    (if header-match
       (let [(method (bytes->string/locale (list-ref header-match 1)))]
         (if (string=? (string-upcase method) "PUT")
            (handle-put input output (bytes->string/locale (list-ref header-match 2)))
            (error 'request-method "unknown method ~a" method)))
       (error 'request-method "invalid request header format"))))

; Handles PUT connections
(define (handle-put input output uri-string)
  (let*-values [((url) (string->url uri-string))
               ((our-ip their-ip) (tcp-addresses output))
               ((query) (url-query url))]
    (if (equal? (length (url-path url)) 1)
       (begin
         (run-nsupdate output
        #:server dns-server
        #:zone dns-zone
        #:keyn (dict-ref query 'key)
        #:keys (dict-ref query 'secret)
        #:record (path/param-path (first (url-path url)))
        #:ip their-ip)
         (display "200 OK\r\n" output)
         (flush-output output))
       (error 'handle-put "Path should have only one component"))))

; Executes a record update using nsupdate
(define (run-nsupdate out
         #:server server
         #:zone zone
         #:keyn keyn
         #:keys keys
         #:record record
         #:ip ip)
  (let-values ([(subprocess-ref stdout stdin stderr) (subprocess #f #f #f "/usr/bin/nsupdate" "-k" key-file "-v")])
    (fprintf stdin "server ~a\n" server)
    (fprintf stdin "zone ~a\n" zone)
    (fprintf stdin "key ~a ~a\n" keyn keys)
    (fprintf stdin "update delete ~a A\n" record)
    (fprintf stdin "update add ~a 3600 A ~a\n" record ip)
    (flush-output stdin)
    (close-output-port stdin)
    (sync/timeout 2 subprocess-ref)
    (let [(retval (port->string stdout))
          (reterr (port->string stderr))]
      (map close-input-port (list stdout stderr))
      (if (string=? reterr "") retval (error 'nsupdate "Error from nsupdate: ~a" reterr)))))

; Start a listener loop
(define (listen-loop listener)
  (let-values ([(input output) (tcp-accept listener)])
    ((lambda ()
       (with-handlers [(exn:fail? (lambda (e)
                  (fprintf output "500 Internal Server Error\r\n\r\n~a\r\n" (exn-message e))
                  (flush-output output)))]
          (tcp-handler input output))
       (close-input-port input)
       (close-output-port output)))
    (listen-loop listener)))
(listen-loop (tcp-listen tcp-port))
