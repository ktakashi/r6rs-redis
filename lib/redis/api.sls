;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; redis/api.sls - High level API
;;;
;;;   Copyright (c) 2018  Takashi Kato  <ktakashi@ymail.com>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

#!r6rs
(library (redis api)
    (export redis-send-command
	    redis-error?
	    redis-error-command
	    redis-error-connection)
    (import (rnrs)
	    (redis resp))

(define-condition-type &redis &error
  make-redis-error redis-error?
  (command redis-error-command)
  (connection redis-error-connection))

(define (redis-error command msg connection)
  (raise (condition
	  (make-redis-error command connection)
	  (make-who-condition command)
	  (make-message-condition msg))))

;; API
(define (redis-send-command connection command . args)
  (if (null? args)
      (redis-simple-command connection command)
      (redis-bulk-command connection command args)))

;; just send
(define (redis-simple-command connection command)
  (receive-response command
   (redis-send-request
    connection (string->utf8 (string-append command "\r\n")))))

;; construct Redis array
;; args must be a list of string/integer/bytevector/vector
(define (redis-bulk-command connection command args)
  (define (build-bulk-command command args)
    (let-values (((out extract) (open-bytevector-output-port)))
      (write-redis-array out `#(,command ,@args))
      (extract)))
  (receive-response command
   (redis-send-request
    connection (build-bulk-command command args))))

(define (receive-response command connection)
  (let-values (((type value) (redis-recv-response connection)))
    (if (eq? type 'error)
	(redis-error command value connection)
	value)))

;; R6RS suggest string-ref to be constant time
;; so make use of it
(define (string-contains? s pred)
  (let loop ((i 0))
    (cond ((= i (string-length s)) #f)
	  ((pred (string-ref s i)))
	  (else (loop (+ i 1))))))
(define terminator #vu8(#x0d #x0a))
(define (write-redis-data out data)
  (define (terminator? c) (or (eqv? c #\newline) (eqv? c #\return)))
  (cond ((string? data)
	 (if (string-contains? data terminator?)
	     (write-redis-bulk out (string->utf8 data))
	     (write-redis-string out data)))
	((bytevector? data) (write-redis-bulk out data))
	((number? data)
	 (if (integer? data)
	     (write-redis-integer out data)
	     (write-redis-string out (number->string data))))
	((vector? data) (write-redis-array out data))
	(else (assertion-violation 'redis-command "Invalid data" data))))

(define (write-redis-string out s)
  (put-u8 out (char->integer #\+))
  (put-bytevector out (string->utf8 s))
  (put-bytevector out terminator))

(define (write-redis-bulk out bv)
  (put-u8 out (char->integer #\$))
  (put-bytevector out (string->utf8 (number->string (bytevector-length bv))))
  (put-bytevector out terminator)
  (put-bytevector out bv)
  (put-bytevector out terminator))

(define (write-redis-integer out i)
  (put-u8 out (char->integer #\:))
  (put-bytevector out (string->utf8 (number->string i)))
  (put-bytevector out terminator))
  
(define (write-redis-array out vec)
  (define len (vector-length vec))
  (put-u8 out (char->integer #\*))
  (put-bytevector out (string->utf8 (number->string len)))
  (put-bytevector out terminator)
  (vector-for-each (lambda (e) (write-redis-data out e)) vec))
)
