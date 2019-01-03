;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; redis/resp.sls - REdis Serialization Protocol
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
(library (redis resp)
    (export redis-send-request
	    redis-recv-response

	    &redis-connection
	    redis-connection-error?
	    redis-error-connection
	    (rename (redis-bidirectional <redis-bidirectional>))
	    redis-bidirectional? 
	    redis-bidirectional-input-port
	    redis-bidirectional-input-port-set!
	    redis-bidirectional-output-port
	    redis-bidirectional-output-port-set!
	    (rename
	     (redis-bidirectional-sent redis-bidirectional-after-sending))

	    make-redis-connection
	    redis-connection-host
	    redis-connection-port
	    redis-connection?
	    redis-connection-open!
	    redis-connection-close!)
    (import (rnrs)
	    (redis errors)
	    (usocket))

(define-record-type redis-bidirectional
  (fields (mutable input-port)
	  (mutable output-port)
	  sent))
    
(define-record-type redis-connection
  (parent redis-bidirectional)
  (fields host
	  port
	  (mutable socket))
  (protocol (lambda (n)
	      (lambda (host . maybe-port)
		(define port (if (null? maybe-port)
				 "6379"
				 (car maybe-port)))
		(unless (string? port)
		  (assertion-violation 'make-redis-connection
				       "Port must be a string" port))
		((n #f #f (lambda (me command) #f))
		 host port #f)))))

(define-condition-type &redis-connection &redis
  make-redis-connection-error redis-connection-error?
  (connection redis-error-connection))

(define (redis-connection-error who conn msg)
  (raise (condition
	  (make-redis-connection-error conn)
	  (make-i/o-error)
	  (make-who-condition who)
	  (make-message-condition msg))))

(define (redis-connection-open! connection)
  (when (redis-connection-socket connection)
    (redis-connection-error 'redis-connection-open! connection
			    "Already connected"))
  (let* ((sock (make-tcp-client-usocket (redis-connection-host connection)
					(redis-connection-port connection)))
	 (in (client-usocket-input-port sock))
	 (out (client-usocket-output-port sock)))
    (redis-connection-socket-set! connection sock)
    (redis-bidirectional-input-port-set! connection in)
    (redis-bidirectional-output-port-set! connection out)
    connection))
(define (redis-connection-close! connection)
  (let ((sock (redis-connection-socket connection)))
    (unless sock
      (redis-connection-error 'redis-connection-close! connection
			      "Not connected"))
    (usocket-shutdown! sock *usocket:shutdown-read&write*)
    (usocket-close! sock)
    (redis-connection-socket-set! connection #f)
    (redis-bidirectional-input-port-set! connection #f)
    (redis-bidirectional-output-port-set! connection #f)
    connection))

;; [Low API]: the request must be a bytevector
(define (redis-send-request connection bv)
  (unless (redis-bidirectional-output-port connection)
    (redis-connection-error 'redis-send-request connection
			    "Connection is closed"))
  (let ((out (redis-bidirectional-output-port connection)))
    (put-bytevector out bv)
    (flush-output-port out)
    ((redis-bidirectional-sent connection) connection bv)
    connection))

;; [Low API]: receive response.
;; (connection) -> (values type body)
(define (redis-recv-response connection)
  (unless (redis-bidirectional-input-port connection)
    (redis-connection-error 'redis-recv-response
			    connection "Connection is closed"))
  (let ((in (redis-bidirectional-input-port connection)))
    (read-response in)))

;; [internal]
(define (read-response in)
  (define u8 (get-u8 in))
  (case u8
    ((#x2b) ;; +
     (read-simple-string in))
    ((#x2d) ;; -
     (read-error in))
    ((#x3a) ;; :
     (read-integer in))
    ((#x24) ;; $
     (read-bulk-string in))
    ((#x2a) ;; *
     (read-array in))
    (else
     (if (eof-object? u8)
	 (values 'eof u8) ;; in case of expansion (i.e. pipeline)
	 (error 'redis-recv-response "Unknown type" u8)))))

(define (read-until-crlf in)
  (let-values (((out extract) (open-bytevector-output-port)))
    ;; Should we care about EOF here?
    (let loop ()
      (let ((u8 (get-u8 in)))
	(case u8
	  ((#x0d) ;; \r
	   (let ((n (get-u8 in)))
	     (case n
	       ;; \n
	       ((#x0a) (extract))
	       (else (put-u8 out u8) (put-u8 out n) (loop)))))
	  (else (put-u8 out u8) (loop)))))))

(define (read-simple-string in)
  (let ((bv (read-until-crlf in)))
    (values 'string (utf8->string bv))))

(define (read-error in)
  (let ((bv (read-until-crlf in)))
    (values 'error (utf8->string bv))))

(define (read-integer in)
  (let* ((s (utf8->string (read-until-crlf in)))
	 (n (string->number s)))
    ;; Should never happen...
    (unless n (error 'redis-recv-response "Unknown integer" s))
    (values 'integer n)))

(define (terminate? in) (and (eqv? #x0d (get-u8 in)) (eqv? #x0a (get-u8 in))))
(define (read-bulk-string in)
  (define (check-terminate in n bulk)
    (unless (terminate? in)
      ;; should never happen
      (error 'redis-recv-response "Invalid bulk string size" n))
    (values 'bulk bulk))
  (let-values (((t n) (read-integer in)))
    (if (negative? n)
	(values 'bulk #f)
	(check-terminate in n (get-bytevector-n in n)))))

(define (read-array in)
  (define (return array) (values 'array array))
  (let-values (((t n) (read-integer in)))
    (cond ((zero? n) (return '#()))
	  ((negative? n) (return #f))
	  (else 
	   (do ((i 0 (+ i 1)) (v (make-vector n)))
	       ((= i n) (return v))
	     ;; we wrap error response
	     (let-values (((t b) (read-response in)))
	       (vector-set! v i (if (eq? t 'error)
				    (make-redis-error-result b)
				    b))))))))

)
