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

	    make-redis-connection
	    redis-connection?
	    redis-connection-open!
	    redis-connection-close!)
    (import (rnrs)
	    (usocket))

(define-record-type redis-connection
  (fields host
	  port
	  (mutable socket))
  (protocol (lambda (p)
	      (lambda (host . maybe-port)
		(define port (if (null? maybe-port)
				 "6379"
				 (car maybe-port)))
		(unless (string? port)
		  (assertion-violation 'make-redis-connection
				       "Port must be a string" port))
		(p host port #f)))))

(define (redis-connection-open! connection)
  (when (redis-connection-socket connection)
    (assertion-violation 'redis-connection-open! "Already connected"))
  (let ((sock (make-tcp-client-usocket (redis-connection-host connection)
				       (redis-connection-port connection))))
    (redis-connection-socket-set! connection sock)
    connection))
(define (redis-connection-close! connection)
  (let ((sock (redis-connection-socket connection)))
    (unless sock (assertion-violation 'redis-connection-close! "Not connected"))
    (usocket-shutdown! sock *usocket:shutdown-read&write*)
    (usocket-close! sock)
    (redis-connection-socket-set! connection #f)
    connection))

;; internal for now?
(define redis-connection-open? redis-connection-socket)

;; [Low API]: the request must be a bytevector
(define (redis-send-request connection bv)
  (unless (redis-connection-open? connection)
    (assertion-violation 'redis-send-request "Connection is closed" connection))
  (let ((out (client-usocket-output-port (redis-connection-socket connection))))
    (put-bytevector out bv)
    (flush-output-port out)
    connection))

;; [Low API]: receive response.
;; (connection) -> (values type body)
(define (redis-recv-response connection)
  (unless (redis-connection-open? connection)
    (assertion-violation 'redis-recv-response
			 "Connection is closed" connection))
  (let ((in (client-usocket-input-port (redis-connection-socket connection))))
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
    (else (error 'redis-recv-response "Unknown type" u8))))

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
      (error 'redis-recv-response "Invalid bulk string size" n))
    (values 'bulk bulk))
  (let-values (((t n) (read-integer in)))
    (if (negative? n)
	(check-terminate in n #f)
	(check-terminate in n (get-bytevector-n in n)))))

(define (read-array in)
  (define (check-terminate in n array)
    (unless (terminate? in)
      (error 'redis-recv-response "Invalid array" n))
    (values 'array array))
  (let-values (((t n) (read-integer in)))
    (cond ((zero? n) (check-terminate in n '#()))
	  ((negative? n) (check-terminate in n #f))
	  (else 
	   (do ((i 0 (+ i 1)) (v (make-vector n)))
	       ((= i n) (check-terminate in n v))
	     (vector-set! v i (read-response in)))))))

)
