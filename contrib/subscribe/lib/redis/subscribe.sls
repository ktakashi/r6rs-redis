;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; redis/subscribe.sls - Redis Pub/Sub
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

;; this library is *not* portable due to the lack of portable
;; thread library. I'm kinda lazy to make it since some of the
;; major R6RS implementation (e.g. Larceny) doesn't support
;; threading.
#!r6rs
(library (redis subscribe)
    (export redis-subscription?
	    redis-subscription-waiting?
	    redis-subscribe!
	    redis-psubscribe!
	    redis-unsubscribe!
	    redis-punsubscribe!
	    )
    (import (rnrs)
	    (redis)
	    (redis resp)
	    (srfi :1)
	    (srfi :13)
	    (srfi :18)
	    (srfi :19)
	    (srfi :27))

(define-record-type redis-subscription
  (fields (mutable channels) ;; normal channels
	  (mutable pchannels) ;; pattern channels
	  callback
	  connection
	  input-port
	  output-port
	  id ;; self monitoring for stopping
	  (mutable thread))
  (protocol (lambda (p)
	      (lambda (id connection callback)
		(define in (redis-bidirectional-input-port connection))
		(define out (redis-bidirectional-output-port connection))
		;; pseudo close sort of
		(redis-bidirectional-output-port-set! connection #f)
		;; (redis-bidirectional-input-port-set! connection #f)
		(p '() '() callback connection in out id #f)))))

(define (redis-subscription-waiting? subscription)
  (and (redis-subscription-thread subscription) #t))

(define probably-unique-id
  (let ((count 0))
    (lambda ()
      (define random (random-integer (expt 2 64)))
      (define second (time-second (current-time)))
      (set! count (+ count 1))
      (string-append "$system-"
		     (number->string second 16) "-"
		     (number->string random 16) "-"
		     (number->string count 16)))))

(define stop-command-prefix "$system$command$stop$")
(define (make-stop-command subscription)
  (define id (redis-subscription-id subscription))
  (string->utf8 (string-append stop-command-prefix id)))

(define (callback-hahdler subscription callback)
  (lambda ()
    (define connection (redis-subscription-connection subscription))
    (define id (redis-subscription-id subscription))
    (define stop (make-stop-command subscription))
    (let loop ()
      (let-values (((t b) (redis-recv-response connection)))
	(when (and (eq? t 'array) (positive? (vector-length b)))
	  (let ((e0 (vector-ref b 0)))
	    (when (bytevector? e0)
	      (case (string->symbol (utf8->string e0))
		;; published message
		((message)
		 (let ((channel (utf8->string (vector-ref b 1)))
		       (msg (vector-ref b 2)))
		   (if (string=? channel id)
		       (unless (bytevector=? stop msg) (loop))
		       (begin (callback channel msg) (loop)))))
		((pmessage)
		 ;; we ignore original pattern
		 (let ((channel (utf8->string (vector-ref b 2)))
		       (msg (vector-ref b 3)))
		   (if (string=? channel id)
		       (unless (bytevector=? stop msg) (loop))
		       (begin (callback channel msg) (loop)))))
		((unsubscribe punsubscribe)
		 (cond ((vector-ref b 1)
			(lambda (e1)
			  (let ((channel (utf8->string e1)))
			    ;; in case of all unsubscribed
			    (unless (string=? channel id)
			      (loop)))))
		       ;; psubscribe + unsubscribe returns #f ignore it
		       (else (loop))))
		(else
		 ;; (display (utf8->string e0)) (newline)
		 (loop))))))))))

(define-syntax define-subscribe
  (syntax-rules ()
    ((_ name subscribe command getter setter!)
     (define (name redis-connection callback/channel . channels)
       (define (set-thread subscription callback c*)
	 (redis-subscription-thread-set! subscription
	  (thread-start!
	   (make-thread (callback-hahdler subscription callback))))
	 (setter! subscription c*)
	 subscription)
       (define (discard conn c*)
	 (do ((i 0 (+ i 1)) (len (length c*)))
	     ((= i len))
	   (redis-recv-response conn)))
       (define (make-subscribe id conn callback c*)
	 ;; okey discard here
	 (discard conn c*)
	 (set-thread (make-redis-subscription id conn callback) callback c*))
       (define (with-connection callback channels)
	 (let ((id (probably-unique-id)))
	   (apply subscribe redis-connection id channels)
	   (make-subscribe id redis-connection callback channels)))
       (define (with-subscription channel channels)
	 (define conn (redis-subscription-connection redis-connection))
	 (define id (redis-subscription-id redis-connection))
	 (define thread (redis-subscription-thread redis-connection))
	 (define c* (if thread
			(cons channel channels)
			(cons* id channel channels)))
	 (redis-bidirectional-output-port-set! conn
	  (redis-subscription-output-port redis-connection))
	 (redis-send-request conn
	  (string->utf8 
	   (string-append  (string-join (cons command c*) " ") "\r\n")))
	 (redis-bidirectional-output-port-set! conn #f)
	 (cond (thread
		(setter! redis-connection
			 (append (getter redis-connection) c*)))
	       (else
		(discard conn c*)
		(set-thread redis-connection
		 (redis-subscription-callback redis-connection) (cdr c*))))
	 redis-connection)
	 
       (cond ((redis-connection? redis-connection)
	      (with-connection callback/channel channels))
	     ((redis-subscription? redis-connection)
	      (with-subscription callback/channel channels))
	     (else
	      (assertion-violation 'name
	       "Redis connection/subscription required" redis-connection)))))))

(define-subscribe redis-subscribe!
  redis-subscribe "SUBSCRIBE"
  redis-subscription-channels
  redis-subscription-channels-set!)
(define-subscribe redis-psubscribe!
  redis-psubscribe "PSUBSCRIBE"
  redis-subscription-pchannels
  redis-subscription-pchannels-set!)

(define (common-unsubscribe! subscription command channels)
  (define conn (redis-subscription-connection subscription))
  (define request
    (string-append (string-join (cons command channels) " ") "\r\n"))
  (define (unsubscribe conn command channels)
    (redis-bidirectional-output-port-set! conn
     (redis-subscription-output-port subscription))
    (redis-send-request conn (string->utf8 request))
    (redis-bidirectional-output-port-set! conn #f))
  (unsubscribe conn command channels)
  (when (and (null? (redis-subscription-channels subscription))
	     (null? (redis-subscription-pchannels subscription)))
    ;; if this is null, then it's all unsubscribed, so don't send publish
    (unless (null? channels)
      (call-with-redis-connection (redis-connection-host conn)
       (lambda (new-conn)
	 (redis-publish new-conn (redis-subscription-id subscription)
			(make-stop-command subscription)))
       (redis-connection-port conn)))
    (thread-join! (redis-subscription-thread subscription))
    ;; revive the connection
    (redis-bidirectional-output-port-set!
     conn (redis-subscription-output-port subscription))
    (redis-subscription-thread-set! subscription #f))
  subscription)

(define (redis-unsubscribe! subscription . channels)
  (unless (for-all string? channels)
    (assertion-violation 'redis-unsubscribe! 
			 "Channel must be a string" channels))
  (if (null? channels)
      (redis-subscription-channels-set! subscription '())
      (redis-subscription-channels-set! subscription
       (lset-difference string=?
			(redis-subscription-channels subscription) channels)))
  (common-unsubscribe! subscription "UNSUBSCRIBE" channels))

(define (redis-punsubscribe! subscription . channels)
  (unless (for-all string? channels)
    (assertion-violation 'redis-unsubscribe! "Channel must a string" channels))
  (if (null? channels)
      (redis-subscription-pchannels-set! subscription '())
      (redis-subscription-pchannels-set! subscription
       (lset-difference string=?
			(redis-subscription-pchannels subscription) channels)))
  (common-unsubscribe! subscription "PUNSUBSCRIBE" channels))

)
    
