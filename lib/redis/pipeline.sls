;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; redis/pipeline.sls - Redis pipeline
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

;; reference: https://redis.io/topics/pipelining
;; For pipelining, we creates an wrapper connection to buffer
;; all commands users want to pipeline. With this manner, the
;; library can see how many commands are pipelining and how
;; many times we need to receive the response.
#!r6rs
(library (redis pipeline)
    (export make-redis-pipeline-connection
	    redis-pipeline-connection?
	    redis-pipeline-connection-flush-commends!

	    redis-pipeline-error?
	    (rename (redis-error-result-value redis-pipeline-error-value)))
    (import (rnrs)
	    (redis resp)
	    (redis errors))

(define-record-type redis-pipeline-error
  (parent <redis-error-result>))

(define-record-type redis-pipeline-connection
  (parent <redis-bidirectional>)
  (fields connection
	  extractor
	  (mutable count))
  (protocol (lambda (n)
	      (lambda (redis-connection)
		(unless (redis-connection? redis-connection)
		  (assertion-violation 'make-redis-pipeline-connection
		    "Redis connection required" redis-connection))
		(let-values (((out extract) (open-bytevector-output-port)))
		  ((n (open-bytevector-input-port #vu8()) out count-up)
		   redis-connection extract 0))))))
(define (count-up pipeline command)
  (redis-pipeline-connection-count-set! pipeline
   (+ (redis-pipeline-connection-count pipeline) 1)))

(define (redis-pipeline-connection-flush-commends! pipeline)
  (let ((conn (redis-send-request
	       (redis-pipeline-connection-connection pipeline)
	       ((redis-pipeline-connection-extractor pipeline))))
	(count (redis-pipeline-connection-count pipeline)))
    ;; reset count
    (redis-pipeline-connection-count-set! pipeline 0)
    (do ((i 0 (+ i 1)) (vec (make-vector count)))
	((= i count) vec)
      (let-values (((t v) (redis-recv-response conn)))
	(vector-set! vec i (if (eq? t 'error)
			     (make-redis-pipeline-error v)
			     v))))))
)
