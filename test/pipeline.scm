#!r6rs
(import (rnrs)
	(redis resp)
	(redis pipeline)
	(srfi :64))

(test-begin "Redis pipeline")

(test-error assertion-violation? (make-redis-pipeline-connection #f))

(define-syntax test-values
  (syntax-rules ()
    ((_ msg expected expr)
     (test-equal msg expected (let-values ((r expr)) r)))))

(define cont (list 'eof (eof-object)))
(let* ((conn (make-redis-connection "localhost"))
       (pipeline (make-redis-pipeline-connection conn)))
  (define u8* string->utf8)
  (test-assert "push (1)" (redis-send-request pipeline (u8* "PING\r\n")))
  (test-assert "push (2)" (redis-send-request pipeline (u8* "PING\r\n")))
  (test-assert "push (3)" (redis-send-request pipeline (u8* "PING\r\n")))

  (test-values "recv (1)" cont (redis-recv-response pipeline))
  
  (redis-connection-open! conn)
  (let ((r (redis-pipeline-connection-flush-commends! pipeline)))
    (test-assert "result" (vector? r))
    (test-equal "count (1)" 3 (vector-length r))
    (test-equal "results (1)" '#("PONG" "PONG" "PONG") r))

  (test-assert "push (4)" (redis-send-request pipeline (u8* "PING\r\n")))
  (test-assert "push (5)" (redis-send-request pipeline (u8* "INCR\r\n")))

  (let ((r (redis-pipeline-connection-flush-commends! pipeline)))
    (test-equal "count (2)" 2 (vector-length r))
    (test-equal "results (2)" '#(#f #t)
		(vector-map redis-pipeline-error? r)))

  (redis-connection-close! conn))

(test-end)
