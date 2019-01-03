#!r6rs
(import (rnrs)
	(redis resp)
	(srfi :64))

(test-begin "Redis RESP")

(test-error assertion-violation? (make-redis-connection "localhost" 1111))

(let ((conn (make-redis-connection "localhost")))
  (test-assert "open" (redis-connection? (redis-connection-open! conn)))
  (test-error "opened (1)" redis-connection-error?
	      (redis-connection-open! conn))
  (test-assert "close (1)" (redis-connection? (redis-connection-close! conn)))
  (test-error "closed (1)" redis-connection-error?
	      (redis-connection-close! conn))
  
  (test-assert "re-open" (redis-connection? (redis-connection-open! conn)))
  ;; simple inline
  (test-assert "send (1)"
   (redis-connection? (redis-send-request conn (string->utf8 "PING\r\n"))))
  (let-values (((type body) (redis-recv-response conn)))
    (test-equal "recv type (1)" 'string type)
    (test-equal "recv body (1)" "PONG" body))

  ;; bulk command
  (test-assert "send (2)"
   (redis-connection? 
    (redis-send-request conn
     (string->utf8 "*2\r\n$4\r\nPING\r\n$5\r\nhello\r\n"))))
  (let-values (((type body) (redis-recv-response conn)))
    (test-equal "recv type (2)" 'bulk type)
    (test-equal "recv body (3)" (string->utf8 "hello") body))

  ;; inline with body
  (test-assert "send (3)"
   (redis-connection?
    (redis-send-request conn (string->utf8 "PING hello\r\n"))))
  (let-values (((type body) (redis-recv-response conn)))
    (test-equal "recv type (3)" 'bulk type)
    (test-equal "recv body (3)" (string->utf8 "hello") body))
  
  (test-assert "close (2)" (redis-connection? (redis-connection-close! conn))))

(test-end)
