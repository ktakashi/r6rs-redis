#!r6rs
(import (rnrs)
	(redis transaction)
	(redis resp)
	(redis errors)
	(redis commands)
	(srfi :64))

(test-begin "Redis transaction")

(test-assert call-with-redis-transaction)
(let ((conn (make-redis-connection "localhost")))
  (redis-connection-open! conn)
  (test-equal "exec (1)" '#(1 1)
	      (call-with-redis-transaction conn
	       (lambda (transaction)
		 (redis-incr transaction "foo")
		 (redis-incr transaction "bar")
		 (redis-exec-transaction! transaction))))
  (test-equal "get foo (1)" (string->utf8 "1") (redis-get conn "foo"))
  (test-equal "get bar (1)" (string->utf8 "1") (redis-get conn "bar"))
  (test-assert (redis-del conn "foo"))
  (test-assert (redis-del conn "bar"))

  (test-equal "discard (1)" "OK"
	      (call-with-redis-transaction conn
	       (lambda (transaction)
		 (redis-incr transaction "foo")
		 (redis-incr transaction "bar"))))

  (test-equal "get foo (2)" #f (redis-get conn "foo"))
  (test-equal "get bar (2)" #f (redis-get conn "bar"))

  (test-error "error during transaction"
   (call-with-redis-transaction conn
    (lambda (transaction)
      (redis-incr transaction "bla")
      (redis-send-request transaction (string->utf8 "APPEND foo bar bla"))
      (redis-exec-transaction! transaction))))
  (test-equal "get bla" #f (redis-get conn "bla"))

  (let ((r (call-with-redis-transaction conn
	    (lambda (transaction)
	      (redis-set transaction "a" 3)
	      (redis-lpop transaction "a")
	      (redis-exec-transaction! transaction)))))
    (test-equal "error after transaction"
		'#(#f #t) (vector-map redis-error-result? r))))

(test-end)
