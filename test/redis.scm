#!r6rs
(import (rnrs)
	(redis)
	(srfi :64))

(test-begin "Redis client")

(call-with-redis-connection "localhost"
  (lambda (conn)
    (test-equal "PING command" "PONG" (redis-ping conn))
    (let ((v (redis-time conn)))
      (test-assert "TIME command (1)" (vector? v))
      (test-equal  "TIME command (2)" 2 (vector-length v))
      ;; multi bulk array
      (test-assert "TIME command (3)" (bytevector? (vector-ref v 0)))
      (test-assert "TIME command (4)" (bytevector? (vector-ref v 1))))

    ;; set (with integer)
    (test-equal "set (1)" "OK" (redis-set conn "key" 1))
    (test-equal "incr (1)" 2 (redis-incr conn "key"))
    (test-equal "get (1)" (string->utf8 "2") (redis-get conn "key"))
    
    ;; set (with string)
    (test-equal "set (2)" "OK" (redis-set conn "key2" "1"))
    (test-equal "incr (2)" 2 (redis-incr conn "key2"))
    (test-equal "get (2)" (string->utf8 "2") (redis-get conn "key2"))

    ;; set empty string
    (test-equal "set (3)" "OK" (redis-set conn "key3" ""))
    (test-equal "get (3)" #vu8() (redis-get conn "key3"))

    ;; get non existing key
    (test-equal "get (4)" #f (redis-get conn "key-not-exists"))
    ))

(test-end)
