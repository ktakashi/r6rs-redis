#!r6rs
(import (rnrs)
	(redis)
	(srfi :64))

(test-begin "Redis client")

(call-with-redis-connection "localhost"
  (lambda (conn)
    (test-equal "PING command" "PONG" (redis-ping conn))
    ))

(test-end)
