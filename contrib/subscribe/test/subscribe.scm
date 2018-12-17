#!r6rs
(import (rnrs)
	(redis)
	(redis subscribe)
	(srfi :64))

(test-begin "Redis Pub/Sub")

(let ((conn (redis-connection-open! (make-redis-connection "localhost"))))
  (define (callback channel body)
    (test-equal "callback channel" "test1" channel)
    (test-equal "callback body" "body" (utf8->string body)))

  (test-assert (redis-connection? conn))
  (let ((subscription (redis-subscribe! conn callback "test1")))
    (test-assert "subscription?" (redis-subscription? subscription))
    (call-with-redis-connection "localhost"
     (lambda (conn)
       (test-equal "publish" 1 (redis-publish conn "test1" "body"))))
    (test-assert "unsubscribe (1)" (redis-unsubscribe! subscription)))

  (redis-connection-close! conn))

(test-end)
