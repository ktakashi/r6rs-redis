#!r6rs
(import (rnrs)
	(redis)
	(redis subscribe)
	(srfi :13)
	(srfi :18)
	(srfi :64))

(test-begin "Redis Pub/Sub")

(let ((conn (redis-connection-open! (make-redis-connection "localhost"))))
  (define (callback channel body)
    (test-assert "callback channel" (string? channel))
    (if (string=? channel "test1")
	(test-equal "callback body" "body1" (utf8->string body))
	(test-equal "callback body" "body2" (utf8->string body))))
  (define (send-publish channel body)
    (call-with-redis-connection "localhost"
     (lambda (conn)
       (test-equal "publish" 1 (redis-publish conn channel body)))))

  (test-assert (redis-connection? conn))
  (let ((subscription (redis-subscribe! conn callback "test1")))
    (test-assert "subscription?" (redis-subscription? subscription))
    (test-assert "waiting (1)" (redis-subscription-waiting? subscription))
    (send-publish "test1" "body1")
    (test-assert "unsubscribe (1)" (redis-unsubscribe! subscription))

    (test-assert "waiting (2)" (not (redis-subscription-waiting? subscription)))
    ;; it should be able to re-subscribe
    (test-assert "subecribe (1)"
		 (redis-subscription? (redis-subscribe! subscription "test1")))
    (test-assert "waiting (3)" (redis-subscription-waiting? subscription))
    (test-assert "subecribe (2)"
		 (redis-subscription? (redis-subscribe! subscription "test2")))
    (send-publish "test1" "body1")
    (send-publish "test2" "body2")
    (test-assert "unsubscribe (2)" (redis-unsubscribe! subscription "test1"))
    (test-assert "unsubscribe (3)" (redis-unsubscribe! subscription "test2"))

    (test-assert "waiting (4)" (not (redis-subscription-waiting? subscription)))
    )

  (redis-connection-close! conn))

;; psubecribe
(let ((conn (redis-connection-open! (make-redis-connection "localhost"))))
  (define mutex (make-mutex))
  (define cv (make-condition-variable))
  (define count 0)
  (define (callback channel body)
    (mutex-lock! mutex)
    (test-assert "callback channel" (string? channel))
    (set! count (+ count 1))
    (if (string=? "test1" channel)
	(test-equal "callback body1" "body1" (utf8->string body))
	(test-equal "callback body2" "body2" (utf8->string body)))
    (condition-variable-broadcast! cv)
    (mutex-unlock! mutex))
  (define (send-publish channel body)
    (call-with-redis-connection "localhost"
     (lambda (conn)
       (test-equal "publish" 1 (redis-publish conn channel body)))))

  (test-assert (redis-connection? conn))
  (let ((subscription (redis-psubscribe! conn callback "t*")))
    (test-assert "subscription?" (redis-subscription? subscription))
    (test-assert "waiting (1)" (redis-subscription-waiting? subscription))
    (send-publish "test1" "body1")
    ;; correct?
    (test-assert "unsubscribe (1)" (redis-unsubscribe! subscription))
    (mutex-unlock! mutex cv)
    (send-publish "test1" "body1")
    (mutex-unlock! mutex)
    (test-equal "count" 1 count)

    (test-assert "waiting (2)" (redis-subscription-waiting? subscription))
    (test-assert "punsubscribe (1)" (redis-punsubscribe! subscription))
    (test-assert "waiting (3)" (not (redis-subscription-waiting? subscription)))
    ;; it should be able to re-subscribe
    (test-assert "psubecribe (1)"
		 (redis-subscription? (redis-psubscribe! subscription "t*")))
    (test-assert "waiting (4)" (redis-subscription-waiting? subscription))
    (test-assert "psubecribe (2)"
		 (redis-subscription? (redis-psubscribe! subscription "f*")))
    (send-publish "test1" "body1")
    (send-publish "test2" "body2")
    (test-assert "punsubscribe (2)" (redis-punsubscribe! subscription "t*"))
    (test-assert "waiting (5)" (redis-subscription-waiting? subscription))
    (test-assert "punsubscribe (3)" (redis-punsubscribe! subscription "f*"))

    (test-assert "waiting (6)" (not (redis-subscription-waiting? subscription)))
    )

  (redis-connection-close! conn))

(test-end)
