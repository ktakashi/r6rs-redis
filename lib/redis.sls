;;; -*- mode:scheme; coding:utf-8 -*-
;;;
;;; redis.sls - Redis client for R6RS
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
(library (redis)
    (export redis-error?
	    redis-connection-error?
	    redis-error-connection
	    redis-command-error?
	    redis-error-command

	    redis-error-result? redis-error-result-value
	    ;; connection
	    make-redis-connection
	    redis-connection?
	    redis-connection-open!
	    redis-connection-close!
	    call-with-redis-connection

	    ;; pipeline
	    make-redis-pipeline-connection
	    redis-pipeline-connection?
	    redis-pipeline-connection-flush-commends!

	    ;; transaction
	    call-with-redis-transaction
	    redis-transaction?
	    redis-exec-transaction!
	    
	    ;; commands
	    redis-append
            redis-auth
            redis-bgrewriteaof
            redis-bgsave
            redis-bitcount
            redis-bitfield
            redis-bitop
            redis-bitpos
            redis-blpop
            redis-brpop
            redis-brpoplpush
            redis-bzpopmin
            redis-bzpopmax
            redis-client-id
            redis-client-kill
            redis-client-list
            redis-client-getname
            redis-client-pause
            redis-client-reply
            redis-client-setname
            redis-client-unblock
            redis-cluster-addslots
            redis-cluster-count-failure-reports
            redis-cluster-countkeysinslot
            redis-cluster-delslots
            redis-cluster-failover
            redis-cluster-forget
            redis-cluster-getkeysinslot
            redis-cluster-info
            redis-cluster-keyslot
            redis-cluster-meet
            redis-cluster-nodes
            redis-cluster-replicate
            redis-cluster-reset
            redis-cluster-saveconfig
            redis-cluster-set-config-epoch
            redis-cluster-setslot
            redis-cluster-slaves
            redis-cluster-replicas
            redis-cluster-slots
            redis-command
            redis-command-count
            redis-command-getkeys
            redis-command-info
            redis-config-get
            redis-config-rewrite
            redis-config-set
            redis-config-resetstat
            redis-dbsize
            redis-debug-object
            redis-debug-segfault
            redis-decr
            redis-decrby
            redis-del
            redis-discard
            redis-dump
            redis-echo
            redis-eval
            redis-evalsha
            redis-exec
            redis-exists
            redis-expire
            redis-expireat
            redis-flushall
            redis-flushdb
            redis-geoadd
            redis-geohash
            redis-geopos
            redis-geodist
            redis-georadius
            redis-georadiusbymember
            redis-get
            redis-getbit
            redis-getrange
            redis-getset
            redis-hdel
            redis-hexists
            redis-hget
            redis-hgetall
            redis-hincrby
            redis-hincrbyfloat
            redis-hkeys
            redis-hlen
            redis-hmget
            redis-hmset
            redis-hset
            redis-hsetnx
            redis-hstrlen
            redis-hvals
            redis-incr
            redis-incrby
            redis-incrbyfloat
            redis-info
            redis-keys
            redis-lastsave
            redis-lindex
            redis-linsert
            redis-llen
            redis-lpop
            redis-lpush
            redis-lpushx
            redis-lrange
            redis-lrem
            redis-lset
            redis-ltrim
            redis-memory-doctor
            redis-memory-help
            redis-memory-malloc-stats
            redis-memory-purge
            redis-memory-stats
            redis-memory-usage
            redis-mget
            redis-migrate
            redis-monitor
            redis-move
            redis-mset
            redis-msetnx
            redis-multi
            redis-object
            redis-persist
            redis-pexpire
            redis-pexpireat
            redis-pfadd
            redis-pfcount
            redis-pfmerge
            redis-ping
            redis-psetex
            redis-psubscribe
            redis-pubsub
            redis-pttl
            redis-publish
            redis-punsubscribe
            redis-quit
            redis-randomkey
            redis-readonly
            redis-readwrite
            redis-rename
            redis-renamenx
            redis-restore
            redis-role
            redis-rpop
            redis-rpoplpush
            redis-rpush
            redis-rpushx
            redis-sadd
            redis-save
            redis-scard
            redis-script-debug
            redis-script-exists
            redis-script-flush
            redis-script-kill
            redis-script-load
            redis-sdiff
            redis-sdiffstore
            redis-select
            redis-set
            redis-setbit
            redis-setex
            redis-setnx
            redis-setrange
            redis-shutdown
            redis-sinter
            redis-sinterstore
            redis-sismember
            redis-slaveof
            redis-replicaof
            redis-slowlog
            redis-smembers
            redis-smove
            redis-sort
            redis-spop
            redis-srandmember
            redis-srem
            redis-strlen
            redis-subscribe
            redis-sunion
            redis-sunionstore
            redis-swapdb
            redis-sync
            redis-time
            redis-touch
            redis-ttl
            redis-type
            redis-unsubscribe
            redis-unlink
            redis-unwatch
            redis-wait
            redis-watch
            redis-zadd
            redis-zcard
            redis-zcount
            redis-zincrby
            redis-zinterstore
            redis-zlexcount
            redis-zpopmax
            redis-zpopmin
            redis-zrange
            redis-zrangebylex
            redis-zrevrangebylex
            redis-zrangebyscore
            redis-zrank
            redis-zrem
            redis-zremrangebylex
            redis-zremrangebyrank
            redis-zremrangebyscore
            redis-zrevrange
            redis-zrevrangebyscore
            redis-zrevrank
            redis-zscore
            redis-zunionstore
            redis-scan
            redis-sscan
            redis-hscan
            redis-zscan
            redis-xinfo
            redis-xadd
            redis-xtrim
            redis-xdel
            redis-xrange
            redis-xrevrange
            redis-xlen
            redis-xread
            redis-xgroup
            redis-xreadgroup
            redis-xack
            redis-xclaim
            redis-xpending
	    
	    ;; Low level
	    redis-send-command)
    (import (rnrs)
	    (redis api)
	    (redis errors)
	    (redis commands)
	    (redis pipeline)
	    (redis transaction)
	    (redis resp))

(define (call-with-redis-connection host proc . maybe-port)
  (let ((conn (apply make-redis-connection host maybe-port)))
    (let-values ((r (proc (redis-connection-open! conn))))
      (redis-connection-close! conn)
      (apply values r))))

)
	    
