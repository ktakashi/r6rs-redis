Redis subscribe
===============

`(redis subscribe)` provides an utility of 
[Redis Pub/Sub](https://redis.io/topics/pubsub). The basic idea is basically
a callback model.

APIs
====

- `(redis-subscription? obj)`:
  Returns `#t` if the given *obj* is a Redis subscription.
- `(redis-subscription-waiting? subscription)`:
  Returns `#t` if the given *subscription* is waiting for messages.
- `(redis-subscribe! connection callback channel . channels)`:
  Creates a Redis subsctiption which uses *callback* as a callback
  procedure and subscribe to *channel(s)*. Once the subscription is
  created, then the givne *connection* will be invalidated until
  all subscribed channels are unsubscribed.
- `(redis-subscribe! subscription channel . channels)`:
  Add given *channel(s)* to the *subscription*.
- `(redis-psubscribe! connection callback channel . channels)`:
  The same as `redis-subscribe!` but uses `PSUBSCRIBE` command.
- `(redis-psubscribe! subscription channel . channels)`:
  The same as `redis-subscribe!` but uses `PSUBSCRIBE` command.
- `(redis-unsubscribe! subscription . channels)`:
  Unsubscribe given *channels* from the *subscription*. If there's
  no channel is given, then it unsubscreibes all the subscription.

  Once all the subscriptions are unsubscribed, then the connection
  of the *subscription* will be available again.
- `(redis-punsubscribe! subscription . channels)`:
  The same as `redis-unsubscribe!` but using `PUNSUBSCRIBE`.

Callback
--------

The *callback* passed to either `redis-subscribe!` or `redis-psubscribe!`
must accept 2 arguments, *channel* and *body*. The *channel* is a string
which is the name of the channel which receives the message. The *body*
is a bytevector which is the content of the received message.

If the *callback* throws an error, then entire backend thred will
stops, so it is the users responsibility not to do so.

