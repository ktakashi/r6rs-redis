`(redis)`: R6RS portable Redis client
=====================================

`(redis)` provides R6RS portable Redis client.

APIs
====

The `(redis)` library consists with the following concepts.

Errors
------

These procedures are either conditions or Redis response indicating
an error.

- `(redis-error? obj)`:
  Returns `#t` if the given *obj* is an instance of `&redis`.
- `(redis-command-error? obj)`:
  Returns `#t` if the given *obj* is an instance of `&redis-command`.
- `(redis-error-command redis-command-error)`:
  Return a command name of the given *redis-comand-error*.
- `(redis-error-connection redis-command-error)`:
  Return the redis connection which caused the ghiven *redis-command-error*.
  The connection may or may not be closed.
- `(redis-error-result? obj)`:
  Returns `#t` if the given *obj* is an instance of redis error result.
- `(redis-error-result-value error-result)`
  Returns the value of given *error-result*.

Connection
----------

These procedures are related to Redis connection.

- `(make-redis-connection host)`
- `(make-redis-connection host port)`:
  Creates a Redis connection which connects to the given *host* and *port*. If
  the first form is used, then it uses the default port `6379`.
  
  Both *host* and *port* must be strings.
- `(redis-connection? obj)`:
  Returns `#t` if the given *obj* is an instance of redis connection.
- `(redis-connection-open! redis-connection)`:
  Opens the given connection and returns the given `redis-connection`.
- `(redis-connection-close! redis-connection)`:
  Closes the given connection and returns the given `redis-connection`.
- `(call-with-redis-connection host proc)`
- `(call-with-redis-connection host proc port)`:
  Passing an open redis connection to the given *proc* and returns
  the result of the *proc*. The connection is created with the given
  *host* and *port*, if the second form is used.

You can see the redis error result in the array response.

Pipeline
--------

These procedures provides Redis pipelining utilities

- `(make-redis-pipeline-connection redis-connection)`:
  Creates a pipelining connection from the given *redis-connection*.
  The returning connection can be used as if it's a Redis connection,
  except it can't be opened or closed by calling the
  `redis-connection-open!` or `redis-connection-close!`. And it requires
  flushing commands by calling `redis-pipeline-connection-flush-commends!`.
- `(redis-pipeline-connection? obj)`:
  Returns `#t` if the given *obj* is a pipelining connection.
- `(redis-pipeline-connection-flush-commends! pipeline-connection)`:
  Flushes stored commands and returns a vector of command results.
  The vector may contain the error result in case of errors.

Transaction
-----------

These procedures provides Redis transaction utilities

- `(call-with-redis-transaction redis-connection proc)`:
  Call the given *proc* with Redis transaction object.
  The procedure executes the `MULTI` command before calling the *proc*. And
  it will discards the transaction if the `redis-exec-transaction!` is not
  called.
- `(redis-transaction? obj)`:
  Returns `#t` if the given *obj* is a Redis transaction.
- `(redis-exec-transaction! redis-transaction)`:
  Emits `EXEC` command and returns the result array.
  If this procedure is called, then it will immediately return from the
  `call-with-redis-transaction` procedure. So the procedure must be the
  on the tail position of the `call-with-redis-transaction`.

Commands
--------

The `(redis)` provides all commands Redis provides with the following 
naming convension:

`redis-${lowercase of the command name}`

For example, `GET` command is `redis-get`.

The procedures signatures are like this:

`(redis-$command redis-connection [required args ...] . [optiona args])`

If the command has required argument, then they are also required on the
Scheme procedure. For example, the `BITCOUNT` command requires *key* and
accepts *start* and *end* as its optional argument. So the Scheme signature
looks like this:

`(redis-bitcount redis-connection key . opts)`

In fact, the `BITCOUNT` requires the 2 of the optional argument to be 
provided, but we don't check on Scheme world.

For more detail of the commands, see official document:

- [Redis Commands](https://redis.io/commands)


Limitation
==========

This library is usign [`(usocket)`](https://github.com/ktakashi/r6rs-usocket),
means the implememtation must be supported by the `(usocket)` library.

References
==========

- [Redis Protocol specification](https://redis.io/topics/protocol)

Copyright and lincence
======================

Copyright 2018 Takashi Kato. Code released under the BSD-style
license. See [COPYING](COPYING).
