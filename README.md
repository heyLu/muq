# µtomic

`µtomic` is an in-memory soon-to-be temporal database inspired by
[Datomic][].

## Quickstart

Run `lein cljsbuild once default` and then open `mutomic.html` in your browser.

Alternatively, have a look in the queries in `src/learn_datalog_today.clj`.

## Why?

* I just had to. And starting was both challenging and possible. How could I not?
* Runs in ClojureScript, too.

    I want to use it in places where having a JVM is too heavy.

    Also, having this thing run in the browser is just plain cool.

## Next up

* actually put some data in it (e.g. run on node, serialize to EDN/...,
    have a tiny transactor-like thing, allow querying for time)
* a comprehensive set of queries
* think about a better index
* support the schema
* ...

[Datomic]: http://datomic.com
