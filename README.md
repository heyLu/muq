# µq

`µq` is an in-memory soon-to-be temporal database inspired by
[Datomic][].

## Quickstart

Run `lein cljsbuild once default` and then open `muq.html` in your browser.

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
* on-disk storage, preferably in a key-value store (e.g. `localStorage`)
* support the schema
* ...

## Ideas

* quantified self
* making interaction data explorable (making asking questions about ones
    own activities easier, allow storage of loosely structured, interlinked
    data about documents on the web)
* as an ai in games (not for something like pathfinding, but for asking
    questions about the game world and other entities in it. "where can i
    buy X, which nearby system has an occurrence of Y?" but that requires
    a bit more thought, most likely something custom-made fits better.)

[Datomic]: http://datomic.com
