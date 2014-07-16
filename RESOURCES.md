# Resources

## Datomic

* [website](http://datomic.com) & [documentation](http://docs.datomic.com)
* created by Rich Hickey, the inventor of Clojure, and other people at Metadata Partners,
    [Relevance](http://thinkrelevance) and now [Cognitect](http://cognitect.com);
    including Stuart Halloway, Micheal Fogus and probably a lot more people
* [Day of Datomic](https://github.com/Datomic/day-of-datomic), source code & tutorial
    by the people who created it
* [Learn Datalog Today!](http://www.learndatalogtoday.org/), teaching the dialect of
    Datalog that is the query language of Datomic
* talks & videos
    - [The Design of Datomic](http://www.infoq.com/presentations/The-Design-of-Datomic)
    - in general everything by Rich Hickey, some are on [InfoQ](http://www.infoq.com/author/Rich-Hickey),
        I recommend [The Value of Values](http://www.infoq.com/presentations/Value-Values)
        and an [interview with him](https://web.archive.org/web/20111224165834/http://www.codequarterly.com/2011/rich-hickey)
        to understand the thinking behind Clojure & Datomic
* random:
    - it's immutable
    - the db is a value in your program
    - you can query the history of everything
    - you can query multiple dbs
    - there is a schema for attributes
    - but everything else is up to you (e.g. which entities should have which attributes)

## Purely functional data structures

* [Purely Functional Data Structures (thesis)](https://www.cs.cmu.edu/~rwh/theses/okasaki.pdf), by Chris Okasaki, the book of the same name is an expanded version of this
* [What's new in purely functional data structures since Okasaki?](http://cstheory.stackexchange.com/questions/1539/whats-new-in-purely-functional-data-structures-since-okasaki)
* [Extreme Cleverness: Functional Data Structures in Scala](http://www.infoq.com/presentations/Functional-Data-Structures-in-Scala) by Daniel Spiewak (video), a nicely presented, fairly broad (lists, queues, trees, vectors) presentation about the data structures in scala (and other languages)
    - [slides](http://dl.dropbox.com/u/1679797/NE%20Scala/Extreme%20Cleverness.pdf), [code in Scala](https://github.com/djspiewak/extreme-cleverness/tree/master/src/main/scala/com/codecommit/collection)
    - I've [implemented](https://github.com/heyLu/lp/blob/master/hs/DataStructures.hs) some of them in Haskell
