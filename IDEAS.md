# Ideas

## Persistent storage

* simply dump the index in EDN format
    - works for smaller examples, but probably starts breaking with
      "real" usage (how much?)
* segment the index
    - yes! but how?
    - we want to store things in a key-value database (e.g.
      localStorage)
    - every new "version" (e.g. transaction) creates a new root

        -> copy-on-write (b-)tree? at least we have a root that
        references segments

        but we still don't know what segments are?
    - the index is a nested tree of `E -> A -> V -> T -> [datoms]`, but
      do we have to store it on disk?
    - trivial "solution": store transaction log and load that

        should work, but we'd have to load all of it to access all data
    - if we would have a way to reliably address parts of the index
      (segments), then we might be able to get around that
    - but we don't want to write new nodes on every write, so we want to
      put many nodes into one segment

        but how do we know where data we want to access is? so the
        addressing scheme and what data we put into one segment are
        going to interact in some way
