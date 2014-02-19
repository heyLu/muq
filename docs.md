# api

    GET / - this document

    GET /entity/:id - get an entity by it's entity id

    GET /entity-by/:key?value=:value - get an entity which has the value for the key

    GET /entities?with=:spec - get all entities matching the spec (see below)

`/entites` supports `start`, `count` and `q` parameters to further filter the results.
the `q` parameter searches all (!) fields of the results for matching values.

# examples

    GET /entity/20
    => the entity whose :db/id is 20

    GET /entity-by/url?value=http://waxy.org/
    => the entity whose :url attribute has the value "http://waxy.org/"

    GET /entities?with=note
    => all entities that have a :note attribute

    GET /entities?with=tags->name:people
    => all entities tagged with "people"
