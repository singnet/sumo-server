# SUMO Server

This directory contains a Dockerfile for creating a SUMO server container.
The guile script loads the ontologies into the AtomSpace and starts the server
for queries. The default port is 7084 but it can be set to a different one by
exporting the environment variable SUMO_SERVER_PORT.

## Building and Running The Image

To build the image run:
```
  docker image build -t sumo_server .
```

After setting the evironment variables SUMO_SERVER_PORT,
start the server with:
```
  docker run -p $SUMO_SERVER_PORT:$SUMO_SERVER_PORT \
        -w /opt/sumo-server \
        -e SUMO_SERVER_PORT \
        sumo_server \
        guile --no-auto-compile -l sumo-server.scm
```

## Query
Currently, the following type of requests are supported
    * /subclasses/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]
    * /superclasses/X/[depth]/d]/[whitelist/h,b,z]/[blacklist/d,e,f]
    * /related/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]

 each pair of segments must have the format: REQ_TYPE/REQ
 where REQ_TYPE can be one of :
    subclassses, superclasses, related, depth, whitelist, blacklist 
 
/related/X would fetch ConceptNodes related to X until results start to
get redundany. The depth limits how far the relationship should go.
A depth of 0 means immediate/direct relations.
These relations are InheritanceLinks.

/subclasses/X would fetch every descendant of (ConceptNode "X")
for example if the following exists in the AtomSpace:
```
  (InheritanceLink (ConceptNode "chordate") (ConceptNode "animalia"))
  (InheritanceLink (ConceptNode "arthropoda") (ConceptNode "animalia"))
  (InheritanceLink (ConceptNode "mammal") (ConceptNode "chordate"))
  (InheritanceLink (ConceptNode "insect") (ConceptNode "arthropoda"))
  (InheritanceLink (ConceptNode "primate") (ConceptNode "mammal"))
```
The query `/subclasses/chordate` would respond with
```
{
  "SubClasses" :
  {
    "Concept" : "mammal",
    "Concept" : "primate",
  }
}
```
whereas the query `/superclasses/chordate` would respond with
```
{
  "SuperClasses" :
  {
    "Concept" : "animalia",
  }
}
```
The above queries can be black/white listed by using /whitelist/Y /blacklist/Z
segments. The values Y and Z should be comma delimited list of SUMO categories.

## SUMO Files
The onotologies are arranged in different categories and when the server is
started, each category of ontologies are loaded into separate atomspaces.
Each atomspace named in the whitelist is searched and each atomspace named
in blacklist is skipped.

For example after starting the server with following parameters,
```
  export SUMO_SERVER_PORT=7084
  docker run -p $SUMO_SERVER_PORT:$SUMO_SERVER_PORT \
        -w /opt/sumo-server \
        -e SUMO_SERVER_PORT \
        sumo_server \
        guile --no-auto-compile -l sumo-server.scm
```
The query
```
  curl localhost:7084/superclasses/spinach
```
will return

```
{
  "SuperClasses" : 
  {
    "Concept" : "leafy_green_vegetable",
    "Concept" : "fruit_or_vegetable",
    "Concept" : "vegetable"
  }
}
```

## Scheme client API

A client api for Scheme is provided in a scheme module in api/scheme/sumo.scm
To use the API,
```
  (use-modules (sumo))
  (set-sumo-server "http://localhost:7083")
  (sumo-search (ConceptNode "device") "related" #:depth 0 #:whitelist "Transportation" #:blacklist "Cars")
```
The above query would result in the following atomese
```
(EvaluationLink
    (PredicateNode "Related")
    (ListLink
        (ConceptNode "canal_lock_gate")
        (ConceptNode "device")
    )
)
```
