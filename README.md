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

After setting the necessary evironment variables like SUMO_SERVER_PORT and LOAD_SUMO_DATA,
start the server with:
```
  docker run -p $SUMO_SERVER_PORT:$SUMO_SERVER_PORT \
        -w /opt/sumo-server \
        -e LOAD_SUMO_DATA \
        -e SUMO_SERVER_PORT \
        sumo_server \
        guile --no-auto-compile -l sumo-server.scm
```

## Query
Currently, /subclasses/X , /superclasses/X and /related/<depth>/X are available.
/related/<depth>/X would fetch ConceptNodes related to X. The depth limits
how far the relationship should go. A depth of 0 means immediate/direct relations.
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



## SUMO Files
The onotologies are arranged in different categories and one must set the environment
variable LOAD_SUMO_DATA before starting the server with a list of the categories
that need to be loaded into the AtomSpace delimited with ":"
If the server is started without setting the environment variable, the options will
be displayed. To load all the ontologies set LOAD_SUMO_DATA to `all-sumo-labeled-kb

For example after starting the server with following parameters,
```
  export LOAD_SUMO_DATA="Food:Dining:Economy"
  export SUMO_SERVER_PORT=7084
  docker run -p $SUMO_SERVER_PORT:$SUMO_SERVER_PORT \
        -w /opt/sumo-server \
        -e LOAD_SUMO_DATA \
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

