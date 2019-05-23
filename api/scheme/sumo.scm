(define-module (sumo)
    #:export
        (reset-sumo-server
        set-sumo-server
        sumo-superclasses
        sumo-subclasses
        sumo-related))


(use-modules (opencog)
             (opencog exec)
             (opencog query))

(use-modules (web client)
             (json))

(define default-sumo-server "http://144.76.153.5:7084")

(define sumo-server default-sumo-server)

(define (reset-sumo-server)
"
    reset-sumo-server - resets the sumo server address
                        back to the default one
                        http://144.76.153.5:7084
"
    (set! sumo-server default-sumo-server))

(define (set-sumo-server addr)
"
    set-sumo-server addr - set the current sumo server 
                           address to addr
                           http://<address>:<port>
"
    (set! sumo-server addr))

(define (get-http-response req)
    (call-with-values
        (lambda () (http-get req))
        (lambda (r-head r-body) r-body)))


(define (fetch-wrap query-type query)
    (define resp
        (get-http-response
            (string-append
                sumo-server
                query-type
                query)))
    (if (not (string-ci=? "" resp))
    (map ConceptNode
        (map cdr
            (cdar
                (json-string->scm resp)))) '()))

(define (sumo-superclasses nod)
"
    sumo-superclasses nod - query the sumo-server for
                            superclasses of nod.
                            nod should be an opencog
                            node with a valid name.
    Return: list of superclass Nodes reachable from nod
"
    (if (cog-atom? nod)
        (fetch-wrap "/superclasses/" (cog-name nod))
        '()))

(define (sumo-subclasses nod)
"
    sumo-subclasses nod - query the sumo-server for
                            subclasses of nod.
                            nod should be an opencog
                            node with a valid name.
    Return: list of subclass Nodes reachable from nod
"
    (if (cog-atom? nod)
        (fetch-wrap "/subclasses/" (cog-name nod))
        '()))

(define (sumo-related depth nod)
    (if (cog-atom? nod)
        (fetch-wrap
            (string-append
                sumo-server
                "/related/"
                (number->string depth)
                "/" (cog-name nod))
            "related"
            nod)
        '()))
