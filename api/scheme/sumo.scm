(define-module (sumo)
    #:export
        (reset-sumo-server
        set-sumo-server
        sumo-search
        sumo-categories))


(use-modules (opencog)
             (opencog exec)
             (opencog query))

(use-modules (web client)
             (ice-9 regex)
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

(define (load-ontologies onts)
    (get-http-response
        (string-append
            sumo-server
            "/loadontologies/"
            onts)))


(define (get-http-response req)
    (call-with-values
        (lambda () (http-get req))
        (lambda (r-head r-body) r-body)))


(define (fetch-wrap relation query query-node)
    (define resp
        (get-http-response
            (string-append
                sumo-server
                query)))
    (define (resp-atom nod)
        (EvaluationLink
            (PredicateNode relation)
            (ListLink
                nod
                query-node)))
    (if (not (string-ci=? "" resp))
    (map resp-atom (map ConceptNode
        (map cdr
            (cdar
                (json-string->scm resp))))) '()))

(define (sumo-categories)
"
    sumo-categories - fetch a list of categories for white/blacklisting queries

    return: a list of categories
"
    (string-split
        (get-http-response
            (string-append sumo-server "/cats"))
        #\,))

(define search-options (list "subclasses" "superclasses" "related"))

(define* (sumo-search
            nod
            search-type
            #:key
                (depth 0)
                (whitelist "")
                (blacklist ""))
"
    sumo-search - query the sumo-server for either
                  superclasses, subclasses of or related to nod.
          (fixed)
            nod         - nod should be an opencog
                          ConceptNode with a valid name.
            search-type - either of <superclasses> <subclasses> or <related>
          (optionals)
            depth       - depth of recursion
            whitelist   - comma delimited list of
                          SUMO categories to search
            blacklist   - comma delimited list of
                          SUMO categories to avoid search

    Return: atomese of Nodes reachable from nod with
            relationship-type as predicate
"
    (if (and (cog-atom? nod) (member search-type search-options))
            (let ((req-c (string-append "/" search-type "/" (cog-name nod)
                                         "/depth/" (number->string depth))))
            (if (not (equal? whitelist ""))
                (set! req-c (string-append req-c "/whitelist/" whitelist)))
            (if (not (equal? blacklist ""))
                (set! req-c (string-append req-c "/blacklist/" blacklist)))
            (fetch-wrap (string-capitalize search-type) req-c nod)
            )
        '()))

