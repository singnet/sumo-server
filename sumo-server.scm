
(add-to-load-path "/usr/local/share/guile/site/2.2")
(add-to-load-path "/usr/local/share/guile/2.2")
(add-to-load-path ".")

(use-modules (opencog)
             (opencog query))
(load-from-path "opencog.scm")

(use-modules (web server)
             (web request)
             (web response)
             (web uri))

(use-modules (opencog exec))

(use-modules (srfi srfi-1)
             (ice-9 format)
             (ice-9 regex)
             (json))

(define sumo-port 7083)
(if (getenv "SUMO_SERVER_PORT")
    (set! sumo-port (string->number (getenv "SUMO_SERVER_PORT"))))

(define LOAD-SUMO-ENVVAR "LOAD_SUMO_DATA")

(define (get-sumo-files)
    (define (get-file-name s)
        (list-ref (string-split s #\.) 0))
    (map get-file-name (list-files "./sumo-data/")))

; create list of ontology categories
(define (get-cats)
    (define (rm-ext s) (string-drop-right s 4))
    (map rm-ext (list-files "./sumo-data/")))

(define all-cats (get-cats))

; hash table to keep track of category->atomspace
(define sumo-cat-as (make-hash-table))

; populate hash table with cat keys and atomspace vals
(define (popl-h)
    (define (set-h cat) 
        (hash-set! 
            sumo-cat-as cat 
            (cog-new-atomspace)))
    (map set-h all-cats))

; load sumo ontologies into respective atomspaces
(define (load-sumo-data)
    (define (load-into-as cat)
        (cog-set-atomspace! (hash-ref sumo-cat-as cat))
        (load-from-path 
            (string-append 
                "./sumo-data/" cat ".scm")))
    (if (not (eq? (hash-count (const #t) sumo-cat-as) 0))
        (hash-clear! sumo-cat-as))
    (popl-h)
    (map load-into-as all-cats))

(define (load-sumo-data-var)
    (if (not (getenv LOAD-SUMO-ENVVAR))
        (begin
            (newline)
            (display "** Please Set Environment Variable LOAD_SUMO_DATA **")
            (newline)
            (display "** Use Delimiter ':' **")
            (newline)
            (display "Choices are:") (newline)
            (display (get-sumo-files)) (newline)
            (exit)))
    (load-sumo-data (getenv LOAD-SUMO-ENVVAR)))

(define (nod->alist nod)
    (cons (regexp-substitute/global #f "Node" (format #f "~A" (cog-type nod)) 'pre)
          (cog-name nod)))

(define (atom->alist at)
    (define (conv-at at)
        (cond
            ((cog-node-type? (cog-type at))
                (cons (format #f "~A" (cog-type at)) (cog-name at)))
            ((cog-link-type? (cog-type at))
                (cons (format #f "~A" (cog-type at)) (map conv-at (cog-outgoing-set at))))))
    (list (conv-at at)))

(define (atom->json at)
    (scm->json-string (atom->alist at)))

(define (atom->json-pretty at)
    (scm->json-string (atom->alist at) #:pretty #t))

(define (list->subclasses l cls)
    (define (lnk ch)
        (InheritanceLink ch cls))
    (map lnk l))

(define (flatten x)
    (cond ((null? x) '())
          ((not (pair? x)) (list x))
          (else (append (flatten (car x))
                        (flatten (cdr x))))))

(define (unique-flatten x)
    (delete-dup-atoms (flatten x)))

; TODO n-limited recursion for subclass/superclass query
; on proc for subclass 
;   args:
;       fixed: papa
;       optional: depth (default 0 -> until null)
;                 whitelist (default all)
;                 blacklist (default none)
; if none of optargs are set procceed as usual
;    go through all atomspaces for the search
;    stop search on null results
; if depth is provided do counted recursion with depth as limit
;    might need a guard here eg: 0 <= depth <= 100
;    stop recursion if results are null even if depth is not achieved
;    depth=0 means all subclasses of subclasses
;    depth=1 means immediate subclasses
;    depth=2 means include subclasses of immediate subclasses
;    and so on...
; if whitelist is provided, go through the listed atomspace only
; if blacklist is provided, go through all atomspace except for the
;    mentioned ones

(define* (sumo-subclasses 
            papa
            #:key
                (depth 0)
                whitelist
                blacklist)
    (define (recur-subclasses nod)
        (define t
            (cog-outgoing-set 
                (cog-execute! 
                    (GetLink
                        (InheritanceLink
                            (VariableNode "$V") 
                            nod)))))
            (set! depth (- depth 1))
            (cond ((or (<= depth 0) (null? t)) '())
                  ((list? t) (append t (map recur-subclasses t)))))
    
    (define (search-sumo-as cat)
        (begin
            (cog-set-atomspace! (hash-ref sumo-cat-as cat))
            (recur-subclasses papa)))

    (if (not (list? whitelist)) 
        (set! whitelist all-cats))
    (if (list? blacklist) 
        (set! whitelist 
            (lset-difference string-ci=? whitelist blacklist)))

    (map search-sumo-as whitelist))


(define (get-all-subclasses nod cats)
    (define (get-res cat)
        (cog-set-atomspace! (hash-ref sumo-cat-as cat))
        (get-subclasses nod))
    (map get-res (string-split cats #\,)))


(define (get-superclasses child)
    (define t
        (cog-outgoing-set 
            (cog-execute!
                (GetLink
                    (InheritanceLink
                        child
                        (VariableNode "$V"))))))
    (cond ((null? t) '())
          ((list? t) (append t (map get-superclasses t)))))


(define (get-all-superclasses nod cats)
    (define (get-res cat)
        (cog-set-atomspace! (hash-ref sumo-cat-as cat))
        (get-superclasses nod))
    (map get-res (string-split cats #\,)))


(define (related-to cn)
    (delete-dup-atoms
        (cog-chase-link 'InheritanceLink 'ConceptNode cn)))

; TODO /related/x query to look for all relations until no new
;      relationships are found
(define (recurse-relation n cn)
    (define nd (if (not (list? cn)) (list cn) cn))
    (define res (unique-flatten (map related-to nd)))
    (if (eq? n 0) res (append res (recurse-relation (- n 1) res))))


(define (handle-load-ontologies req)
    (begin (load-sumo-data req) ""))

(define (handle-subclass req)
    (define res (get-all-subclasses (ConceptNode req)))
    (if (not (eq? res '()))
        (scm->json-string
            (list (cons "SubClasses"
                (map nod->alist
                    (unique-flatten res))))
            #:pretty #t)
         ""))

(define (handle-superclass req)
    (define res (get-all-superclasses (ConceptNode req)))
    (if (not (eq? res '()))
        (scm->json-string
            (list (cons "SuperClasses"
                    (map nod->alist
                    (unique-flatten res))))
            #:pretty #t)
        ""))

(define (handle-relation depth req)
    (define res (recurse-relation depth (ConceptNode req)))
    (if (not (eq? res '()))
        (scm->json-string
            (list (cons 'depth depth)
                  (cons "related"
                    (map nod->alist
                        (unique-flatten res))))
            #:pretty #t)
        ""))

(define (wrequest-handler req req-body)
    (define cmd
        (split-and-decode-uri-path (uri-path (request-uri req))))
    (define resp
        (if (> (length cmd) 1)
            (cond
                ((string-ci=? (list-ref cmd 0) "loadontologies")
                    (handle-load-ontologies (list-ref cmd 1)))
                ((string-ci=? (list-ref cmd 0) "superclasses")
                    (handle-superclass (list-ref cmd 1)))
                ((string-ci=? (list-ref cmd 0) "subclasses")
                    (handle-subclass (list-ref cmd 1)))
                ((string-ci=? (list-ref cmd 0) "related")
                        (if (or (not (eq? (length cmd) 3)) (not (integer? (string->number (list-ref cmd 1)))))
                            "/related/<integer-depth>/X"
                        (handle-relation (string->number (list-ref cmd 1)) (list-ref cmd 2))))
                (else "Not Found")
            )
            "/superclasses/X , /subclasses/X , /related/<integer-depth>/X")
    )

    (values '((content-type . (text/plain))) resp)
)


;; Load SUMO data from evironment variable LOAD_SUMO_DATA delimited with :
(load-sumo-data-var)

;; Start server at port 9999

(run-server wrequest-handler 'http (list #:addr 0 #:port sumo-port))
;(call-with-new-thread
;   (lambda ()
;       (run-server wrequest-handler 'http (list #:addr 0 #:port 9999))
;   )
;)
