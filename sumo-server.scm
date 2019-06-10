
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

(define help-txt
"<h2>SUMO Server</h2>
<h3>Requests</h3>
/subclasses/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]<br> 
/superclasses/X/[depth]/d]/[whitelist/h,b,z]/[blacklist/d,e,f]<br>
/related/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]")



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

(define (search-pattern type node)
    (if (string-ci=? type "subclasses")
        (GetLink
            (InheritanceLink
                (VariableNode "$V")
                node))
        (GetLink
            (InheritanceLink
                node
                (VariableNode "$V")))))

                


; TODO n-limited recursion for subclass/superclass query
; on proc for subclass 
;   args:
;       fixed: node
;       optional: search-type (default subclass)
;                 depth (default 0 -> until null)
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

; XXX a little hacky with the recursion depth control
;     should be improved!

(define* (sumo-class-search
            node
            #:key
                (search-type "subclasses")
                (depth -1)
                (whitelist "non")
                (blacklist "non"))
    (define n depth)
    (define (recur-class-search nod)
        (define t
            (cog-outgoing-set 
                (cog-execute! (search-pattern search-type nod))))
            (cond ((or (<= depth -1) (null? t)) '())
                  (else (append
                      (if (not (list? t)) (list t) t) 
                      (begin
                          (set! depth (- depth 1))
                          (map recur-class-search t))))))
    
    (define (search-sumo-as cat)
        (begin
            (cog-set-atomspace! (hash-ref sumo-cat-as cat))
            (set! depth n)
            (recur-class-search node)))

    (if (not (list? whitelist)) 
        (set! whitelist all-cats))
    (if (list? blacklist) 
        (set! whitelist 
            (lset-difference string-ci=? whitelist blacklist)))

    (map search-sumo-as whitelist))


(define (related-to cn)
    (delete-dup-atoms
        (cog-chase-link 'InheritanceLink 'ConceptNode cn)))

(define (reld-to cn)
    (delete-dup-atoms
        (cog-chase-link 'InheritanceLink 'ConceptNode cn)))

; TODO /related/x query to look for all relations until no new
;      relationships are found


(define* (sumo-relation-search node
                               #:key
                                (depth 0)
                                whitelist
                                blacklist)
    (define rec-n depth)
    (define prev-res-len -1)

    (define (recurse-relation n cn)
        (define nd (if (not (list? cn)) (list cn) cn))
        (define result (unique-flatten (map related-to nd)))
        (if (or (<= n 0) (equal? prev-res-len (length result)))
            result 
            (begin
                (set! prev-res-len (length result))
                (append result (recurse-relation (- n 1) result)))))
    
    (define (search-r cat)
        (begin
            (cog-set-atomspace! (hash-ref sumo-cat-as cat))
            (recurse-relation depth (cog-new-node 'ConceptNode (cog-name node)))))
    
    (begin
    (if (not (list? whitelist))
        (set! whitelist all-cats))
    (if (list? blacklist)
        (set! whitelist
            (lset-difference string-ci=? whitelist blacklist))))
    (map search-r whitelist))

    



(define (class-search-resp resp type)
    (if (not (eq? resp '()))
        (scm->json-string
            (list (cons type
                    (map nod->alist resp)))
            #:pretty #t)
        ""))

(define (relation-search-resp resp depth)
    (if (not (eq? resp '()))
        (scm->json-string
            (list (cons 'depth depth)
                  (cons "Related"
                    (map nod->alist resp)))
            #:pretty #t)
        ""))

; requests
; /subclasses/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]
; /superclasses/X/[depth]/d]/[whitelist/h,b,z]/[blacklist/d,e,f]
; /related/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]
;   first comes search_type, then search_term then an optinal depth
;   followed by either of the token blacklist or whitelist each 
;   followd by a comma delimited list of ontology categories
; checks
; each pair of segments must have the format: REQ_TYPE/REQ
; where REQ_TYPE can be one of :
;    subclassses, superclasses, related, depth, whitelist, blacklist

(define (args lst)
    (define (str-eq s)
        (member s (list "subclasses" "superclasses"
                        "related" "depth"
                        "whitelist" "blacklist")))
    (filter str-eq lst))

(define (vals lst)
    (define (str-neq s)
        (member s (list "subclasses" "superclasses"
                        "related" "depth"
                        "whitelist" "blacklist")))
    (filter str-neq lst))

(define (rel? s) (string-ci=? s "related"))

; XXX may not be necessary having this function
; match:start of ice-9 throws an error when constant
; in regex doesn't match because then arg:match is #f
; and not a vector
; it's redefined here with a check for arg:match
(define* (match:substring match #:optional (n 0))
  (if match
      (let* ((start (match:start match n))
             (end   (match:end match n)))
            (and start end (substring (match:string match) start end)))
      ""))

(define search-type-regex
    (make-regexp "((related)|(superclasses)|(subclasses))/[^/]*" regexp/icase))
(define dep-regex (make-regexp "depth/[0-9]+" regexp/icase))
(define white-regex (make-regexp "whitelist/[^/]*" regexp/icase))
(define black-regex (make-regexp "blacklist/[^/]*" regexp/icase))

(define (parse-req-act req_cmd)
    (define search-term 
        (string-split
            (match:substring
                (regexp-exec search-type-regex req_cmd))
        #\/))
    (cond ((not (= (length search-term) 2))
            (string-append "<h3>requests</h3><br>"
            "/subclasses/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]<br>"
            "/superclasses/X/[depth]/d]/[whitelist/h,b,z]/[blacklist/d,e,f]<br>"
            "/related/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]"))
          ((string-ci=? (car search-term) "related")
            (let* ((depth-p
                    (string-split
                        (match:substring (regexp-exec dep-regex req_cmd))
                    #\/))
                 (depth-i (if (equal? (car depth-p) "") 0 (string->number (cadr depth-p))))
                 (white-p
                    (string-split
                        (match:substring (regexp-exec white-regex req_cmd))
                    #\/))
                 (black-p
                    (string-split
                        (match:substring (regexp-exec black-regex req_cmd))
                    #\/)))
            (relation-search-resp
                (unique-flatten
                    (sumo-relation-search (ConceptNode (cadr search-term))
                        #:depth depth-i
                        #:whitelist (if (equal? (car white-p) "")
                                        all-cats (string-split
                                        (cadr white-p) #\,))
                        #:blacklist (if (equal? (car black-p) "") 
                                        ""
                                        (string-split (cadr black-p) #\,))))
                    depth-i)))
          (else
            (let ((depth-p
                    (string-split
                        (match:substring (regexp-exec dep-regex req_cmd))
                    #\/))
                 (white-p
                    (string-split
                        (match:substring (regexp-exec white-regex req_cmd))
                    #\/))
                 (black-p
                    (string-split
                        (match:substring (regexp-exec black-regex req_cmd))
                    #\/)))
            (class-search-resp
                (unique-flatten 
                    (sumo-class-search (ConceptNode (cadr search-term))
                        #:search-type (car search-term)
                        #:depth (if (equal? (car depth-p) "")
                                    -1 
                                    (string->number (cadr depth-p)))
                        #:whitelist (if (equal? (car white-p) "")
                                        all-cats (string-split
                                        (cadr white-p) #\,))
                        #:blacklist (if (equal? (car black-p) "") 
                                        ""
                                        (string-split (cadr black-p) #\,))))
                    (car search-term))))))
                

(define (wrequest-handler req req-body)
    (define cmd
        (split-and-decode-uri-path (uri-path (request-uri req))))
    (define resp
        (if (> (length cmd) 1)
            (parse-req-act (uri-path (request-uri req)))
            help-txt)
    )
    (values '((content-type . (text/plain))) resp)
)


;; Load SUMO data from evironment variable LOAD_SUMO_DATA delimited with :
(load-sumo-data)

;; Start server at port 9999

(run-server wrequest-handler 'http (list #:addr 0 #:port sumo-port))
;(call-with-new-thread
;   (lambda ()
;       (run-server wrequest-handler 'http (list #:addr 0 #:port 9999))
;   )
;)
