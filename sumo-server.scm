
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

(use-modules (ice-9 format)
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

; load sumo data into atomspace
(define (load-sumo-data)
    (define (set-path-ext s)
        (string-append "./sumo-data/" s ".scm"))
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
    (map load-from-path
        (map set-path-ext
            (string-split (getenv LOAD-SUMO-ENVVAR) #\:))))

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


(define (get-all-subclasses papa)
    (define t
        (cog-outgoing-set (cog-execute! (GetLink
                             (InheritanceLink
                                 (VariableNode "$V")
                                 papa)))))
    (cond ((null? t) '())
        ((list? t) (append t (map get-all-subclasses t)))))


(define (get-all-superclasses child)
    (define t
        (cog-outgoing-set (cog-execute! (GetLink
                             (InheritanceLink
                                 child
                                 (VariableNode "$V"))))))
    (cond ((null? t) '())
        ((list? t) (append t (map get-all-superclasses t)))))


(define (related-to cn)
    (delete-dup-atoms
        (cog-chase-link 'InheritanceLink 'ConceptNode cn)))

(define (recurse-relation n cn)
    (define nd (if (not (list? cn)) (list cn) cn))
    (define res (delete-dup-atoms (flatten (map related-to nd))))
    (if (eq? n 0) res (append res (recurse-relation (- n 1) res))))

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
(load-sumo-data)

;; Start server at port 9999

(run-server wrequest-handler 'http (list #:addr 0 #:port sumo-port))
;(call-with-new-thread
;   (lambda ()
;       (run-server wrequest-handler 'http (list #:addr 0 #:port 9999))
;   )
;)
