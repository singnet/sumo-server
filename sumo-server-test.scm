(use-modules (opencog)
             (opencog exec)
             (opencog query))

(use-modules (web client))

(use-modules (srfi srfi-64))

(define (get-http-response req)
    (call-with-values
        (lambda () (http-get req))
        (lambda (r-head r-body) r-body)))


; start test

(test-begin "sumo-server-test")

;; is server up?
(define error-query-help
"SUMO Server
Requests
/subclasses/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]
/superclasses/X/[depth]/d]/[whitelist/h,b,z]/[blacklist/d,e,f]
/related/X/[depth/d]/[blacklist/a,b,z]/[whitelist/d,e,f]")

(test-equal (get-http-response "http://localhost:9999")
            error-query-help)

;; need more input
(test-equal (get-http-response (string-append "http://localhost:9999/" (random-string 6)))
            error-query-help)

;; not found for non standard queries
(test-equal
    (get-http-response
        (string-append "http://localhost:9999/" (random-string 6) "/" (random-string 6)))
    error-query-help)

;; correct response - from all categories
(test-assert
    (string-contains
        (get-http-response "http://localhost:9999/subclasses/vegetable")
        "zucchini"))

;; correct response - from Food KB
(test-assert
    (string-contains
        (get-http-response "http://localhost:9999/subclasses/vegetable/whitelist/Food")
        "zucchini"))

;; correct response - zucchini shouldn't be in the result where Food KB is blacklisted
(test-assert
    (not
        (string-contains
            (get-http-response "http://localhost:9999/subclasses/vegetable/blacklist/Food")
         "zucchini")))

;; correct response - no depth
(test-assert
    (string-contains
        (get-http-response "http://localhost:9999/subclasses/vegetable")
        "legume"))

;; correct response - 0 depth
(test-assert
    (string-contains
        (get-http-response "http://localhost:9999/subclasses/vegetable/depth/0")
        "legume"))

;; correct response - 1 level depth
(test-assert
    (string-contains
        (get-http-response "http://localhost:9999/subclasses/vegetable/depth/1")
        "bean"))

;; correct response - superclass
(test-assert
    (string-contains
        (get-http-response "http://localhost:9999/superclasses/vegetable")
        "fruit_or_vegetable"))

;; correct response relation
;; beans shouldn't come as immediate relative to vegetable but legume should
(test-assert
    (string-contains
        (get-http-response "http://localhost:9999/related/vegetable")
        "legume"))

(test-assert
    (not
        (string-contains
            (get-http-response "http://localhost:9999/related/vegetable/depth/0")
         "bean")))

;; now beans should be present
(test-assert
    (string-contains
        (get-http-response "http://localhost:9999/related/vegetable/depth/1")
        "bean"))
