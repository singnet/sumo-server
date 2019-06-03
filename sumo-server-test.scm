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
(define error-query-help "/superclasses/X , /subclasses/X , /related/<integer-depth>/X")
(test-equal (get-http-response "http://localhost:7084")
            error-query-help)

;; need more input
(test-equal (get-http-response (string-append "http://localhost:7084/" (random-string 6)))
            error-query-help)

;; not found for non standard queries
(test-equal
    (get-http-response
        (string-append "http://localhost:7084/" (random-string 6) "/" (random-string 6)))
    "Not Found")

;; correct response
(test-assert
    (string-contains
        (get-http-response "http://localhost:7084/subclasses/vegetable")
        "spinach"))

(test-assert
    (string-contains
        (get-http-response "http://localhost:7084/superclasses/vegetable")
        "fruit_or_vegetable"))

(test-equal
    (get-http-response "http://localhost:7084/related/vegetable")
    "/related/<integer-depth>/X")

;; beans shouldn't come as immediate relative to vegetable
(test-assert
    (not
        (string-contains
            (get-http-response "http://localhost:7084/related/0/vegetable")
         "bean")))

;; now beans should be present
(test-assert
    (string-contains
        (get-http-response "http://localhost:7084/related/1/vegetable")
        "bean"))
