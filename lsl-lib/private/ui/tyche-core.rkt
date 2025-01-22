#lang racket

(provide process-prog)

;; process-prog : Path -> OpenPBTStats
;; Produces an OpenPBTStats JSON representing the results of
;; running the PBT in the given LSL file.
(define (process-prog f)
  ;; TODO: Get help from Cameron with this part.
  (list #hash((type . "test_case")
              (run_start . 1)
              (property . "example_property")
              (status . "passed")
              (status_reason . "example_reason")
              (representation . "2")
              (features . #hash())
              (coverage . "no_coverage_info")
              (metadata . #hash()))
        #hash((type . "test_case")
              (run_start . 1)
              (property . "example_property")
              (status . "failed")
              (status_reason . "example_reason")
              (representation . "1")
              (features . #hash())
              (coverage . "no_coverage_info")
              (metadata . #hash()))))
