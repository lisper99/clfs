;; ----------------------------------------------------------------------------
;; Package definition for the clfs-test package
;; 
;; Paul Griffioen 2020-2020
;; ----------------------------------------------------------------------------

(defpackage :clfs-test
  (:documentation "Tests for The clfs package.")
  (:use :common-lisp)
  (:import-from :clfs "DEFACTION")
  (:export "TEST-CLFS"
           "TEST-CONTRACTS"))
