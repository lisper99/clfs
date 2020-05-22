;; ----------------------------------------------------------------------------
;; System definition for the clfs package
;; 
;; Copyright (c) 2020 Paul Griffioen
;; 
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;; ----------------------------------------------------------------------------

(asdf:defsystem "clfs/sandbox"
  :version "0"
  :serial t
  :depends-on (:uiop
               :trivial-gray-streams)
  :components
  ((:file "src/sandbox/package")
   (:file "src/sandbox/file-data")
   (:file "src/sandbox/directory-data")
   (:file "src/sandbox/fs-node")
   (:file "src/sandbox/stream")
   (:file "src/sandbox/sandbox")
   (:file "src/sandbox/shadowed-actions")
   (:file "src/sandbox/shadowed-observers")))

(asdf:defsystem "clfs"
  :version "0"
  :serial t
  :depends-on (:uiop
               #-clfs-no-extras :trivial-download
               #-clfs-no-extras :zip
               :clfs/sandbox)
  :in-order-to ((test-op (test-op "clfs-test")))
  :components
  ((:file "src/package")
   (:file "src/prelude")
   (:file "src/contract")
   (:file "src/shadowed-actions")
   (:file "src/shadowed-observers")
   #-clfs-no-extras (:file "src/extras")
   (:file "src/clfs")))

