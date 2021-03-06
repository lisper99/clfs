;; ----------------------------------------------------------------------------
;; Automated tests
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

(in-package :clfs-test)

;; -----------------------------------------------------------------------------
;; Action logged-open
;; -----------------------------------------------------------------------------

(defvar *opened-test-streams* ()
  "All opened streams. Used for picking random streams and used during
  dumping scenarios.")

(defaction logged-open
    (filename 
     &key
     (direction :input)
     (element-type 'base-char)
     (if-exists (if (eq (pathname-version filename) :newest)
                    :new-version
                    :error))
     (if-does-not-exist (cond ((eq direction :probe)
                               nil)
                              ((or (eq direction :input)
                                   (eq if-exists :overwrite)
                                   (eq if-exists :append))
                               :error)
                              (t :create)))
     (external-format :default))
  "Alternative to function open that stores the opened stream in
global *opened-test-streams*."
   
  (:pre-condition
   (clfs:test-pre-condition
    'clfs:open filename
    :direction direction
    :element-type element-type
    :if-exists if-exists
    :if-does-not-exist if-does-not-exist
    :external-format external-format))
  
  (:body
   (let ((stream (clfs:open filename
                            :direction direction
                            :element-type element-type
                            :if-exists if-exists
                            :if-does-not-exist if-does-not-exist
                            :external-format external-format)))
     (when (and (clfs:execute-p) stream)
       (setf *opened-test-streams* (append *opened-test-streams*
                                            (list stream))))
     stream))
  
  (:post-condition
   (clfs:post-condition-test
    'clfs:open filename
    :direction direction
    :element-type element-type
    :if-exists if-exists
    :if-does-not-exist if-does-not-exist
    :external-format external-format))
  
  (:difference
   (clfs:difference-test
    'clfs:open filename
    :direction direction
    :element-type element-type
    :if-exists if-exists
    :if-does-not-exist if-does-not-exist
    :external-format external-format)))
