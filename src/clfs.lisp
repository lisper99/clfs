;; ----------------------------------------------------------------------------
;; Exported functions and macros besides actions and observers
;;
;; See the prelude for the special variables.
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

(in-package :clfs)

;; ----------------------------------------------------------------------------
;; Performing a plan
;; ----------------------------------------------------------------------------

(defun execute-plan (fun &key
                           (test-pre *test-pre*)
                           (test-post *test-post*)
                           (test-diff *test-diff*)
                           (confine *confine*))
  "Sets *simulate* to nil, sets the passed special variables and calls
function fun without any arguments."
  (let ((*simulate* nil)
        (*test-pre* test-pre)
        (*test-post* test-post)
        (*test-diff* test-diff)
        (*confine* confine))
    (funcall fun)))

(defun simulate-plan (fun &key
                      (test-pre *test-pre*)
                      (test-post *test-post*)
                      (test-diff *test-diff*)
                      (confine *confine*))
  "Sets *simulate* to t, sets the passed special variables and calls
function fun without any arguments."
  (let ((*simulate* t)
        (*test-pre* test-pre)
        (*test-post* test-post)
        (*test-diff* test-diff)
        (*confine* confine))
    (funcall fun)))

;; ----------------------------------------------------------------------------
;; Macros with-access and with-sandbox
;; ----------------------------------------------------------------------------

(defmacro with-access (directory &body body)
  "Gives access to the directory or directories. Runs body with
directory added to *whitelist*. Argument directory can also be a list
in which case it is appended."
  (let ((var (gensym)))
    `(let ((,var ,directory))
       (let ((*whitelist* (if (listp ,var)
                              (append ,var *whitelist*)
                              (cons ,var *whitelist*))))
         ,@body))))

(defmacro with-sandbox ((directory &key
                                   (kind :both)
                                   (test-pre '*test-pre*)
                                   (test-post '*test-post*)
                                   (test-diff '*test-diff*)
                                   (close-streams t)
                                   (confine '*confine*)
                                   (verbose t))
                        &body body)
  "Simulates and/or executes body with in a sandbox for
directory. keyword kind must be one of :simulate :execute or :both."
  (let ((dir (gensym "DIR"))
        (dirs (gensym "DIRS"))
        (stream (gensym "STREAM")))
    `(let ((,dir ,directory))
       (let ((,dirs (if (listp ,dir) ,dir (list ,dir))))
         (let ((*test-pre* ,test-pre)
               (*test-post* ,test-post)
               (*test-diff* ,test-diff)
               (*confine* ,confine)
               (*whitelist* (append ,dirs *whitelist*))
               (*open-streams* ()))
           (unwind-protect
                (run-sandbox-body
                 ,dirs ,kind (lambda () ,@body) :verbose ,verbose)
             (when ,close-streams
               (loop for ,stream in *open-streams*
                  do (ignore-errors (cl:close ,stream))))))))))

(defun run-sandbox-body (dirs kind body &key verbose)
  "Helper for with-sandbox."
  (ecase kind
    (:simulate
     (when verbose
       (format t "~2%* Running simulation...~%"))
     (let ((*sandbox* (clfs-sandbox:make-populated-sandbox dirs)))
       (simulate-plan body))
     (when verbose
       (format t "~2%Simulation successful.~%")))
    (:execute
     (execute-plan body))
    (:both
     (when verbose
       (format t "~2%* Running simulation...~%"))
     (let ((*sandbox* (clfs-sandbox:make-populated-sandbox dirs)))
       (simulate-plan body))
     (when verbose
       (format t "~2%* Simulation successful, executing for real...~%"))
     (execute-plan body))))

;; ----------------------------------------------------------------------------
;; Taking a snapshot
;; ----------------------------------------------------------------------------

(defun take-snapshot ()
  "Returns a snapshot of the current sandbox's file system. The result
is a sorted list of pathnames. Contains the paths in all whitelisted
directory. Therefore it is a subset of all-paths. See
sorted-pathname-lists-equal."
  (loop
     with paths = ()
     for directory in *whitelist*
     when (directory-exists-p directory)
     do (collect-sub*directories 
         directory 
         (lambda (x) (declare (ignore x)) t)
         (lambda (x) (declare (ignore x)) t)
         (lambda (x) (push (pathname x) paths)
                 (loop for y in (directory-files x)
                    ;; Quick fix for hidden files. Should be handled
                    ;; in the pre and post conditions of relevant
                    ;; functions
                    unless
                      #+ccl (uiop:string-prefix-p "fuse_hidden" 
                                                  (pathname-type y))
                      #-ccl (uiop:string-prefix-p ".fuse_hidden"
                                                  (pathname-name y))
                    do (push (pathname y) paths))))
     finally
     (setf paths (sort paths #'string< :key #'namestring))
     (return paths)))
