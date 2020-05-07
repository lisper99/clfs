;; ----------------------------------------------------------------------------
;; Defining actions
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
;; Macro defaction
;; ----------------------------------------------------------------------------

(defmacro defaction (name args &rest definitions)
  "Defines sandboxable function name with arguments args. Options in
definitions can be :body, :pre-condition, :post-condition
and :difference. Only :body is required."
  (flet ((strip-declare (body)
           (if (when (listp body)
                 (let ((first (first body)))
                   (when (listp first)
                     (equal (first first) 'declare))))
               (rest body)
               body)))
    (loop
       for def in definitions
       if (when (listp def)
            (member (first def)
                    '(:body :pre-condition :post-condition :difference)))
       collect def into defs
       else collect def into non-defs
       finally
         (return
           (let* ((body (let ((arg (assoc :body defs)))
                          (if arg
                              (cdr arg)
                              (error ":body missing in action definition"))))
                  (pre-arg (assoc :pre-condition defs))
                  (post-arg (assoc :post-condition defs))
                  (diff-arg (assoc :difference defs))
                  (pre (if pre-arg
                           (cdr pre-arg)
                           (list t)))
                  (post (if post-arg
                            (cdr post-arg)
                            `((lambda (&rest result)
                                (declare (ignore result))
                                t))))
                  (diff (if diff-arg
                            (cdr diff-arg)
                            `((lambda (&rest result)
                                (declare (ignore result))
                                (lambda (added removed)
                                  (declare (ignore added removed))
                                  t))))))
             `(progn
                (defun ,name ,args
                  ,@non-defs
                  (perform-statement
                   ',name
                   (lambda () ,@(strip-declare body))
                   (lambda () ,@(strip-declare pre))
                   (let () ,@(strip-declare post))
                   (let () ,@(strip-declare diff))))
                (setf (get ',name :pre-condition)
                      ,(if pre-arg
                           `(lambda ,args ,@pre)
                           `(lambda (&rest args)
                              (declare (ignore args))
                              ,@pre)))
                (setf (get ',name :post-condition)
                      ,(if post-arg
                           `(lambda ,args ,@post)
                           `(lambda (&rest args)
                              (declare (ignore args))
                              ,@post)))
                (setf (get ',name :difference)
                      ,(if diff-arg
                           `(lambda ,args ,@diff)
                           `(lambda (&rest args)
                              (declare (ignore args))
                              ,@diff)))))))))

;; ----------------------------------------------------------------------------
;; Testing action conditions
;; ----------------------------------------------------------------------------

(defun test-pre-condition (name &rest args)
  "Tests the pre condition of action name for arguments args."
  (unless (get name :pre-condition)
    (error "No pre condition to test for name ~A" name))
  (apply (get name :pre-condition) args))

(defun post-condition-test (name &rest args)
  "Returns a function that tests the post condition of action name for
arguments args. The function expects the results of doing action name
with arguments args as input and gives the post condition value as
output."
  (unless (get name :post-condition)
    (error "No post condition to test for name ~A" name))
  (when (get name :post-condition)
    (apply (get name :post-condition) args)))

(defun difference-test (name &rest args)
  "Returns a function that returns a function that tests the differece
condition of action name for arguments args. The function expects the
results of doing action name with arguments args as input and gives a
function that expects two arguments, the removed files and the added
files. This last function gives the difference condition value as
output."
  (unless (get name :difference)
    (error "No difference condition to test for name ~A" name))
  (when (get name :difference)
    (apply (get name :difference) args)))

;; ----------------------------------------------------------------------------
;; Performing an action
;; ----------------------------------------------------------------------------

(defun perform-statement (name body pre-condition post-condition difference)
  "Performs an action."
  (let ((snapshot (when *test-diff* (take-snapshot))))
    (when *test-pre*
      (unless (funcall pre-condition)
        (cerror "Evaluate body anyway"
                "Pre condition for ~A fails" name)))
    (let ((results (multiple-value-list (funcall body))))
      (when *test-post*
        (unless (apply post-condition results)
          (cerror "Return as if successful"
                  "Post condition for ~A fails for results (~{~A~^ ~})"
                  name
                  results)))
      (when *test-diff*
        (let ((new-snapshot (take-snapshot)))
          (unless
              (multiple-value-bind (same removed added)
                  (clfs-sandbox:sorted-pathname-lists-equal
                   snapshot new-snapshot)
                (declare (ignore same))
                (funcall (apply difference results)
                         removed added))
            (cerror
             "Return as if successful"
             "Difference condition for ~A fails for results (~{~A~^ ~}). Difference: ~A"
             name
             results
             (with-output-to-string (*standard-output*)
               (clfs-sandbox:print-diff snapshot new-snapshot))))))
      (values-list results))))

