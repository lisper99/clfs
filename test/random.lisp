;; ----------------------------------------------------------------------------
;; Random test elements
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

;; ----------------------------------------------------------------------------
;; Randomness utilities
;; ----------------------------------------------------------------------------

(defun pick-weighted (choices)
  "Randomly chooses a value according to given weights. List choices
  must contain (weight value) pairs with weight a natural
  number (including zero) and value any value."
  (loop
     with num = (random (loop for (weight nil) in choices sum weight))
     for (weight choice) in choices
     sum weight into cumulative
     if (< num cumulative)
     return choice))

(defmacro weighted-choice (&rest choices)
  "Randomly chooses a value according to given weights. Forms choices
  must contain (weight value) pairs with weight a natural
  number (including zero) and value any form that evaluates to a
  value. Only the chosen form gets evaluated. For example
  in (weighted-choice (1 x) (1 y)) x or y gets evaluated.
  In (pick-weighted (list (list 1 x) (list 1 y))) x and y get
  evaluated."
  `(funcall
    (pick-weighted
     (list ,@(loop for (weight . body) in choices
                collect `(list ,weight (lambda () ,@body)))))))

(defun pick (sequence)
  "A uniform random picked element from the given
sequence. It is an error if sequence is empty."
  (if (< 0 (length sequence))
    (elt sequence (random (length sequence)))
    (error "No elements to pick from.")))

;; ----------------------------------------------------------------------------
;; Random actions
;; ----------------------------------------------------------------------------

(defun random-action (directory)
  "Some random action on random paths in directory directory."
  (let ((close-weight (if (if (clfs:execute-p)
                              *opened-test-streams*
                              (clfs-sandbox:streams clfs:*sandbox*))
                          1 0))
        (unsafe-weight 1))
    (weighted-choice

     ;; Common Lisp
     (close-weight (random-close-action directory))
     (0 (random-compile-file-action directory)) ;todo
     (unsafe-weight (random-delete-file-action directory))
     (10 (random-ensure-directories-exist-action directory))
     ((* unsafe-weight 100) (random-rename-file-action directory))

     ;; UIOP
     (0 (random-chdir-action directory)) ;working directory is ignored
     (0 (random-combine-fasls-action directory)) ;todo
     (0 (random-compile-file*-action directory)) ;todo
     (0 (random-concatenate-files-action directory)) ;todo
     ((* 100 unsafe-weight) (random-copy-file-action directory))
     ((* 100 unsafe-weight) (random-delete-directory-tree-action directory))
     (100 (random-delete-empty-directory-action directory))
     (1 (random-delete-file-if-exists-action directory))
     (10 (random-ensure-all-directories-exist-action directory))
     (0 (random-ensure-pathname-action directory))   ;; todo
     (unsafe-weight (random-rename-file-overwriting-target-action directory))
     (0 (random-save-deferred-warnings-action directory))   ;; todo
     
     ;; Extra 
     (10 (random-logged-open-action directory))
     (1 (random-safe-copy-file-action directory))
     (1 (random-safe-delete-directory-tree-action directory))
     (100 (random-safe-delete-file-action directory))
     (10 (random-safe-rename-file-action directory))
     (10 (random-safe-rename-file-overwriting-target-action directory)))))

;; ----------------------------------------------------------------------------
;; Common Lisp random actions
;; ----------------------------------------------------------------------------

(defun random-close-action (directory)
  (declare (ignore directory))
  (list 'clfs:close
        (random-stream)
        :abort (pick '(nil t))))

(defun random-compile-file-action (directory)
  (declare (ignore directory))
  (error "todo: random-compile-file-action"))

(defun random-delete-file-action (directory)
  (list 'clfs:delete-file
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))))

(defun random-ensure-directories-exist-action (directory)
  (list 'clfs:ensure-directories-exist
        (random-path :prefix directory)
        :verbose (pick '(nil t))))

(defun random-open-action (directory)
  (list 'clfs:open
        (weighted-choice
         ;;(0 (random-stream))
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))
        :direction (pick '(:input :output :io :probe))
        :if-exists (pick '(:error ;; :new-version
                           :rename :rename-and-delete :overwrite
                           :append :supersede nil))
        :if-does-not-exist (pick '(:error :create nil))))

(defun random-rename-file-action (directory)
  (list 'clfs:rename-file
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))
        (random-path :prefix directory)))

;; ----------------------------------------------------------------------------
;; UIOP random actions
;; ----------------------------------------------------------------------------

(defun random-chdir-action (directory)
  (list 'clfs:chdir (random-path :prefix directory)))

(defun random-combine-fasls-action (directory)
  (declare (ignore directory))
  (error "todo: random-combine-fasls-action"))

(defun random-compile-file*-action (directory)
  (declare (ignore directory))
  (error "todo: random-compile-file*-action"))

(defun random-concatenate-files-action (directory)
  (declare (ignore directory))
  (error "todo: random-concatenate-files-action"))

(defun random-copy-file-action (directory)
  (list 'clfs:copy-file
        (random-filespec directory)
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))))

(defun random-delete-directory-tree-action (directory)
  (append
   (list 'clfs:delete-directory-tree
         (loop 
            for dir = 
              (weighted-choice
               ;;(0 (random-stream))
               (1 (random-fs-path directory))
               (1 (random-path :prefix directory))
               (1 (random-pathname :prefix directory)))
            while  (equal (pathname directory) (pathname dir))
            finally (return dir)))
   (when (pick '(t nil))
     (list :validate (pick '(test-validator-pos test-validator-neg))))
   (when (pick '(t nil))
     (list :if-does-not-exist (pick '(:ignore :error nil))))))

(defun random-delete-empty-directory-action (directory)
  (list 'clfs:delete-empty-directory
        (loop 
           for dir = 
             (weighted-choice
              ;;(0 (random-stream))
              (1 (random-fs-path directory))
              (1 (random-path :prefix directory))
              (1 (random-pathname :prefix directory)))
           while (equal (pathname directory) (pathname dir))
           finally (return dir))))

(defun random-delete-file-if-exists-action (directory)
  (list 'clfs:delete-file-if-exists
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))))

(defun random-ensure-all-directories-exist-action (directory)
  (list 'clfs:ensure-all-directories-exist
        (list (random-path :prefix directory))))

(defun random-ensure-pathname-action (directory)
  (declare (ignore directory))
  (error "todo: random-ensure-pathname-action"))

(defun random-rename-file-overwriting-target-action (directory)
  (list 'clfs:rename-file-overwriting-target
        (weighted-choice
         (3 (random-fs-path directory))
         (1 (random-path :prefix directory)))
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))))

(defun random-save-deferred-warnings-action (directory)
  (declare (ignore directory))
  (error "todo: random-save-deferred-warnings-action"))

;; ----------------------------------------------------------------------------
;; Extra random actions
;; ----------------------------------------------------------------------------

(defun random-logged-open-action (directory)
  (list 'logged-open
        (weighted-choice
         ;;(0 (random-stream))
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))
        :direction (pick '(:input :output :io :probe))
        :if-exists (pick '(:error ;; :new-version
                           :rename :rename-and-delete :overwrite
                           :append :supersede nil))
        :if-does-not-exist (pick '(:error :create nil))))

(defun random-safe-copy-file-action (directory)
  (list 'clfs:safe-copy-file
        (random-filespec directory)
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))))

(defun random-safe-delete-directory-tree-action (directory)
  (list 'clfs:safe-delete-directory-tree
        (loop 
           for dir = 
           (weighted-choice
            ;;(0 (random-stream))
            (1 (random-fs-path directory))
            (1 (random-path :prefix directory))
            (1 (random-pathname :prefix directory)))
           while  (equal (pathname directory) (pathname dir))
           finally (return dir))
        :validate (pick '(test-validator-pos test-validator-neg))
        :if-does-not-exist (pick '(:ignore :error nil))))

(defun random-safe-delete-file-action (directory)
  (list 'clfs:safe-delete-file
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))))

(defun random-safe-ensure-directories-exist-action (directory)
  (list 'clfs:safe-ensure-directories-exist
        (random-path :prefix directory)
        :verbose (pick '(nil t))))

(defun random-safe-rename-file-action (directory)
  (list 'clfs:safe-rename-file
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))
        (random-path :prefix directory)))

(defun random-safe-rename-file-overwriting-target-action (directory)
  (list 'clfs:safe-rename-file-overwriting-target
        (weighted-choice
         (3 (random-fs-path directory))
         (1 (random-path :prefix directory)))
        (weighted-choice
         (1 (random-fs-path directory))
         (1 (random-path :prefix directory)))))

;; ----------------------------------------------------------------------------
;; Random sandbox
;; ----------------------------------------------------------------------------

(defun random-sandbox (directory)
  (let ((clfs:*sandbox* (clfs-sandbox:make-empty-sandbox
                    (list (clfs-sandbox:path-root directory)))))
    (clfs-sandbox:ensure-directories-exist clfs:*sandbox* directory)
    (let ((clfs:*simulate* t))
      (fill-directory-randomly directory))
    clfs:*sandbox*))

(defun fill-directory-randomly (directory)
  "Fills directory with random content. The files and directories are
created with function random-path."
  (loop
     with at-least = 10
     with at-most = 20
     repeat (+ at-least (random (- at-most at-least -1)))
     for path = (merge-pathnames (random-path :prefix directory) directory)
     do (assert (uiop:absolute-pathname-p path))
     if (uiop:directory-pathname-p path)
     do (when (clfs:ensureable path)
          (clfs:ensure-directories-exist path))
     else
     do (unless (or (clfs:file-exists-p path)
                    (clfs:directory-exists-p path))
          (when (clfs:ensureable (uiop:pathname-directory-pathname path))
            (create-file path)))))

(defun create-file (path)
  (clfs:ensure-directories-exist
   (uiop:pathname-directory-pathname path))
  (close (clfs:open path
               :direction :output
               :if-does-not-exist :create
               ;;:if-exists nil
               )))

;; ----------------------------------------------------------------------------
;; Validators for random-delete-directory-tree-action
;; ----------------------------------------------------------------------------

(defun test-validator-pos (x)
   (declare (ignore x))
   t)

(defun test-validator-neg (x)
  (declare (ignore x))
  nil)

;; ----------------------------------------------------------------------------
;; Random path
;; ----------------------------------------------------------------------------

(defun random-path (&key (prefix ""))
  (weighted-choice
   (1 (random-path-aux :prefix ""))
   (1 (random-path-aux :prefix prefix))
   (1 (pathname (random-path-aux :prefix prefix)))))

(defun random-path-aux (&key (prefix ""))
  "A random string that is syntactically a valid path (if PREFIX is
okay). The string starts with PREFIX and is followed by names
generated by function RANDOM-NAME, separated by slashed."
  (loop 
     with max-path-length = 6
     with suffix-generators = (list (lambda () "")
                                    #'random-name
                                    #'random-file-name
                                    #'random-file-name
                                    #'random-file-name
                                    #'random-file-name)
     repeat (random  max-path-length)
     collect (random-name) into names
     finally (return
               (format nil "~A~{~A/~}~A"
                       prefix
                       names
                       (funcall
                        (pick suffix-generators))))))

(defun random-fs-path (directory)
  ;;(declare (ignore directory))
  ;;(pick (take-snapshot))
  (random-path :prefix directory))

(defun random-file-name ()
  "Some random file name string. A random-name followed by a random
extension."
  (concatenate 'string (random-name) "."
               (pick '("txt" "lisp" "html" "doc" "sys"))))


(defun random-pathname (&key prefix allow-wild)
  "Some random name without a slash or characters invalid in a path."
  (flet ((gen (fun)
           (if (when allow-wild (zerop (random 10)))
               :wild
               (funcall fun))))
    (let ((wild (if allow-wild 1 0)))
      (let ((dir (weighted-choice
                  (wild :wild)
                  (wild :wild-inferior)
                  (9 (append (pathname-directory prefix)
                             (random-directory-list :allow-wild allow-wild))))))
      (weighted-choice
       (1 (make-pathname
           :directory dir
           :name (gen #'random-name)
           :type (gen #'random-type)
           :defaults prefix))
       (1 (make-pathname
           :directory dir
           :defaults prefix)))))))

(defun random-directory-list (&key (allow-wild t) (max-length 7))
  "A random string that is syntactically a valid path (if prefix is
okay). The string starts with prefix and is followed by names
generated by function random-name, separated by slashes."
  (loop
     with wild = (if allow-wild 1 0)
     repeat (random max-length)
     collect
       (if allow-wild
           (weighted-choice
            (wild :wild)
            (wild :wild-inferior)
            (18 (random-name)))
           (random-name))))

;; ----------------------------------------------------------------------------
;; Random stream
;; ----------------------------------------------------------------------------

(defun random-stream ()
  (if (clfs:execute-p)
      (pick *opened-test-streams*)
      (pick (clfs-sandbox:streams clfs:*sandbox*))))

;; ----------------------------------------------------------------------------
;; Other random things
;; ----------------------------------------------------------------------------

(defun random-type ()
  "A random file extension."
  (pick '("txt" "lisp" "html" "doc" "sys")))

(defun random-name ()
  "Some random name without a slash or characters invalid in a path."
  (pick
   '("foo" "bar" "baz"
     "music" "painting" "football" "tennis" "racing" "travel"
     "system" "src" "dev" "null" "nil")))

(defun random-filespec (directory &key stream)
  "FIXME: NO DIRS!!"
  (weighted-choice
   ((if stream 1 0) (random-stream))
   (1 (random-fs-path directory))
   (1 (random-path :prefix directory))))

(defun random-pathspec (directory &key stream)
  (weighted-choice
   ((if stream 1 0) (random-stream))
   (1 (random-fs-path directory))
   (1 (random-path :prefix directory))))
