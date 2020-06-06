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

;; ----------------------------------------------------------------------------
;; Globals
;; ----------------------------------------------------------------------------

(defvar *chosen-restart* nil
  "Restart chosen by the user when an error is caught by the debugger
  during testing. See function test-actions.")

(defvar *action-log* nil
  "A list of actions. Used in testing to store the current scenario.")

;; ----------------------------------------------------------------------------
;; Testing clfs
;; ----------------------------------------------------------------------------

(defun test-clfs (directory)
  "Runs the tests for the clfs package itself. Use function
test-contracts to test the file system actions."
  (let ((*default-pathname-defaults* (pathname directory)))
    (when t
      (test-copy-file-system directory)
      (test-mirror-on-disk directory))
    (when nil
      (test-file-system-observers directory :mode :simulate))
    (when t
      (test-file-system-actions directory :mode :simulate :target 100))))

;; ----------------------------------------------------------------------------
;; Testing contracts
;; ----------------------------------------------------------------------------

(defun test-contracts (directory)
  "Tests all file system actions."
  (let ((*default-pathname-defaults* (pathname directory)))
    (test-file-system-actions directory :mode :execute :target 1000)))

(defun run-scenario (file)
  "Run a dumped scenario without leaving trash in globals."
  (let ((clfs:*whitelist* ())
        (*action-log* ())
        (*opened-test-streams* ()))
    (unwind-protect (load file)
      (reset-test-streams))))

;; ----------------------------------------------------------------------------
;; test-copy-file-system
;; ----------------------------------------------------------------------------

(defun test-copy-file-system (directory)
  "Copies a random file system and checks that the result is equal."
  (format t "~2%* Testing in-memory copy file system action with ~A" directory)
  (let ((clfs:*whitelist* (list directory)))
    (let* ((fs (random-sandbox directory))
           (copy (clfs-sandbox:copy-sandbox fs)))
      (multiple-value-bind (okay)
          (clfs-sandbox:file-system-equal fs copy)
        (if okay
            (format t "~2%Test result okay")
            (cerror "Continue with tests"
                    "Test copy file system fails"))))))

;; ----------------------------------------------------------------------------
;; test-mirror-on-disk
;; ----------------------------------------------------------------------------

(defun test-mirror-on-disk (directory)
  "Creates a mirror directory in directory, writes a random file
system in it, reloads it, and checks that the result is equal to the
original."
  (let ((dir (fresh-test-dir "mirror-test/mirror" :directory directory)))
    (let ((clfs:*whitelist* (list dir)))
      (format t "~2%* Mirroring into ~A" dir)
      (ensure-directories-exist dir)
      (let ((random-fs (random-sandbox dir)))
        (clfs-sandbox:mirror-file-system-to-disk random-fs)
        (format t "~2%Mirror written on disk, reloading...")
        (let ((reloaded (clfs-sandbox:make-populated-sandbox (list dir))))
          (format t "~2%Mirror reloaded from disk")
          (multiple-value-bind (okay)
              (clfs-sandbox:file-system-equal random-fs reloaded)
            (if okay
                (format t "~2%Test result okay")
                (cerror "Continue with tests"
                        "Test copy file system fails"))))))))

;; ----------------------------------------------------------------------------
;; Test observers
;; ----------------------------------------------------------------------------

(defun test-file-system-observers (directory &key mode)
  "Runs random tests on Common Lisp and uiop file system functions."
  (when t
    (let* ((dir (fresh-test-dir "action-test/random" :directory directory))
           (clfs:*whitelist* (list dir))
           (clfs:*sandbox*
            (clfs-sandbox:make-empty-sandbox (list (clfs-sandbox:path-root directory))))
           (clfs:*simulate* (ecase mode
                         (:simulate t)
                         (:execute nil))))
    (fill-directory-randomly dir)
    (test-cl-wild-pathname-p dir)
    ;;(test-cl-directory dir)
    (test-uiop-directory-files dir)
    (test-uiop-subdirectories dir))))

(defun test-cl-wild-pathname-p (directory)
  (test-function
   :function 'wild-pathname-p
   :generator
   (lambda ()
     (list (random-pathname :prefix directory)
           (pick
            '(nil :host :device :directory :name :type :version))))
   :condition
   (lambda (pathname &optional field-key)
     (declare (ignore pathname))
     (member field-key
             '(nil :host :device :directory :name :type :version)))
   :oracle
   (lambda (pathname &optional field-key)
     (lambda (result)
       (clfs::equiv result
              (case field-key
                (:host (eql (pathname-host pathname) :wild))
                (:directory (eql (pathname-name pathname) :wild))
                (:name (eql (pathname-name pathname) :wild))
                (:type (eql (pathname-type pathname) :wild))
                ((nil) (or (member :wild (pathname-directory pathname))
                           (eql (pathname-name pathname) :wild)
                           (eql (pathname-type pathname) :wild)
                           (eql (pathname-version pathname) :wild)))))))))

(defun test-cl-directory (directory)
  "Random tests for Common Lisp function directory."
  (test-function
   :title "directory for random directories"
   :function
   (lambda (pathname)
     (directory
      (merge-pathnames uiop:*wild-directory*
                       (uiop:pathname-parent-directory-pathname pathname))))
   :generator
   (lambda ()
     (list (random-pathspec directory)))
   :condition
   (lambda (pathname)
     (and
      (uiop:directory-pathname-p pathname)
      (when (uiop:pathname-parent-directory-pathname pathname)
        (clfs:directory-exists-p
         (uiop:pathname-parent-directory-pathname pathname)))))
   :oracle
   (lambda (pathname)
     (lambda (result)
       (and
        (loop
           with parent = (uiop:pathname-parent-directory-pathname pathname)
           for path in result
           always (equal (uiop:pathname-parent-directory-pathname path)
                         parent))
        (clfs::equiv (member (pathname pathname) result :test #'equal)
               (clfs:directory-exists-p pathname))))))
  (test-function
   :title "DIRECTORY for random files"
   :function
   (lambda (pathname)
     (directory
      (merge-pathnames uiop:*wild-file*
                       (uiop:pathname-directory-pathname pathname))))
   :generator
   (lambda ()
     (list (random-pathspec directory)))
   :condition
   (lambda (pathname)
     (and
      (uiop:file-pathname-p pathname)
      (clfs:directory-exists-p
       (uiop:pathname-directory-pathname pathname))))
   :oracle
   (lambda (pathname)
     (lambda (result)
       (and
        (loop
           with parent = (uiop:pathname-directory-pathname pathname)
           for path in result
           always (equal (uiop:pathname-directory-pathname path)
                         parent))
        (clfs::equiv (member (pathname pathname) result :test #'equal)
               (clfs:file-exists-p pathname)))))))

(defun test-uiop-subdirectories (directory)
  "Random tests for uiop:subdirectories."
  (test-function
   :title "uiop:subdirectories"
   :function
   (lambda (pathname)
     (clfs:subdirectories (uiop:pathname-parent-directory-pathname pathname)))
   :generator
   (lambda ()
     (list (random-pathspec directory)))
   :condition
   (lambda (pathname)
     (and
      (uiop:directory-pathname-p pathname)
      (when (uiop:pathname-parent-directory-pathname pathname)
        (clfs:directory-exists-p
         (uiop:pathname-parent-directory-pathname pathname)))))
   :oracle
   (lambda (pathname)
     (lambda (result)
       (and
        (loop
           with parent = (uiop:pathname-parent-directory-pathname pathname)
           for path in result
           always (equal (uiop:pathname-parent-directory-pathname path)
                         parent))
        (clfs::equiv (member (pathname pathname) result :test #'equal)
               (clfs:directory-exists-p pathname)))))))

(defun test-uiop-directory-files (directory)
  "Random tests for uiop:directory-files."
  (test-function
   :title "uiop:directory-files"
   :function
   (lambda (pathname)
     (clfs:directory-files (uiop:pathname-directory-pathname pathname)))
   :generator
   (lambda ()
     (list (random-pathspec directory)))
   :condition
   (lambda (pathname)
     (and
      (uiop:file-pathname-p pathname)
      (clfs:directory-exists-p
       (uiop:pathname-directory-pathname pathname))))
   :oracle
   (lambda (pathname)
     (lambda (result)
       (and
        (loop
           with parent = (uiop:pathname-directory-pathname pathname)
           for path in result
           always (equal (uiop:pathname-directory-pathname path)
                         parent))
        (clfs::equiv (member (pathname pathname) result :test #'equal)
               (clfs:file-exists-p pathname)))))))

(defun test-make-pathname ()
  (test-function
   :function 'parse-namestring
   :generator
   (lambda ()
     (list (namestring (random-pathname))))
   :condition
   (lambda (thing &optional host (default-pathname *default-pathname-defaults*)
            &key (start 0) end junk-allowed)
     (declare (ignore thing host default-pathname start end junk-allowed))
     t)
   :oracle
   (lambda (thing &optional host (default-pathname *default-pathname-defaults*)
            &key (start 0) end junk-allowed)
     (declare (ignore host default-pathname start end junk-allowed))
     (lambda (pathname position)
       (declare (ignore position))
       (string-equal
        (namestring pathname)
        thing)))))

;; ----------------------------------------------------------------------------
;;  test-file-system-actions
;; ----------------------------------------------------------------------------

(defun test-file-system-actions (directory &key
                                 (mode :simulate)
                                 (verbosity 1)
                                 (max-attempts 100000)
                                             (target 100))

  (let* ((dir (fresh-test-dir "action-test/random" :directory directory))
         (scenario-dir (merge-pathnames "scenario/" directory))
         (*default-pathname-defaults* (pathname dir)))
    (clfs:with-sandbox (dir
                   :kind mode
                   :test-pre nil
                   :test-post nil
                   :test-diff nil)
      (format t "~%Creating test directory ~A~%" dir)
      (ensure-directories-exist dir)
      (format t "~%Ensuring real scenario directory ~A~%" scenario-dir)
      (clfs:with-sandbox (scenario-dir
                     :kind :execute)
        (ensure-directories-exist scenario-dir))
      (format t "~%Current directory is ~A~%" (clfs:getcwd))
      (fill-directory-randomly dir)
      (let ((*action-log* (snapshot-as-action-log (clfs::take-snapshot))))
        (test-actions dir scenario-dir
                      :target target
                      :verbosity verbosity
                      :max-attempts max-attempts
                      :generator 'random-action
                      :test-post t
                      :test-diff t))))
  (values))

(defun test-actions (directory scenario-directory &key
                     (verbosity 1)
                     (max-attempts 100000)
                     (target 100)
                     (generator 'random-action)
                     (test-post t)
                     (test-diff t))
  "Automated test using pre and post conditions with random actions on
a random file system."
  (flet ((log-lvl (level text &rest args)
           (when (<= verbosity level)
             (apply #'format t text args))))
    (format t "~%* Automated action tests~%")
    (format t "~%Settings:")
    (format t "~%  Test directory:       ~A" directory)
    (format t "~%  Scenario directory:   ~A" scenario-directory)
    (format t "~%  Mode:                 ~S" (if clfs:*simulate*
                                                 :simulate
                                                 :execute))
    (format t "~%  Verbosity:            ~S" verbosity)
    (format t "~%  Target:               ~S" target)
    (format t "~%  Max-attempts:         ~S" max-attempts)
    (format t "~%  Generator:            ~S" 'random-action)
    (format t "~%  Test post:            ~S" t)
    (format t "~%  Test diff:            ~S~%" t)
    (unwind-protect
         (loop
            with fs-copy = (clfs::take-snapshot)
            with success-table = (make-hash-table :test #'equal)
            with failure-table = (make-hash-table :test #'equal)
            with standard-output = *standard-output*
            with success-count = 0
            initially
              (setf *opened-test-streams* ())
              (setf *chosen-restart* nil)
            (when (<= 2 verbosity)
              (format t "~%Initial file system:~2%")
              (clfs-sandbox:print-file-system clfs:*sandbox*)
              (terpri))
            (format t "~%* Running tests...~2%")
            repeat max-attempts
            for (name . args) = (funcall generator directory)
            for pre = (apply #'clfs:test-pre-condition name args)
            for unsuccessful = (if pre 0 1) then (if pre 0 (+ unsuccessful 1))
            for tests = 0 then (if pre (+ tests 1) tests)
            if pre
            do (restart-case
                   (let ((snapshot (clfs::take-snapshot)))
                     (handler-bind 
                       ((condition
                         (lambda (x)
                           (setf (gethash name failure-table)
                                 (+ (gethash name failure-table 0) 1))
                           (let ((*standard-output* standard-output))
                             (format t "~2%Automatic test interputed:~%")
                             (format t "~%~A" x)
                             (format t "~2%during test:~%")
                             (pprint (cons name args))
                             (terpri))
                           (dump-error-scenario
                            x
                            directory
                            scenario-directory
                            (cons name args)
                            snapshot)
                           (if *chosen-restart*
                               (invoke-restart
                                (find-restart *chosen-restart* x))
                               (error x)))))
                     (let ((results (multiple-value-list
                                     (let ((clfs:*test-pre* nil)
                                           (clfs:*test-post* test-post)
                                           (clfs:*test-diff* test-diff))
                                       (when (<= 2 verbosity)
                                         (terpri)
                                         (format t "~%* Running test ~A:" name)
                                         (terpri))
                                       (push (cons name args) *action-log*)
                                       (apply name args)))))
                       (incf success-count)
                       (setf (gethash name success-table)
                             (+ (gethash name success-table 0) 1))
                       (when (<= 2 verbosity)
                         (format t "~%* Test ~A finished" name )
                         (format t "~%result: ~{~A~^,~%~}~%" results))
                       (when (= 1 verbosity)
                         (when (zerop (mod success-count (ceiling target 80)))
                           (format t "~%~A/~A" success-count target))))))
                 (keep-testing ()
                   :report (lambda (stream)
                             (format stream "Keep testing")))
                 (keep-testing-silently ()
                   :report (lambda (stream)
                             (format stream "Keep testing and don't ask again"))
                   (setf *chosen-restart* 'keep-testing))
                 (dump-and-keep-testing ()
                   :report (lambda (stream)
                             (format stream "Print current state and keep testing"))
                   (format t "~%* Dumping file system...~%")
                   (clfs-sandbox:print-file-system clfs:*sandbox*)
                   (format t "~%* File system dumped, resuming tests...~%"))
                 (dump-and-keep-testing-silently ()
                   :report (lambda (stream)
                             (format stream "Print current state, keep testing and don't ask again"))
                   (setf *chosen-restart* 'dump-and-keep-testing)
                   (format t "~%* Dumping file system~%")
                   (clfs-sandbox:print-file-system clfs:*sandbox*)
                   (format t "~%* File system dumped, resuming tests...~%")))
            until (= tests target)
            finally
            (terpri)
            (when (<= 2 verbosity)
              (format t "~%* Tests done. File system changes:")
              (terpri)
              (clfs-sandbox:print-diff fs-copy (clfs::take-snapshot))
              (terpri))
            (format t "~%* Automated action tests ready~%")
            (format t "~%Success count for tested functions:")
            (loop
               for name being the hash-key in success-table
               using (hash-value count)
               for i = 1 then (1+ i)
               do (format t "~%~2,' D. ~A: ~50T~5,' D" i name count))
            (format t "~&~%Failure count for tested functions:")
            (loop
               for name being the hash-key in failure-table
               using (hash-value count)
               for i = 1 then (1+ i)
               do (format t "~%~2,' D. ~A: ~50T~5,' D" i name count)))
      (log-lvl 1 "~2%* Closing opened test streams...")
      (reset-test-streams)
      (log-lvl 1 "done~%")))
  (values))

;; ----------------------------------------------------------------------------
;; Test utilities
;; ----------------------------------------------------------------------------

(defun test-function (&key function generator condition oracle
                      (verbose t)
                      (max-attempts 100000)
                      (target 1000)
                      title)
  ""
  (let ((successes 0))
    (loop
       initially (when verbose
                   (format t "~%* Testing function ~A~%" (or title function)))
       repeat max-attempts
       for arguments = (funcall generator)
       for pre = (apply condition arguments)
       for tests = (if pre 1 0) then (if pre (+ tests 1) tests)
       if pre
       do (let ((results (multiple-value-list (apply function arguments))))
            (if (apply (apply oracle arguments) results)
              (progn
                (incf successes)
                (when verbose
                  (when (zerop (mod successes (ceiling target 80)))
                    (princ "."))))
              (error
               "Function ~2%  ~A~%failed on arguments~2%  ~A and results ~2%~A"
               function arguments results)))
       until (= tests target)
       finally
         (when verbose
           (format t "~%Test ready. Success is ~A/~A" successes target)
           (terpri)))
    successes))

(defun fresh-test-dir (name &key directory)
  (loop
     for tries = 0 then (+ tries 1)
     for dir = (format nil "~A~A~2,'0D/" directory name tries)
     while (clfs:directory-exists-p dir)
     if (< 10 tries)
     do (cerror "Try next"
                "More than 10 (currently trying ~A) ~A test directories" 
                tries name)
     finally
       (return dir)))

(defun fresh-filename (directory name)
  (loop
     for tries = 0 then (+ tries 1)
     for filename = (format nil "~A-~4,'0D.lisp" name tries)
     for path = (merge-pathnames filename directory)
     while (clfs:file-exists-p path)
     finally (return path)))

(defun reset-test-streams ()
  (loop
     for stream in *opened-test-streams*
     do (ignore-errors (close stream))
     finally (setf *opened-test-streams* nil)))

(defun snapshot-as-action-log (snapshot)
  "A list of list expressions that recreates the directories and files
in snapshot."
  (loop
     for path in (reverse snapshot)
     collect (list (if (uiop:directory-pathname-p path)
                       'ensure-directories-exist
                       'create-file)
                   path)))

(defun dump-error-scenario (condition test-dir dump-dir action snapshot)
  "Helper for function test-actions."
  (let ((simulate-p clfs:*simulate*)
        (cwd (clfs:getcwd))
        (clfs:*simulate* nil)
        (whitelist clfs:*whitelist*)
        (actions (reverse *action-log*)))
    (clfs:with-access dump-dir
      (let ((scenario-file (fresh-filename dump-dir "scenario")))
        (format t "~%Writing actions to scenario file ~A~%"
                scenario-file)
        (with-open-file (s scenario-file
                           :direction :output
                           :if-does-not-exist :create)
          (format s "(in-package :cl-user)")
          (format s "~%")
          (format s "~%; Condition '~A' thrown during action:~%"
                  (substitute #\- #\newline (princ-to-string condition)))
          (format s "~%;  (~{~S~^ ~})" action)
          (format s "~%")
          (format s "~%; Info:")
          (format s "~%;   Test directory = ~A" test-dir)
          (format s "~%;   Working directory = ~A" cwd)
          (format s "~%;   Var *whitelist* = ~A" whitelist)
          (format s "~%;   Switch *simulate* = ~A" simulate-p)
          (format s "~%;   Lisp type = ~A" (lisp-implementation-type))
          (format s "~%;   Lisp version = ~A" (lisp-implementation-version))
          (format s "~%")
          (format s "~%(setf clfs:*whitelist* '(~S))" test-dir)
          (format s "~%")
          (format s "~%; Streams:")
          (loop
             for stream in *opened-test-streams*
             for i = 0 then (+ i 1)
             do (format s "~%; ~A) ~S" i stream))
          (format s "~%")
          (format s "~%(unless (clfs:directory-exists-p ~S)" test-dir)
          (format s "~%  (error \"Test directory ~A does not exist\"))"
                  test-dir)
          (format s "~%")
          (format s "~%(when (clfs:subdirectories ~S)" test-dir)
          (format s "~%  (error \"Test directory ~A is not empty\"))"
                  test-dir)
          (format s "~%")
          (format s "~%(when (clfs:directory-files ~S)" test-dir)
          (format s "~%  (error \"Test directory ~A is not empty\"))"
                  test-dir)
          (format s "~%")
          (format s "~%(setf *default-pathname-defaults* (pathname ~S))"
                  test-dir)
          (format s "~%; (clfs:chdir ~S)" test-dir)
          (format s "~%")
          (format s "~%(setf clfs::*open-streams* nil)")
          (format s "~%")
          (format s "~%(if t  ;; Setting to nil might be sufficient to reproduce the error and simplify analysis")
          (format s "~%")
          (format s "~%  (progn")
          (format s "~%    (setf clfs:*test-pre* nil)")
          (format s "~%    (setf clfs:*test-post* nil)")
          (format s "~%    (setf clfs:*test-diff* nil)")
          (format s "~%    ;; Scenario:~%" )
          (loop
             for sub on actions
             while (rest sub)
             do (format s "~%    (~{~S~^ ~})" (quote-action (first sub)))
             finally
               (format s "~%    (setf clfs:*test-pre* t)")
               (format s "~%    (setf clfs:*test-post* t)")
               (format s "~%    (setf clfs:*test-diff* t)")
               (format s "~%    ;; Error action:~%" )
               (format s "~%    (~{~S~^ ~})" (quote-action (first sub))))
          (format s ")")
          (format s "~%  (progn")
          (format s "~%    (setf clfs:*test-pre* nil)")
          (format s "~%    (setf clfs:*test-post* nil)")
          (format s "~%    (setf clfs:*test-diff* nil)")
          (format s "~2%    ;; Snapshot:~%")
          (loop
             for xs on snapshot
             for x = (first x)
             do (format s "~%    (~S ~S)"
                        (if (uiop:directory-pathname-p x)
                            'ensure-directories-exist 'create-file)
                        x))
          (format s "~%    (setf clfs:*test-pre* t)")
          (format s "~%    (setf clfs:*test-post* t)")
          (format s "~%    (setf clfs:*test-diff* t)")
          (format s "~2%    ;; Action:~%")
          (format s "~%    (~{~S~^ ~})" (quote-action action))
          (format s "))"))))))

(defun quote-action (action)
  "Helper for function function dump-error-scenario. Quotes the
arguments in action when necessary. Makes it so that we can do (eval
action) instead of (apply (first action) (rest action))."
  (let ((fun (first action))
        (args (rest action)))
    (cons fun
          (loop
             with streams = *opened-test-streams*
             for arg in args
             collect (typecase arg
                       (list (when arg
                               (cons 'list arg)))
                       (symbol (if (keywordp arg)
                                   arg (list 'quote arg)))
                       (stream (list 'nth
                                     (position arg streams)
                                     '*opened-test-streams*))
                       (t arg))))))

;; ----------------------------------------------------------------------------
;; Variants using &rest args
;; ----------------------------------------------------------------------------

(defaction ensure-directories-exist-args-variant (path &rest args &key verbose)
  "Variant of clfs:ensure-directories-exist for testing default args."
  (:pre-condition
   (declare (ignore args))
   (clfs:test-pre-condition
    'clfs:ensure-directories-exist path :verbose verbose))
  (:body
   (clfs::validate-access path)
   (if (clfs:execute-p)
       (apply #'cl:ensure-directories-exist
              #+abcl (clfs::absolute-pathname path)
              #-abcl path
              args)
       (apply #'clfs-sandbox:ensure-directories-exist clfs:*sandbox* path args)))
  (:post-condition
   (declare (ignore args))
   (clfs:post-condition-test
    'clfs:ensure-directories-exist path :verbose verbose))
  (:difference
   (declare (ignore args))
   (clfs:difference-test
    'clfs:ensure-directories-exist path :verbose verbose)))

(defaction close-args-variant (stream &rest args &key abort)
  "Sandboxable implementation of Common Lisp function close."
  (:pre-condition
   (declare (ignore args))
   (clfs:test-pre-condition
    'clfs:close stream :abort abort))
  (:body
   (prog1 (if (clfs:execute-p)
              (apply #'cl:close stream args)
              (clfs-sandbox:close clfs:*sandbox* stream :abort abort))
     (setf clfs::*open-streams* (remove stream clfs::*open-streams*))))
  (:post-condition
   (declare (ignore args))
   (clfs:post-condition-test
    'clfs:close stream :abort abort))
  (:difference
   (declare (ignore args))
   (clfs:difference-test
    'clfs:close stream :abort abort)))

(defaction delete-directory-tree-args-variant (directory-pathname
                                               &rest args
                                               &key validate (if-does-not-exist :error))
  "Sandboxable version of uiop:delete-directory-tree."
  (:pre-condition
   (declare (ignore args))
   (clfs:test-pre-condition
    'clfs:delete-directory-tree directory-pathname
    :validate validate
    :if-does-not-exist if-does-not-exist))
  (:body
   (clfs::validate-access directory-pathname)
   (if (clfs:execute-p)
       (apply #'uiop:delete-directory-tree directory-pathname args)
       (clfs-sandbox:delete-directory-tree clfs:*sandbox* directory-pathname
                                           :validate validate
                                           :if-does-not-exist if-does-not-exist)))
  (:post-condition
   (declare (ignore args))
   (clfs:post-condition-test
    'clfs:delete-directory-tree directory-pathname
    :validate validate
    :if-does-not-exist if-does-not-exist))
  (:difference
   (declare (ignore args))
   (clfs:difference-test
    'clfs:delete-directory-tree directory-pathname
    :validate validate
    :if-does-not-exist if-does-not-exist)))
