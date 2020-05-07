;; ----------------------------------------------------------------------------
;; All sandboxable observers
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
;; Function pathname
;; ----------------------------------------------------------------------------

(defun pathname (pathspec)
  "Version of Common Lisp function pathname that handles
sandbox-stream streams."
  (if (typep pathspec 'clfs-sandbox:sandbox-stream)
      (clfs-sandbox:path pathspec)
      (cl:pathname pathspec)))

;; ----------------------------------------------------------------------------
;; Common Lisp functions
;; ----------------------------------------------------------------------------

(defun truename (filespec)
  "Sandboxable version of Common Lisp function truename."
  (if (execute-p)
      (cl:truename filespec)
      (clfs-sandbox:truename *sandbox* filespec)))

(defun probe-file (pathspec)
  "Sandboxable version of Common Lisp function probe-file."
  (if (execute-p)
      (cl:probe-file pathspec)
      (clfs-sandbox:probe-file *sandbox* pathspec)))

(defun file-author (pathspec)
  "Sandboxable version of Common Lisp function file-author."
  (if (execute-p)
      (cl:file-author pathspec)
      (clfs-sandbox:file-author *sandbox* pathspec)))

(defun file-write-date (pathspec)
  "Sandboxable version of Common Lisp function file-write-date."
  (if (execute-p)
      (cl:file-write-date pathspec)
      (clfs-sandbox:file-write-date *sandbox* pathspec)))

(defun file-length (pathspec)
  "Sandboxable version of Common Lisp function file-write-date."
  (if (execute-p)
      (cl:file-length pathspec)
      (clfs-sandbox:file-length *sandbox* pathspec)))

(defun file-position (pathspec)
  "Sandboxable version of Common Lisp function file-write-date."
  (if (execute-p)
      (cl:file-position pathspec)
      (clfs-sandbox:file-position *sandbox* pathspec)))

(defun directory (pathspec &rest keys &key &allow-other-keys)
  "Sandboxable version of Common Lisp function directory"
  (if (execute-p)
      (apply #'cl:directory pathspec keys)
      (clfs-sandbox:directory *sandbox* pathspec)))

;; ----------------------------------------------------------------------------
;; UIOP functions
;; ----------------------------------------------------------------------------

;; todo: parse-file-location-info
;; todo: parse-windows-shortcut
;; todo: resolve-symlinks*
;; todo: resolve-symlinks

(defun get-pathname-defaults (&optional (defaults *default-pathname-defaults*))
  "Sandboxable version of uiop:get-pathname-defaults. No need to
dispatch. Simply a copy of the uiop code, except the call to the
sandboxable getcwd."
  (or (uiop:absolute-pathname-p defaults)
      (uiop:merge-pathnames* defaults (getcwd))))

(defun directory-exists-p (path)
  "Sandboxable version of uiop:directory-exists-p."
  (if (execute-p)
      #+ccl(when (uiop:directory-exists-p path)
             ;; copied from probe-file
             (let ((real-path (ccl::%realpath
                               (ccl::defaulted-native-namestring path))))
               (when real-path
                 (eql (ccl::%unix-file-kind real-path) :directory))))
      #-ccl(uiop:directory-exists-p path)
      (clfs-sandbox:directory-exists-p *sandbox* path)))

(defun file-exists-p (path)
  "Sandboxable version of uiop:file-exists-p."
  (if (execute-p)
      #-ccl(uiop:file-exists-p path)  ;; FIXME: also allows dirs!!!
      #+ccl(when (uiop:file-exists-p path)
             ;; copied from probe-file
             (let ((real-path (ccl::%realpath
                               (ccl::defaulted-native-namestring path))))
               (when real-path
                 (eql (ccl::%unix-file-kind real-path) :file))))
      (clfs-sandbox:file-exists-p *sandbox* path)))

(defun subdirectories (directory)
  "Sandboxable version of uiop:subdirectories."
  (if (execute-p)
      (uiop:subdirectories directory)
      (clfs-sandbox:subdirectories *sandbox* directory)))

(defun directory-files (directory)
  "Sandboxable version of uiop:directory-files."
  (if (execute-p)
      (uiop:directory-files directory)
      (clfs-sandbox:directory-files *sandbox* directory)))

(defun collect-sub*directories (directory collectp recursep collector)
  "Sandboxable version of uiop:collect-sub*directories."
  (if (execute-p)
      (uiop:collect-sub*directories directory collectp recursep collector)
      (clfs-sandbox:collect-sub*directories
       *sandbox* directory collectp recursep collector)))

(defun directory* (pathname-spec &rest keys &key &allow-other-keys)
  "Sandboxable version of uiop:directory*."
  (if (execute-p)
      (apply #'uiop:directory* pathname-spec keys)
      (funcall #'clfs-sandbox:directory* *sandbox* pathname-spec)))

(defun safe-file-write-date (pathname)
  "Sandboxable version of uiop:safe-file-write-date"
  (if (execute-p)
      (uiop:safe-file-write-date pathname)
      (clfs-sandbox:safe-file-write-date *sandbox* pathname)))

(defun getcwd ()
  "Sandboxable version of uiop:getcwd"
  (if (execute-p)
      ;;(uiop:getcwd) is problematic in CLISP. Calling (uiop:getcwd)
      ;; resets *default-pathname-defaults*.
      #-clisp(uiop:getcwd)
      #+clisp *default-pathname-defaults*
      (clfs-sandbox:getcwd *sandbox*)))

(defun truename* (pathname)
  "Sandboxable version of uiop:getcwd"
  (if (execute-p)
      (uiop:truename* pathname)
      (clfs-sandbox:truename* *sandbox* pathname)))

(defun truenamize (pathname)
  "Sandboxable version of uiop:getcwd"
  (if (execute-p)
      (uiop:truenamize pathname)
      (clfs-sandbox:truenamize *sandbox* pathname)))

(defun probe-file* (pathname)
  "Sandboxable version of uiop:probefile*"
  (if (execute-p)
      (uiop:probe-file* pathname)
      (clfs-sandbox:probe-file* *sandbox* pathname)))

;; ----------------------------------------------------------------------------
;; Other functions
;; ----------------------------------------------------------------------------

(defun ensureable (path)
  "Can path be ensured? Sandboxable."
  (loop
     for p = (absolute-pathname path) then parent
     for parent = (uiop:pathname-parent-directory-pathname p)
     until (uiop:pathname-equal p parent)
     never (uiop:file-exists-p
            (merge-pathnames (first (last (pathname-directory p))) parent))))

(defun open-file-p (file)
  "Is argument file the pathname of some open stream. Sandboxable."
  (when (file-exists-p file)
    (loop
       with abs = (truename file)
       for stream in (if (execute-p)
                         *open-streams*
                         (clfs-sandbox:streams *sandbox*))
       thereis (and
                (open-stream-p stream)
                (uiop:pathname-equal (absolute-pathname (pathname stream))
                                     abs)))))

(defun any-open-file-p (directory)
  "Walks directory directory on disk and checks if any file is open."
  (let ((any-open nil))
    (uiop:collect-sub*directories 
     directory 
     (lambda (x) (declare (ignore x)) (not any-open))
     (lambda (x) (declare (ignore x)) (not any-open))
     (lambda (x)
       (setf any-open
             (loop for file in (uiop:directory-files x)
                thereis (open-file-p file)))))
    any-open))
