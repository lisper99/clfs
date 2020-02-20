;; ----------------------------------------------------------------------------
;; Sandbox operations
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

(in-package :clfs-sandbox)

;; ----------------------------------------------------------------------------
;; Common Lisp operations
;; ----------------------------------------------------------------------------

(defun ensure-directories-exist (sandbox path &key verbose)
  "Sandbox version of Common Lisp function ensure-directories-exist."
  (declare (ignore verbose))
  (let ((pathname (pathname path)))
    (multiple-value-bind (okay created)
        (ensure-sandbox-dir sandbox pathname)
      (declare (ignore okay))
      (values path created))))

(defun delete-file (sandbox path)
  "Sandbox version of Common Lisp function delete-file."
  (let ((node (locate-node sandbox (pathname path))))
    (if (when node (file-nodep node))
        (delete-node node)
        (error 'file-error :pathname (pathname path)))
    t))

(defun rename-file (sandbox filespec new-name)
  "Sandbox version of Common Lisp function rename-file."
  (let ((node (locate-node sandbox filespec)))
    (if node
        (let ((name (merge-pathnames new-name filespec)))
          (delete-node node)
          (let ((new-node (create-sandbox-file sandbox name)))
            (values name (node-pathname node) (node-pathname new-node))))
        (error 'file-error :pathname (pathname filespec)))))

(defun open (sandbox filename &key
                     (direction :input)
                     (if-exists (if (eq (pathname-version filename) :newest)
                                    :new-version
                                    :error))
                     (if-does-not-exist (cond ((eq direction :probe)
                                               nil)
                                              ((or (eq direction :input)
                                                   (eq if-exists :overwrite)
                                                   (eq if-exists :append))
                                               :error)
                                              (t :create))))
  "Sandbox version of Common Lisp function open."
  (let ((filename (pathname filename)))
    (cond
      ((wild-pathname-p filename)
       (error "Cannot open ~A. The filename cannot be wild." filename))
      ((not (uiop:file-pathname-p filename))
       (error "Cannot open ~A. Expected a filename." filename))
      (t 
       (let ((node
              (let ((existing (locate-node sandbox filename)))
                (if existing
                    (cond
                      ((and (equal direction :output)
                            (equal if-exists :error))
                       (error "Cannot create file ~A. It already exists."
                              filename))
                      ((null if-exists) nil)
                      (t existing))
                    (cond
                      ((equal if-does-not-exist :error)
                       (error "Cannot open file ~A. It does not exist." 
                              filename))
                      ((null if-does-not-exist) nil)
                      (t (create-sandbox-file sandbox filename)))))))
         (when node
           (let ((stream (make-instance 'sandbox-stream
                           :path (pathname filename))))
             (push stream (streams sandbox))
             stream)))))))

(defun close (sandbox stream &key abort)
  "Sandbox version of Common Lisp function close."
  (prog1 (equal (status stream) :open)
    (when abort
      (setf (streams sandbox)
            (remove stream (streams sandbox))))
    (setf (status stream) :closed)))

;; ----------------------------------------------------------------------------
;; UIOP operations
;; ----------------------------------------------------------------------------

(defun chdir (sandbox pathspec)
  "Sandbox version of uiop:chdir."
  (setf (current-directory sandbox)
        (truename sandbox pathspec)))

(defun copy-file (sandbox from to)
  "Sandbox version of uiop:copy-file."
  (unless (file-exists-p sandbox from)
    (error "Cannot copy: ~A does not exist" from))
  (ensure-sandbox-dir sandbox (uiop:pathname-directory-pathname to))
  (create-sandbox-file sandbox (pathname to))
  t)

(defun delete-file-if-exists (sandbox pathspec)
  "Sandbox version of uiop:delete-file-if-exists."
  (let ((node (locate-node sandbox (pathname pathspec))))
    (cond
      ((null node)
       nil)
      ((directory-nodep node)
;;       (error "Cannot delete ~A because it is a directory, not a file."
;;              pathspec)
       nil)
      (t (delete-node node) t))))

(defun directory-exists-p (sandbox path)
  "Sandbox version of uiop:directory-exists-p."
  (let ((node (locate-node sandbox path)))
    (when node (directory-nodep node))))

(defun delete-directory-tree (sandbox pathspec &key
                                      validate if-does-not-exist)
  "Sandbox version of uiop:delete-directory-tree."
  (let ((pathname (pathname pathspec)))
    (unless (uiop:directory-pathname-p pathname)
      (error "Pathname~%  ~A~%is not a valid name for delete-directory-tree" 
             pathname))
    (let ((node (locate-node sandbox pathname)))
      (if node
          (when (uiop:call-function validate pathname)
            (loop for stream in (all-opened-streams sandbox pathname)
               do (setf (status stream) :closed))
            (delete-node node))
          (when (equal if-does-not-exist :error)
            (error "Cannot delete directory tree '~A' because it does not exist"
                   pathspec))))))

(defun delete-empty-directory (sandbox pathspec)
  "Sandbox version of uiop:delete-directory-tree."
  (let ((node (locate-node sandbox (pathname pathspec))))
    (cond
      ((null node)
       (error
        "Cannot delete directory tree '~A' because it does not exist"
        pathspec))
      ((not (directory-nodep node))
       (error
        "Pathname~%  ~A~%is not a valid name for delete-directory-tree" 
        pathspec))
      ((children node)
       (error
        "Cannot delete directory tree '~A' because it is not empty"
        pathspec))
      (t (delete-node node)))))

(defun rename-file-overwriting-target (sandbox filespec new-name)
  "Sandbox version of uiop:rename-file-overwriting-target."
  (let ((node (locate-node sandbox filespec)))
    (if node
        (let ((name (merge-pathnames new-name filespec)))
          (delete-node node)
          (let ((old (locate-node sandbox name)))
            (when old
              (delete-node old)))
          (let ((new-node (create-sandbox-file sandbox name)))
            (values name (node-pathname node) (node-pathname new-node))))
        (error "Cannot rename '~A' because the file does not exist."
               filespec))))

(defun ensure-all-directories-exist (sandbox pathnames)
  "Sandbox version of uiop:ensure-all-directories-exist."
  (loop for pathname in pathnames
     when pathname
     do (ensure-directories-exist sandbox pathname)))

(defun collect-sub*directories (sandbox directory collectp recursep collector)
  "Almost literal copy of uiop:collect-sub*directories."
  (when (uiop:call-function collectp directory)
    (uiop:call-function collector directory)
    (dolist (subdir (subdirectories sandbox directory))
      (when (uiop:call-function recursep subdir)
        (collect-sub*directories
         sandbox subdir collectp recursep collector)))))

(defun directory* (sandbox pathspec)
  "Sandbox version of uiop:directory*."
  (loop for path in (all-paths sandbox)
     if (pathname-match-p path pathspec)
     collect path))

(defun safe-file-write-date (sandbox pathspec)
  "Sandbox version of uiop:safe-file-write-date."
  (declare (ignore sandbox pathspec))
  nil)
