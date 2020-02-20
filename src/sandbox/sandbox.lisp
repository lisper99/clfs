;; ----------------------------------------------------------------------------
;; The sandbox class
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
;; Class sandbox
;; ----------------------------------------------------------------------------

(defclass sandbox ()
  ((root-data
    :type directory-data
    :initarg :root-data
    :initform (make-instance 'directory-data)
    :reader root-data
    :documentation
    "Contains the file system's roots. Each root is a directory in the
    directory-data instance.")
   (streams
    :type list
    :initform ()
    :accessor streams
    :documentation
    "List of stream objects created by function clfs-sandbox:open.")
   (current-directory
    :type cl:pathname
    ;;(uiop:getcwd) is problematic in CLISP. Calling (uiop:getcwd)
    ;; resets *default-pathname-defaults*.
    :initform #+clisp *default-pathname-defaults* #-clisp (uiop:getcwd)
    :initarg :current-directory
    :accessor current-directory
    :documentation "The sandbox current working directory. See
    functions chdir and getcwd."))
  (:documentation
   "A virtual file system that is compatible with common lisp's file
   system api. An environent in which file system actions can be
   simulated."))

;; ----------------------------------------------------------------------------
;; Constructors
;; ----------------------------------------------------------------------------

(defun make-empty-sandbox (&optional root-names)
  "Creates a sandbox containing an empty file system. Creates an empty
file system with roots from root-names. These are typically windows
partitions, for example '(\"C:\" \"D:\")."
  (loop
     with sandbox = (make-instance 'sandbox)
     for name in root-names
     do (add-root sandbox name (get-universal-time))
     finally (return sandbox)))

(defun make-populated-sandbox (directories)
  "Creates a sandbox filled with the files from disk in the
directories."
  (loop
     with pathnames = (mapcar #'pathname directories)
     with sandbox = (make-empty-sandbox
                     (remove-duplicates
                      (mapcar #'path-root pathnames)
                      :test #'string-equal))
     for pathname in pathnames
     do (unless (uiop:directory-pathname-p pathname)
          (error
           "Pathname~%  ~A~%is not a valid directory for make-sandbox"
           pathname))
     do (ensure-sandbox-dir sandbox pathname)
     do (populate-file-system-from-disk sandbox pathname)
     finally (return sandbox)))

(defun copy-sandbox (sandbox)
  "A deep copy of sandbox. The file system in the copy shares nothing
with the original. The action log is included, the open streams not."
  (make-instance 'sandbox
    :root-data (copy-data (root-data sandbox))))

;; ----------------------------------------------------------------------------
;; The root nodes
;; ----------------------------------------------------------------------------

(defun add-root (sandbox name date)
  "Addes a root node to sandbox with the given name and date. Returns
the new fs-node."
  (assert (stringp name))
  (add-directory-entry (root-data sandbox)
            (make-instance 'fs-node
              :name name
              :date date
              :content (make-instance 'directory-data)
              :parent nil)))

(defun find-root (sandbox name &key (error-p t) error-value)
  "The root node of sandbox named name. Keywords error-p and
error-value determine behavior if the name does not exist."
  (or (find-directory-entry name (root-data sandbox) :error-p nil)
      (if error-p
          (error
           "Cannot find file system root '~A'. Available roots are ~A."
           name (root-names sandbox))
          error-value)))

(defun remove-root (sandbox name)
  "Removes the node named name from sandbox if it exists."
  (remove-directory-entry (root-data sandbox) name))

(defun root-names (sandbox)
  "A list of strings containing all root names in sandbox."
  (mapcar #'name (entries (root-data sandbox))))

;; ----------------------------------------------------------------------------
;; All paths
;; ----------------------------------------------------------------------------

(defun all-paths (sandbox)
  "A sorted list containing a pathname for every path in sandbox."
  (sort
   (loop for root in (root-names sandbox)
      append (all-paths-from-node (find-root sandbox root)))
   #'string<
   :key #'namestring))

;; ----------------------------------------------------------------------------
;; Operations
;; ----------------------------------------------------------------------------

(defun create-sandbox-file (sandbox pathname &key date)
  (assert (typep pathname 'cl:pathname))
  (unless (uiop:file-pathname-p pathname)
    (error "Pathname~%  ~A~%is not a valid name for create-file" 
           pathname))
  (let ((parent (locate-node sandbox
                             (uiop:pathname-directory-pathname pathname)))
        (file-name (file-namestring pathname)))
    (if (or (null file-name) (equal file-name ""))
        (error "Empty file name in path ~A when creating file" 
               pathname)
        (if parent
            (if (locate-child parent file-name :error-p nil)
                (error "Path '~A' already exists" pathname)
                (add-node parent ;; (content parent)
                          (make-instance 'fs-node
                            :name file-name
                            :date (or date (get-universal-time))
                            :content (make-instance 'file-data)
                            ;;:parent parent
                            )))
            (error "Directory to add file '~A' does not exist" 
                   pathname)))))

(defun absolute-pathname (pathname)
  "Wrapper around uiop:ensure-absolute-pathname that only uses
*default-pathname-defaults*. Sandboxable. Note that
using (get-pathname-defaults) instead of *default-pathname-defaults*
would still be sandboxable and make the absolute pathname depend on
the current working directory. It seems CCL uses the working directory
if no *default-pathname-defaults* is given, but sbcl doesn't. Seems
safest not to depend on cwd and just use *default-pathname-defaults*
to get an absolute directory."
  (uiop:ensure-absolute-pathname pathname *default-pathname-defaults*))

(defun ensure-sandbox-dir (sandbox path)
  "Ensures that all directories in names in that order exists from the
root of the file system with root name root-name. It is an error if
the root does not exist."
  (loop
     with abs-path = (absolute-pathname path)
     with root-name = (path-root abs-path)
     with names = (rest (pathname-directory abs-path))
     with root = (find-root sandbox root-name)
     for name in names
     for parent = root then node
     for available = (locate-child parent name :error-p nil)
     for node = (or available
                    (add-node parent
                               (make-instance 'fs-node
                                 :name name
                                 :date (get-universal-time)
                                 :content (make-instance 'directory-data))))
     for created = (not available) then (or created (not available))
     while node
     finally (return (values (if names node root) created))))

;; ----------------------------------------------------------------------------
;; Other functions
;; ----------------------------------------------------------------------------

(defun locate-node (sandbox path)
  "Find the node for path in sandbox. Returns nil if the node is
not found."
  (loop
     with abs-path = (absolute-pathname path)
     for name in (cons (path-root abs-path) (path-names abs-path))
     for node = (find-root sandbox name)
     then (when (directory-nodep node)
            (locate-child node name :error-p nil))
     unless node do (return nil)
     finally (return node)))

(defun all-opened-streams (sandbox directory)
  "A list of all open streams with a pathname in directory or any of
directory's subdirectories."
  (assert (pathnamep directory))
  (loop
     for stream in (streams sandbox)
     when (uiop:subpathp (path stream) directory)
     collect stream))

(defun file-system-equal (x y)
  "Do file systems x and y contain the same files and directories?
Returns as second value the paths only in x and as third value the
paths only in y."
  (sorted-pathname-lists-equal (all-paths x) (all-paths y)))

(defun sorted-pathname-lists-equal (x y)
  "Compares lists of strings x and y and returns a non-nil value if
the lists contain the same elements. The lists can have duplicates,
but they must be sorted. As second value returns the strings that are
only in x and as third value the strings that are only in y."
  (loop
     for left = (pop x)
     then (if (or left-turn (not right-turn)) (pop x) left)
     for right = (pop y)
     then (if (or right-turn (not left-turn)) (pop y) right)
     while (or left right)
     for left-turn = (or (null right)
                         (when left (string< (namestring left)
                                             (namestring right))))
     for right-turn = (or (null left)
                          (when right (string> (namestring left) 
                                               (namestring right))))
     if left-turn
     collect left into left-only
     if right-turn
     collect right into right-only
     finally (return (values
                      (and (null left-only) (null right-only))
                      left-only
                      right-only))))

(defun print-diff (old new)
  "Prints the difference between pathnames lists old and new. See
take-snapshot and sorted-pathname-lists-equal."
  (multiple-value-bind (equal removed added)
      (sorted-pathname-lists-equal old new)
    (when equal (format t "~%File systems are equal"))
    (when removed
      (loop
         initially (format t "~%Removed:")
         for x in removed
         do (format t "~% ~A" x)))
    (when added
      (loop
         initially (format t "~%Added:")
         for x in added
         do (format t "~%~A" x)))))

;; ----------------------------------------------------------------------------
;; Printing
;; ----------------------------------------------------------------------------

(defun print-file-system (sandbox)
  "Print the entire file system"
  (loop
     with roots = (root-names sandbox)
     initially
       (format t "~&~%File System")
       (format t "~%~4,' D Root(s)" (length roots))
       (terpri)
     for root in roots
     do (format t "~%File system under root '~A'" root)
     do (print-node-tree (find-root sandbox root))
     finally
       (loop
          for path in (all-paths sandbox)
          for is-dir = (uiop:directory-pathname-p path)
          count is-dir into nr-dirs
          count (not is-dir) into nr-files
          finally
            (format t "~%Total:")
            (format t "~%~4,' D File(s)" nr-files)
            (format t "~%~4,' D Dirs(s)" nr-dirs))))

;; ----------------------------------------------------------------------------
;; Mirroring to and populating from disk
;; ----------------------------------------------------------------------------

(defun mirror-file-system-to-disk (sandbox)
  "Writes file-system to disk. Opposite of
populate-file-system-from-disk."
  (loop
     for root in (root-names sandbox)
     do (mirror-fs-node-rec (find-root sandbox root))))

(defun populate-file-system-from-disk (sandbox path)
  "Walks directory path on disk and creates virtual variants for all
directories and file. See mirror-file-system-to-disk."
  (uiop:collect-sub*directories 
   path
   (lambda (x) (declare (ignore x)) t)
   (lambda (x) (declare (ignore x)) t)
   (lambda (x)
      (ensure-sandbox-dir sandbox x)
      (loop for file in (uiop:directory-files x)
         do (create-sandbox-file sandbox file))))
  sandbox)



