;; ----------------------------------------------------------------------------
;; Class fs-node
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
;; File system node
;; ----------------------------------------------------------------------------

(defclass fs-node ()
  ((name
    :type string
    :initarg :name
    :reader name
    :documentation "The name of the node entry. For files this
    includes the extension.")
   (date
    :type integer
    :initarg :date
    :reader date
    :documentation "The universal time of the file write date.")
   (parent
    :type (or null fs-node)
    :initarg :parent
    :accessor parent
    :documentation "Is nil for the root and the parent fs-node for the
    rest")
   (content
    :type (or directory-data file-data)
    :initarg :content
    :reader content
    :documentation "A record with content. The content's type
    determines if the node is a file or a directory."))
  (:documentation "A node for the file system. See class sandbox."))

(defun copy-fs-node (fs-node)
  "Makes a deep copy of a file system node"
  (make-instance 'fs-node
    :name (name fs-node)
    :date (date fs-node)
    :content (copy-data (content fs-node))
    :parent (parent fs-node)))

(defun directory-nodep (fs-node)
  "True if fs-node a directory, false if it is a file."
  (typep (content fs-node) 'directory-data))

(defun file-nodep (fs-node)
  "True if fs-node a directory, false if it is a file."
  (typep (content fs-node) 'file-data))

(defun children (dir-node)
  "All child nodes of file system node dir-node"
  (assert (directory-nodep dir-node))
  (entries (content dir-node)))

(defun locate-child (dir-node name &key (error-p t) error-value)
  "The child node of file system node dir-node named name. Keywords
error-p and error-value determine behavior if the name does not
exist."
  (assert (directory-nodep dir-node))
  (find-directory-entry name (content dir-node)
                        :error-p error-p
                        :error-value error-value))

;; ----------------------------------------------------------------------------
;; Operations
;; ----------------------------------------------------------------------------

(defun delete-node (fs-node)
  "Stores node under name in node dir-node. Returns node."
  (assert (typep fs-node 'fs-node))
  (remove-directory-entry (content (parent fs-node))
                          (name fs-node)))

(defun add-node (dir-node fs-node &key (error-p t) error-value)
  "Stores fs-node as child in node dir-node. Returns fs-node."
  (assert (directory-nodep dir-node))
  (assert (typep fs-node 'fs-node))
  (prog1 (add-directory-entry (content dir-node) fs-node
                   :error-p error-p
                   :error-value error-value)
    (setf (parent fs-node) dir-node)))

;; ----------------------------------------------------------------------------
;; Pathnames
;; ----------------------------------------------------------------------------

(defun full-name (fs-node)
  "The names of all parents of fs-node separated by slashes."
  (if (parent fs-node)
      (format nil "~A/~A" (full-name (parent fs-node)) (name fs-node))
      (name fs-node)))

(defun full-path-name (fs-node)
  "Same as full-name except it adds a slash to directories."
  (if (parent fs-node)
      (format nil "~A~A~A"
              (full-path-name (parent fs-node))
              (name fs-node)
              (if (directory-nodep fs-node) "/" ""))
      (format nil "~A/" (name fs-node))))

(defun node-pathname (fs-node)
  (parse-namestring (full-path-name fs-node)))

(defun all-paths-from-node (fs-node)
  "List containing a pathname for every node from file-system in the
subtree for node."
  (cons (node-pathname fs-node)
        (when (directory-nodep fs-node)
          (loop
             for child in (children fs-node)
             append (all-paths-from-node child)))))

;; ----------------------------------------------------------------------------
;; Mirroring to disk
;; ----------------------------------------------------------------------------

(defun mirror-fs-node-rec (parent-node)
  "Helper for mirror-file-system-to-disk."
  (assert (directory-nodep parent-node))
  (loop
     with for-real = t
     for file-node in (children parent-node)
     for full-name = (full-path-name file-node)
     unless (uiop:probe-file* full-name)
     if (directory-nodep file-node)
     do (let ((path full-name))
          (if (uiop:probe-file* path)
              (error "Oops, directory ~A already exists!!!" path)
              (progn
                (when for-real
                  (cl:ensure-directories-exist path)))))
     else do (if (uiop:probe-file* full-name)
                 (error "Oops, file ~A already exists!!!" full-name)
                 (progn
                   (when for-real
                     (cl:with-open-file (s (full-name file-node)
                                        :direction :output
                                        :if-exists :error
                                        :if-does-not-exist :create)
                       (format s "Hello World!~%"))))))
  (loop
     for dir-node
     in (sort (remove-if-not #'directory-nodep (children parent-node))
              #'string< :key #'name)
     do (mirror-fs-node-rec dir-node)))

;; ----------------------------------------------------------------------------
;; Printing
;; ----------------------------------------------------------------------------

(defun print-node-tree (fs-node)
  "Prints the node and its children recursively."
  (assert (directory-nodep fs-node))
  (loop
     initially (if (equal (path-root (full-path-name fs-node)) "")
                   (print-directory-node-nix fs-node)
                   (print-directory-node-win fs-node))
     for dir-node
     in (sort (remove-if-not #'directory-nodep (children fs-node))
              #'string< :key #'name)
     do (print-node-tree dir-node)))

(defun print-directory-node-win (fs-node)
  "Prints the files and directories in directory fs-node."
  (assert (directory-nodep fs-node))
  (loop
     initially
       (format t "~2% Directory of ~A~%" (full-path-name fs-node))
     with children = (children fs-node)
     for file-node in (sort (copy-list children) #'string< :key #'name)
     for file-name = (name file-node)
     for file-date = (date file-node)
     for kind = (if (directory-nodep file-node) "<DIR>" "     ")
     count (directory-nodep file-node) into nr-dirs
     do (multiple-value-bind (second minute hour date month year)
            (decode-universal-time file-date)
          (format t "~%~A/~2,'0D/~2,'0D  ~2,'0D:~2,'0D:~2,'0D    ~A    ~A"
                  year month date hour minute second kind file-name))
     finally
       (format t "~%~20,' D File(s)" (- (length children) nr-dirs))
       (format t "~%~20,' D Dir(s)" nr-dirs)))

(defun print-directory-node-nix (fs-node)
  "Prints the files and directories in directory fs-node."
  (assert (directory-nodep fs-node))
  (flet ((print-line (name date dir-p)
           (multiple-value-bind (second minute hour date month year)
               (decode-universal-time date)
             (format t "~Arwx------ ~A/~2,'0D/~2,'0D  ~2,'0D:~2,'0D:~2,'0D    ~A~%"
                     (if dir-p "d" "-")
                     year month date hour minute second name))))
  (loop
     with children = (children fs-node)
     initially
       (format t "~2% Directory of ~A~%" (full-path-name fs-node))
       (format t "~%total ~A~%" 0)
       (print-line "." (date fs-node) t)
       (when (parent fs-node)
         (print-line ".." (date (parent fs-node)) t))
     for file-node in (sort (copy-list children) #'string< :key #'name)
     for file-name = (name file-node)
     for file-date = (date file-node)
     do (print-line file-name file-date (directory-nodep file-node)))))

;; ----------------------------------------------------------------------------
;; Utilities
;; ----------------------------------------------------------------------------

(defun path-names (path)
  (assert (uiop:absolute-pathname-p path))
  (let ((dir (pathname-directory path))
        (name (file-namestring path)))
    (assert (equal (first dir) :absolute))
    (if (and name (not (equal name "")))
        (append (rest dir) (list name))
        (rest dir))))

(defun path-root (path)
  (assert (uiop:absolute-pathname-p path))
  (let ((device (pathname-device path)))
    (if (when device (not (equal device "")))
        (concatenate 'string device ":")
        (or device ""))))
