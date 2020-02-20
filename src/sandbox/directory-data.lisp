;; ----------------------------------------------------------------------------
;; Directory data
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
;; Class directory-data
;; ----------------------------------------------------------------------------

(defclass directory-data ()
  ((entries
    :type list
    :initarg :entries
    :initform ()
    :accessor entries
    :documentation "A list of fs-node objects."))
  (:documentation "A record with directory node data. See class
  FS-NODE."))

(defmethod copy-data ((data directory-data))
  "Makes a new directory-data object with a deep copy of all child
nodes."
  (make-instance 'directory-data
    :entries (mapcar #'copy-fs-node (entries data))))

(defun find-directory-entry (name directory-data &key (error-p t) error-value)
  "Looks up a node."
  (or (find name (entries directory-data) :key #'name :test #'string-equal)
      (if error-p
          (error "Cannot find '~A' in directory ~A." 
                 name (name directory-data))
          error-value)))

(defun add-directory-entry (directory-data node &key (error-p t) error-value)
  "Stores node as child in node directory-data. Returns node."
  (if (find (name node) (entries directory-data)
            :key #'name :test #'string-equal)
      (if error-p
          (error
           "Name ~A already exists in directory ~A"
           (name node) (name directory-data))
          error-value)
      (progn
        (setf (entries directory-data)
              (cons node (entries directory-data)))
        node)))

(defun remove-directory-entry (directory-data name)
  "Removed the node with name name in directory-data. Returns something."
  (setf (entries directory-data)
        (remove name (entries directory-data)
                :key #'name :test #'string-equal)))
