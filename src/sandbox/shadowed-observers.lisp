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
;; Observers
;; ----------------------------------------------------------------------------

(defun truename (sandbox filespec)
  "Sandbox version of Common Lisp function truename."
  (if (wild-pathname-p filespec)
      (error 'file-error :pathname (pathname filespec))
      (let ((node (locate-node sandbox (absolute-pathname filespec))))
        (if node
            (pathname (full-path-name node))
            (error 'file-error :pathname (pathname filespec))))))

(defun file-exists-p (sandbox path)
  "Sandbox version of uiop:file-exists-p."
  (let ((node (locate-node sandbox path)))
    (when node (not (directory-nodep node)))))

(defun probe-file (sandbox pathspec)
  "Sandbox version of Common Lisp function probe-file."
  (if (typep pathspec 'stream)
      (locate-node sandbox (path pathspec))
      (locate-node sandbox pathspec)))

(defun directory (sandbox pathspec)
  "Sandbox version of Common Lisp function directory."
  (loop for path in (all-paths sandbox)
     if (pathname-match-p path pathspec)
     collect path))

(defun file-author (sandbox pathspec)
  "Sandbox version of Common Lisp function file-author."
  (declare (ignore sandbox pathspec))
  nil)

(defun file-write-date (sandbox pathspec)
  "Sandbox version of Common Lisp function file-write-date."
  (date (locate-node sandbox pathspec)))

(defun file-length (sandbox pathspec)
  "Sandbox version of Common Lisp function file-length."
  (declare (ignore sandbox pathspec))
  0)

(defun file-position (sandbox pathspec)
  "Sandbox version of Common Lisp function file-position."
  (declare (ignore sandbox pathspec))
  0)

;; ----------------------------------------------------------------------------
;; UIOP observers
;; ----------------------------------------------------------------------------

(defun subdirectories (sandbox directory)
  "Sandbox version of uiop:subdirectories."
  (let ((dir-node (locate-node sandbox directory)))
    (if dir-node
        (if (directory-nodep dir-node)
            (sort (mapcar #'node-pathname
                          (remove-if-not #'directory-nodep
                                         (children dir-node)))
                  #'string<
                  :key #'namestring)
            (error "Pathname '~A' is a file, not a directory" directory))
        (error "Directory '~A' does not exist" directory))))

(defun directory-files (sandbox directory)
  "Sandbox version of uiop:subdirectories."
  (let ((dir-node (locate-node sandbox directory)))
    (if dir-node
        (if (directory-nodep dir-node)
            (sort (mapcar #'node-pathname
                          (remove-if #'directory-nodep
                                     (children dir-node)))
                  #'string<
                  :key #'namestring)
            (error "Pathname '~A' is a file, not a directory" directory))
        (error "Directory '~A' does not exist" directory))))

(defun getcwd (sandbox)
  "Sandbox version of uiop:getcwd."
  (current-directory sandbox))

(defun truename* (sandbox pathname)
  "Sandbox version of uiop:truename*."
  (truename sandbox pathname))

(defun truenamize (sandbox pathname)
  "Sandbox version of uiop:truenamize."
  (truename sandbox pathname))

(defun probe-file* (sandbox pathname)
  "Sandbox version of uiop:probe-file*."
  (probe-file sandbox pathname))

