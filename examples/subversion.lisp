;; ----------------------------------------------------------------------------
;; Subversion example
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

(in-package "CL-USER")

;; ----------------------------------------------------------------------------
;; Sandboxable function update-svn-repo
;; ----------------------------------------------------------------------------

(clfs:defaction update-svn-repo (directory &key user password)
  "Arguments user and password are functions that get called to give a
username and a password name. (to avoid logging credentials)"
  (:body
   (if (clfs:execute-p)
       (svn-update directory :user user :password password)
       (multiple-value-bind (repo-only common local-only)
           (svn-repo-status directory :user user :password password)
         (format t "~%Updating '~A' (only simulating adds and deletes)"
                 directory)
         (loop
            with sorted = (sort repo-only #'string<)
            for line in sorted
            do (if (probe-file line) 
                   (error "Subversion update cannot add ~A because it already exists" 
                          line)
                   (progn
                     (clfs::create-file line)
                     (format t "~%Added '~A'" line))))
         (loop
            with sorted = (sort local-only #'string>)
            for line in sorted
            do (if (probe-file line)
                   (progn
                     (clfs:delete-file line)
                     (format t "~%Deleted '~A'" line))
                   (format t "Warning: Subversion skips deleting ~A because it doesn't exist" 
                           line)))
         (loop 
            with sorted = (sort common #'string<)
            for line in sorted
            unless (probe-file line)
            do (format t "~%Restored '~A'" line))
         (format t "~%Repository ~A is up to date~%" directory)))
   (values)))

;; ----------------------------------------------------------------------------
;; Helpers for update-svn-repo
;; ----------------------------------------------------------------------------

(defun svn-update (directory &key user password)
  "Arguments user and password are functions that get called to give a
username and a password name. (to avoid logging credentials)"
  (uiop:run-program
   (format nil "svn update ~A --non-interactive~A"
           directory
           (if (and user password)
               (format nil " --no-auth-cache --username ~A --password ~A"
                       (funcall user) (funcall password))
               ""))
   :output t
   :error-output :output))

(defun svn-ls (directory &key user password remote-p)
  "List of files and directory namestrings in the Subversion
repository directory (not the files on disk!). Arguments user and
password are functions that get called to give a username and a
password name. (to avoid logging credentials). If remote-p is nil it
gives the local repo contents, otherwise the remote repo contents on
the server."
  (let ((ls-result
         (with-output-to-string (*standard-output*)
           (uiop:run-program
            (format nil "svn ls ~A --recursive --non-interactive~A~A"
                    directory
                    (if (and user password)
                        (format nil " --no-auth-cache --username ~A --password ~A"
                                (funcall user) (funcall password))
                        "")
                    (if remote-p " -r HEAD" ""))
            :output t
            :error-output :output
            :ignore-error-status t))))
    (with-input-from-string (s ls-result)
      (loop
         for raw = (read-line s nil nil)
         while raw
         for line = ;; Quick fix to strip the Windows \r
           #+windows (subseq raw 0 (1- (length raw)))
           #-windows raw
         collect (namestring (merge-pathnames line directory))))))

(defun svn-repo-status (directory &key user password)
  "Checks the status of the Subversion repository checked out in the
  given directory againts the head version on the server. Returns two
  values, a list of directory and file names to be added, and a list
  of directory and file names to be deleted."
  (let ((repo (svn-ls directory :user user :password password :remote-p t))
        (local (svn-ls directory :user user :password password :remote-p nil)))
    (values (set-difference repo local :test #'string-equal)
            (intersection repo local :test #'string-equal)
            (set-difference local repo :test #'string-equal))))

