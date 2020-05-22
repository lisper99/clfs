;; ----------------------------------------------------------------------------
;; Extra sandboxable actions
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
;; Extra: copy-directory
;; ----------------------------------------------------------------------------

(defaction copy-directory (from to)
  "Copy a directory"
   
  (:pre-condition
   (and (directory-exists-p from)
        (not (probe-file to))))
  
  (:body
   (if
    #+windows (execute-p)
    #-windows nil
    (progn
      (cl:ensure-directories-exist to)
      (uiop:run-program (format nil "xcopy ~S ~S /E /q"
                                (string-trim "/" (namestring (truename from)))
                                (string-trim "/" (namestring (truename to))))
                        :output t
                        :error-output :output))
    ;; This also works on Windows but is more expensive
    (collect-sub*directories
     from
     (lambda (dir) (declare (ignore dir)) t)
     (lambda (dir) (declare (ignore dir)) t)
     (lambda (dir)
       (loop
          with dst = (merge-pathnames (enough-namestring dir from) to)
          initially (ensure-directories-exist dst)
          for file in (directory-files dir)
          for enough = (enough-namestring file from)
          do (copy-file file (merge-pathnames enough to)))))))
  
  (:post-condition
   (declare (ignore from to))
   (lambda (result)
     (declare (ignore result))
     t))
  
  (:difference
   (declare (ignore from to))
   (lambda (result)
     (declare (ignore result))
     t)))

;; ----------------------------------------------------------------------------
;; Extra: zip/unzip
;;
;; untested
;; ----------------------------------------------------------------------------

(defaction zip-directory (directory filespec)
  "Zips the directory to zip file filespec. Paths are stored relative
to the directory."
   
  (:pre-condition
   (and (directory-exists-p directory)
        (not (probe-file filespec))))
  
  (:body
   (validate-access directory)
   (validate-access filespec)
   (when (file-exists-p filespec)
     (error 'file-error :pathname (pathname filespec)))
   (if (execute-p)
       (zip:zip filespec directory)
       (create-file filespec))
   t)
  
  (:post-condition
   (lambda (result)
     (and result
          (file-exists-p filespec)
          (implies (execute-p)
                   (correct-zip-mirror filespec directory)))))
  
  (:difference
   (declare (ignore directory))
   (lambda (result)
     (lambda (removed added)
       (and result
            (null removed)
            (equals-single-pathname added (truename filespec)))))))

(defaction unzip-file (zipfile directory)
  "Unzips the zip file zipfile in directory."
   
  (:pre-condition
   (and (file-exists-p zipfile)
        (directory-exists-p directory)))
  
  (:body
   (validate-access zipfile)
   (validate-access directory)
   (when (execute-p)
     (zip:unzip zipfile directory))
   t)
  
  (:post-condition
   (lambda (result)
     (and result
          (implies (execute-p)
                   (correct-zip-mirror zipfile directory)))))
  
  (:difference
   (declare (ignore zipfile directory))
   (lambda (result)
     (lambda (removed added)
       (declare (ignore added))
       ;; check added against sorted-zip-pathnames
       (and result
            (null removed))))))

(defun correct-zip-mirror (zip-file directory)
  "Is the set of pathname in zip-file equal to the set of pathnames in
directory? Does not compare file contents."
  (clfs-sandbox:sorted-pathname-lists-equal
   (sorted-zip-pathnames zip-file)
   (sorted-directory-pathnames directory)))

(defun sorted-zip-pathnames (zip-file)
  "Helper for correct-zip-mirror."
  (let ((dir (merge-pathnames
              (uiop:ensure-directory-pathname (pathname-name zip-file))
              (uiop:pathname-directory-pathname zip-file)))
        (paths ()))
    (push dir paths)
    (zip:do-zipfile-entries (name entry (zip:open-zipfile zip-file))
      (push (merge-pathnames (pathname name) dir) paths))
    (setf paths (sort paths #'string< :key #'namestring))
    paths))

(defun sorted-directory-pathnames (directory)
  "Helper for correct-zip-mirror."
  (let ((paths ()))
    (collect-sub*directories 
     directory 
     (lambda (x) (declare (ignore x)) t)
     (lambda (x) (declare (ignore x)) t)
     (lambda (x) (push (pathname x) paths)
             (loop for y in (directory-files x)
                do (push (pathname y) paths))))
     (setf paths (sort paths #'string< :key #'namestring))
     paths))

;; ----------------------------------------------------------------------------
;; Extra: download-file
;;
;; untested
;; ----------------------------------------------------------------------------

(defaction download-file (url filespec)
  "Downloads file url and saves it as file filespec."
   
  (:pre-condition
   (declare (ignore url))
   (not (directory-exists-p filespec)))
  
  (:body
   (validate-access filespec)
   (if (execute-p)
       (trivial-download:download url filespec)
       (create-file filespec)))
  
  (:post-condition
   (declare (ignore url))
   (lambda (result)
     (declare (ignore result))
     (file-exists-p filespec)))
  
  (:difference
   (declare (ignore url))
   (lambda (result)
     (declare (ignore result))
     (lambda (removed added)
       (and (null removed)
            (implies added
                     (equals-single-pathname added (truename filespec))))))))
