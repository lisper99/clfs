;; ----------------------------------------------------------------------------
;; Package definition for the clfs package
;; 
;; Paul Griffioen 2020-2020
;; ----------------------------------------------------------------------------

(defpackage :clfs
  (:documentation "The clfs package provides a library for safe
  file management. A virtual file system allows simulation of actions
  before they are really performed.")
  (:use :common-lisp)
  (:shadow "ENSURE-DIRECTORIES-EXIST"
           "RENAME-FILE"
           "DELETE-FILE"
           "OPEN"
           "CLOSE"
           "WRITE-STRING"
           "WITH-OPEN-FILE"
           "FILE-LENGTH"
           "FILE-POSITION"
           "PATHNAME"
           "TRUENAME"
           "PROBE-FILE"
           "FILE-AUTHOR"
           "FILE-WRITE-DATE"
           "DIRECTORY")
  (:export

   "*SANDBOX*"
   "*WHITELIST*"
   "*SIMULATE*"
   "*TEST-PRE*"
   "*TEST-POST*"
   "*TEST-DIFF*"
   "*CONFINE*"

   "WITH-SANDBOX"
   "WITH-ACCESS"
   "DEFACTION"

   "EXECUTE-P"
   "SIMULATE-P"

   "TEST-PRE-CONDITION"
   "POST-CONDITION-TEST"
   "DIFFERENCE-TEST"

   "ENSUREABLE"
   
   "ENSURE-DIRECTORIES-EXIST"
   "RENAME-FILE"
   "DELETE-FILE"
   "DELETE-FILE*"
   "OPEN"
   "CLOSE"
   "WITH-OPEN-FILE"
   "COPY-FILE"
   "DELETE-DIRECTORY-TREE"
   "DELETE-EMPTY-DIRECTORY"
   "DELETE-FILE-IF-EXISTS"
   "RENAME-FILE-OVERWRITING-TARGET"
   "ENSURE-ALL-DIRECTORIES-EXIST"
   "PATHNAME"
   "TRUENAME"
   "PROBE-FILE"
   "FILE-AUTHOR"
   "FILE-WRITE-DATE"
   "DIRECTORY"
   "DIRECTORY-EXISTS-P"
   "FILE-EXISTS-P"
   "SUBDIRECTORIES"
   "DIRECTORY-FILES"
   "COLLECT-SUB*DIRECTORIES"
   "DIRECTORY*"
   "SAFE-FILE-WRITE-DATE"

   "GETCWD"
   "CHDIR"

   "SAFE-COPY-FILE"
   "SAFE-RENAME-FILE"
   "SAFE-RENAME-FILE-OVERWRITING-TARGET"
   "SAFE-DELETE-FILE"
   "SAFE-DELETE-DIRECTORY-TREE"
   "SAFE-ENSURE-DIRECTORIES-EXIST"
   
   "WRITE-STRING"

   #+:wrapuiop "PATHNAME-DIRECTORY-PATHNAME"
   #+:wrapuiop "DIRECTORY-PATHNAME-P"

   "VARY-FILE-TYPE"
   "COPY-DIRECTORY"
   "ZIP-DIRECTORY"
   "UNZIP-FILE"
   "DOWNLOAD-FILE"))
