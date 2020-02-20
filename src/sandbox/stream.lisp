;; ----------------------------------------------------------------------------
;; Sandbox streams
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
;; The sandbox stream class
;; ----------------------------------------------------------------------------

(defclass sandbox-stream (trivial-gray-streams:fundamental-stream)
  ((path
    :type cl:pathname
    :initarg :path
    :reader path
    :documentation "Pathname of the connected file.")
   (status
    :type symbol
    :initform :open
    :accessor status
    :documentation "One of :open and :closed"))
  (:documentation "A stream for virtual file system FILE-SYSTEM."))

(defmethod cl:open-stream-p ((stream sandbox-stream))
  (equal (status stream) :open))

(defmethod trivial-gray-streams:stream-write-string
    ((stream sandbox-stream) string
     &optional (start 0) (end (length string)))
  (declare (ignore end start))
  (unless (equal (status stream) :open)
    (error "Stream is not open when writing a string"))
  string)

(defmethod trivial-gray-streams:stream-write-char
    ((stream sandbox-stream) char)
  (unless (equal (status stream) :open)
    (error "Stream is not open when writing a char"))
  char)
