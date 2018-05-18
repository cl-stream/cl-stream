;;
;;  cl-stream  -  Stream classes for Common Lisp
;;
;;  Copyright 2017,2018 Thomas de Grivel <thoxdg@gmail.com>
;;
;;  Permission to use, copy, modify, and distribute this software for any
;;  purpose with or without fee is hereby granted, provided that the above
;;  copyright notice and this permission notice appear in all copies.
;;
;;  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;

(in-package :cl-stream)

(defclass stream ()
  ()
  (:documentation "Base class for all streams."))

(defgeneric check-if-open (stream))

(defgeneric stream-blocking-p (stream)
  (:documentation "Returns T if STREAM is in blocking mode,
or NIL if in non-blocking mode."))

(defgeneric (setf stream-blocking-p) (value stream)
  (:documentation "Set to T to put STREAM in blocking mode,
or NIL for non-blocking mode."))

(defgeneric stream-close (stream)
  (:documentation "Prevent all further operations on STREAM."))

(defgeneric stream-element-type (stream)
  (:documentation "Returns the type of elements of STREAM."))

(defgeneric stream-open-p (stream)
  (:documentation "Returns T is STREAM is open, or NIL if closed."))

(define-condition stream-error (error)
  ((stream :initarg :stream
           :reader stream-error-stream
           :type stream))
    (:documentation "Superclass for all errors related to streams."))

(define-condition stream-closed-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "~S is closed."
                     (stream-error-stream condition))))
  (:documentation "An error that is signalled when trying to read from
or write to a closed stream."))

(define-condition stream-end-error (stream-error)
  ()
  (:report (lambda (condition stream)
             (format stream "End of stream ~S."
                     (stream-error-stream condition))))
  (:documentation "An error that is signalled when stream end was reached."))

(defvar *stream-default-buffer-size*
  4096)

(defmethod check-if-open (stream)
  "Checks if STREAM is open and signals an error otherwise."
  (unless (stream-open-p stream)
    (error 'stream-closed-error
           :stream stream)))

(defmethod stream-blocking-p ((stream stream))
  t)

(defmacro with-stream ((var stream) &body body)
  "Ensures STREAM gets closed returning from BODY with VAR bound to STREAM."
  (let ((s (gensym "STREAM-")))
    `(let ((,s ,stream))
       (unwind-protect (let ((,var ,s))
                         ,@body)
         (stream-close ,s)))))
