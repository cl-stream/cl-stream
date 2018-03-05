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

(defclass buffered-input-stream (input-stream)
  ((input-buffer)
   (input-buffer-size :initarg :input-buffer-size
                      :initform *stream-default-buffer-size*
                      :reader stream-input-buffer-size)
   (input-index :initform 0
                :accessor stream-input-index
                :type fixnum+)
   (input-length :initform 0
                 :accessor stream-input-length
                 :type fixnum+)))

(defgeneric make-stream-input-buffer (buffered-input-stream)
  (:documentation "Returns a new input buffer for stream."))

(defgeneric discard-stream-input-buffer (buffered-input-stream))

(defgeneric stream-input-buffer (buffered-input-stream)
  (:documentation "Returns the stream input buffer, calling
MAKE-STREAM-INPUT-BUFFER to create it if needed."))

(defgeneric (setf stream-input-buffer) (value buffered-input-stream)
  (:documentation "Sets the stream input buffer."))

(defgeneric stream-fill-input-buffer (buffered-input-stream)
  (:documentation "Fills the stream input buffer.
Returns NIL if successful, or
:EOF if end of file was reached, or
:NON-BLOCKING if operation would block."))

(defmethod make-stream-input-buffer ((stream buffered-input-stream))
  (make-array `(,(stream-input-buffer-size stream))
              :element-type (stream-element-type stream)))

(defmethod discard-stream-input-buffer ((stream buffered-input-stream))
  (setf (stream-input-buffer stream) nil))

(defmethod stream-input-buffer ((stream buffered-input-stream))
  (if (slot-boundp stream 'input-buffer)
      (slot-value stream 'input-buffer)
      (setf (slot-value stream 'input-buffer)
            (make-stream-input-buffer stream))))

(defmethod (setf stream-input-buffer) (value (stream buffered-input-stream))
  (setf (slot-value stream 'input-buffer) value))

(defgeneric stream-read-element-from-buffer (stream))

(defmethod stream-read-element-from-buffer ((stream buffered-input-stream))
  (let ((element (aref (stream-input-buffer stream)
                       (stream-input-index stream))))
    (assert (typep element (stream-element-type stream)))
    (incf (stream-input-index stream))
    (values element nil)))

(defmethod stream-read ((stream buffered-input-stream))
  (check-if-open stream)
  (if (< (stream-input-index stream) (stream-input-length stream))
      (stream-read-element-from-buffer stream)
      (case (stream-fill-input-buffer stream)
        ((nil) (stream-read-element-from-buffer stream))
        ((:eof) (values nil :eof))
        ((:non-blocking) (values nil :non-blocking))
        (otherwise (error 'stream-input-error :stream stream)))))

(defmethod stream-close :after ((stream buffered-input-stream))
  (discard-stream-input-buffer stream))
