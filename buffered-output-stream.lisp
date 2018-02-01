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

(defclass buffered-output-stream (output-stream)
  ((output-buffer)
   (output-buffer-size :initarg :output-buffer-size
                       :initform *stream-default-buffer-size*
                       :reader stream-output-buffer-size)
   (output-index :initform 0
                 :accessor stream-output-index
                 :type fixnum+)
   (output-length :initform 0
                  :accessor stream-output-length
                  :type fixnum+))
  (:documentation "An output stream that buffers its writes until it
gets flushed."))

(defgeneric make-stream-output-buffer (buffered-output-stream)
  (:documentation "Returns a new output buffer for stream."))

(defgeneric discard-stream-output-buffer (buffered-output-stream))

(defgeneric stream-output-buffer (buffered-output-stream)
  (:documentation "Returns the stream output buffer, calling
MAKE-STREAM-OUTPUT-BUFFER to create it if needed."))

(defgeneric (setf stream-output-buffer) (value buffered-output-stream)
  (:documentation "Sets the stream output buffer."))

(defgeneric stream-flush-output-buffer (buffered-output-stream)
  (:documentation "Tries to flush once the stream output buffer. Returns
 NIL if successful, or
 :EOF if end of file was reached, or
 :NON-BLOCKING if operation would block."))

(defgeneric stream-write-element-to-buffer (stream element))

(defgeneric flush (buffered-output-stream)
  (:documentation "Flushes the output buffer of BUFFERED-OUTPUT-STREAM
by repeatedly calling STREAM-FLUSH-OUTPUT-BUFFER until empty. Returns
 NIL if output buffer was empty or emptied, or
 :EOF if end of file was reached, or
 :NON-BLOCKING if write would block."))

(defmethod make-stream-output-buffer ((stream buffered-output-stream))
  (make-array `(,(stream-output-buffer-size stream))
              :element-type (stream-element-type stream)))

(defmethod discard-stream-output-buffer ((stream buffered-output-stream))
  (setf (stream-output-buffer stream) nil))

(defmethod stream-output-buffer ((stream buffered-output-stream))
  (if (slot-boundp stream 'output-buffer)
      (slot-value stream 'output-buffer)
      (setf (slot-value stream 'output-buffer)
            (make-stream-output-buffer stream))))

(defmethod (setf stream-output-buffer) (value (stream buffered-output-stream))
  (setf (slot-value stream 'output-buffer) value))

(defmethod stream-write-element-to-buffer ((stream buffered-output-stream)
                                           element)
  (setf (aref (stream-output-buffer stream) (stream-output-length stream))
        element)
  (incf (stream-output-length stream))
  nil)

(defmethod stream-write ((stream buffered-output-stream) element)
  (check-if-open stream)
  (assert (typep element (stream-element-type stream)))
  (if (< (stream-output-length stream) (stream-output-buffer-size stream))
      (stream-write-element-to-buffer stream element)
      (case (stream-flush-output-buffer stream)
        ((nil) (stream-write-element-to-buffer stream element))
        ((:eof) :eof)
        ((:non-blocking) :non-blocking)
        (otherwise (error 'stream-output-error :stream stream)))))

(defmethod stream-flush ((stream buffered-output-stream))
  (loop
     (case (stream-flush-output-buffer stream)
       ((nil) (when (= 0 (stream-output-length stream))
                (return)))
       ((:eof) (return :eof))
       ((:non-blocking (return :non-blocking)))
       (otherwise (error 'stream-output-error :stream stream)))))

(defmethod stream-close :before ((stream buffered-output-stream))
  (flush stream))

(defmethod stream-close :after ((stream buffered-output-stream))
  (discard-stream-output-buffer stream))
