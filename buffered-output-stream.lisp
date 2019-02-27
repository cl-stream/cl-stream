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
  ((element-type :initarg :element-type
                 :initform t)
   (output-buffer)
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
gets flushed. To subclass it actual methods are needed for
 MAKE-STREAM-OUTPUT-BUFFER
 STREAM-WRITE-ELEMENT-TO-BUFFER
 STREAM-FLUSH-OUTPUT"))

(defgeneric make-stream-output-buffer (buffered-output-stream)
  (:documentation "Returns a new output buffer for stream."))

(defgeneric stream-buffer-element (stream position)
  (:documentation "Returns the element in stream buffer at position."))

(defgeneric stream-clear-output (buffered-output-stream)
  (:documentation "Removes the contents of the output buffer."))

(defgeneric stream-flush-output (buffered-output-stream)
  (:documentation "Tries to flush once the stream output buffer. Returns
 NIL if the output buffer was partially or completely flushed, or
 :EOF if end of file was reached, or
 :NON-BLOCKING if operation would block."))

(defgeneric stream-output-buffer (buffered-output-stream)
  (:documentation "Returns the stream output buffer, calling
MAKE-STREAM-OUTPUT-BUFFER to create it if needed."))

(defgeneric (setf stream-output-buffer) (value buffered-output-stream)
  (:documentation "Sets the stream output buffer."))

(defgeneric stream-write-element-to-buffer (stream element))

(defmethod stream-clear-output ((stream buffered-output-stream))
  (setf (stream-output-index stream) 0
        (stream-output-length stream) 0))

(defmethod stream-close :before ((stream buffered-output-stream))
  (stream-flush stream))

(defmethod stream-close :after ((stream buffered-output-stream))
  (stream-discard-output-buffer stream))

(defmethod stream-discard-output-buffer ((stream t)))

(defmethod stream-discard-output-buffer ((s buffered-output-stream))
  (setf (stream-output-buffer s) nil))

(defmethod stream-element-type ((s buffered-output-stream))
  (slot-value s 'element-type))

(defmethod stream-flush ((stream buffered-output-stream))
  "Flushes the output buffer of BUFFERED-OUTPUT-STREAM
by repeatedly calling STREAM-FLUSH-OUTPUT until empty. Returns
 NIL if output buffer was empty or emptied, or
 :EOF if end of file was reached, or
 :NON-BLOCKING if write would block."
  (loop
     (case (stream-flush-output stream)
       ((nil) (when (= 0 (the fixnum (stream-output-length stream)))
                (return)))
       ((:eof) (return :eof))
       ((:non-blocking (return :non-blocking)))
       (otherwise (error 'stream-output-error :stream stream)))))

(defmethod stream-flush-output ((stream buffered-output-stream))
  (error "No method for STREAM-FLUSH-OUTPUT ~S." stream))

(defmethod stream-output-buffer ((stream buffered-output-stream))
  (if (slot-boundp stream 'output-buffer)
      (slot-value stream 'output-buffer)
      (setf (slot-value stream 'output-buffer)
            (make-stream-output-buffer stream))))

(defmethod (setf stream-output-buffer) (value (stream buffered-output-stream))
  (setf (slot-value stream 'output-buffer) value))

(defmethod stream-write ((stream buffered-output-stream) element)
  (check-if-open stream)
  (assert (typep element (stream-element-type stream)))
  (if (< (the fixnum (stream-output-length stream))
         (the fixnum (stream-output-buffer-size stream)))
      (stream-write-element-to-buffer stream element)
      (case (stream-flush-output stream)
        ((nil) (stream-write-element-to-buffer stream element))
        ((:eof) :eof)
        ((:non-blocking) :non-blocking)
        (otherwise (error 'stream-output-error :stream stream)))))

(defmethod stream-write-sequence ((s buffered-output-stream)
                                  (seq sequence)
                                  &key start end)
  (check-if-open s)
  (setf start (or start 0))
  (setf end (or end (length seq)))
  (let* ((count 0)
         (state
          (flet ((write-element ()
                   (stream-write-element-to-buffer
                    s (aref seq start))
                   (incf (the fixnum start))
                   (incf (the fixnum count))))
            (loop
               (unless (< (the fixnum start) (the fixnum end))
                 (return))
               (cond ((< (the fixnum (stream-output-length s))
                         (the fixnum (stream-output-buffer-size s)))
                      (write-element))
                     (t
                      (case (stream-flush-output s)
                        ((nil) (write-element))
                        ((:eof) (return :eof))
                        ((:non-blocking) (return :non-blocking))
                        (otherwise (error 'stream-output-error
                                          :stream s)))))))))
    (when state
      (values count state))))
