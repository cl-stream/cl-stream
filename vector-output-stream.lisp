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

(defclass vector-output-stream (sequence-output-stream)
  ())

(defmethod make-stream-output-buffer ((stream vector-output-stream))
  (make-array `(,(stream-output-buffer-size stream))
              :element-type (stream-element-type stream)))

(defmethod sequence-output-stream-reset ((stream vector-output-stream))
  (setf (stream-output-length stream) 0))

(defmethod stream-flush ((stream vector-output-stream))
  nil)

(defmethod stream-flush-output ((stream vector-output-stream))
  (setf (slot-value stream 'output-buffer)
        (let* ((output-buffer (stream-output-buffer stream))
               (length (length output-buffer))
               (size (stream-output-buffer-size stream))
               (new-size (+ length size)))
          (adjust-array output-buffer `(,new-size))))
  nil)

(defmethod stream-write-element-to-buffer ((stream vector-output-stream)
                                           element)
  (setf (aref (stream-output-buffer stream) (stream-output-length stream))
        element)
  (incf (stream-output-length stream))
  nil)

(defmacro with-output-to-vector ((var &optional (element-type t))
                                &body body)
  "Binds VAR to a new vector output stream with element-type
ELEMENT-TYPE. Returns the output vector if BODY returns normally.
The stream is closed after BODY returns normally or before it is
aborted by a control transfer of some kind."
  (let ((stream (gensym "STREAM-")))
    `(with-stream (,stream (make-instance 'vector-output-stream
                                          :element-type ,element-type))
       (let ((,var ,stream))
         ,@body
         (sequence-output-stream-sequence ,stream)))))
