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

(defclass sequence-input-stream (buffered-input-stream)
  ()
  (:documentation "A buffered input stream that reads from a sequence."))

(defmethod initialize-instance ((stream sequence-input-stream)
                                &rest initargs
                                &key sequence &allow-other-keys)
  (declare (ignore initargs)
           (type sequence sequence))
  (call-next-method)
  (setf (slot-value stream 'input-buffer) sequence))

(defmethod stream-element-type ((stream sequence-input-stream))
  (array-element-type (stream-input-buffer stream)))

(defmethod stream-input-buffer-size ((stream sequence-input-stream))
  (length (stream-input-buffer stream)))

(defmethod stream-input-length ((stream sequence-input-stream))
  (length (stream-input-buffer stream)))

(defmethod stream-fill-input-buffer ((stream sequence-input-stream))
  :eof)

(defmacro with-input-from-sequence ((var sequence) &body body)
  "Binds VAR to a new sequence input stream reading from SEQUENCE.
The stream is closed after BODY returns normally or before it is
aborted by a control transfer of some kind."
  (let ((stream (gensym "STREAM-")))
    `(let ((,stream (make-instance 'sequence-input-stream :sequence ,sequence)))
       (unwind-protect (let ((,var ,stream))
                         ,@body)
         (close ,stream)))))

(defmacro with-input-from-string ((var string) &body body)
  "Binds VAR to a new sequence input stream reading from STRING.
The stream is closed after BODY returns normally or before it is
aborted by a control transfer of some kind."
  `(with-input-from-sequence (,var (the string ,string))
     ,@body))
