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

(defclass sequence-output-stream (buffered-output-stream)
  ()
  (:documentation "A buffered output stream that writes to a sequence."))

(defgeneric sequence-output-stream-sequence (sequence-output-stream)
  (:documentation "Returns the sequence that was written to
SEQUENCE-OUTPUT-STREAM."))

(defgeneric sequence-output-stream-reset (sequence-output-stream))

(defmethod sequence-output-stream-sequence ((stream sequence-output-stream))
  (subseq (stream-output-buffer stream) 0 (stream-output-length stream)))

(defmethod sequence-output-stream-reset ((stream sequence-output-stream))
  (setf (stream-output-length stream) 0))

(defmethod initialize-instance ((stream sequence-output-stream)
                                &rest initargs
                                &key element-type &allow-other-keys)
  (declare (ignore initargs))
  (call-next-method)
  (setf (slot-value stream 'output-buffer)
        (make-array `(,*stream-default-buffer-size*)
                    :element-type element-type
                    :adjustable t)))

(defmethod stream-element-type ((stream sequence-output-stream))
  (array-element-type (stream-output-buffer stream)))

(defmethod stream-output-buffer-size ((stream sequence-output-stream))
  (length (stream-output-buffer stream)))

(defmethod stream-flush ((stream sequence-output-stream))
  nil)

(defmethod stream-flush-output-buffer ((stream sequence-output-stream))
  (setf (slot-value stream 'output-buffer)
        (let ((output-buffer (stream-output-buffer stream)))
          (adjust-array output-buffer
                        `(,(+ (length output-buffer)
                              *stream-default-buffer-size*)))))
  nil)

(defmacro with-output-to-sequence ((var element-type) &body body)
  "Binds VAR to a new sequence output stream with element-type
ELEMENT-TYPE. Returns the sequence output stream sequence if
BODY returns normally. The stream is closed after BODY returns
normally or before it is aborted by a control transfer of some kind."
  (let ((stream (gensym "STREAM-")))
    `(let ((,stream (make-instance 'sequence-output-stream
                                   :element-type ,element-type)))
       (unwind-protect (let ((,var ,stream))
                         ,@body
                         (sequence-output-stream-sequence ,stream))
         (close ,stream)))))

(defmacro with-output-to-string ((var) &body body)
  `(with-output-to-sequence (,var 'string) ,@body))
