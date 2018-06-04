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
  ((open-p :initform t
           :accessor stream-open-p
           :type boolean)
   (element-type :initarg :element-type
                 :initform t
                 :reader stream-element-type))
  (:documentation "A buffered output stream that writes to a sequence."))

(defgeneric sequence-output-stream-sequence (sequence-output-stream)
  (:documentation "Returns the sequence that was written to
SEQUENCE-OUTPUT-STREAM."))

(defgeneric sequence-output-stream-reset (sequence-output-stream)
  (:documentation "Resets SEQUENCE-OUTPUT-STREAM to an empty
 sequence."))

(defmethod sequence-output-stream-sequence ((stream
                                             sequence-output-stream))
  (subseq (stream-output-buffer stream)
          0 (stream-output-length stream)))

(defmethod stream-close ((stream sequence-output-stream))
  (setf (stream-open-p stream) nil))
