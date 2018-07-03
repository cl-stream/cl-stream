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

(defclass line-tracking-input-stream (super-stream character-stream
                                                   input-stream)
  ((input-column :initform 0
                 :initarg :input-column
                 :accessor stream-input-column
                 :type fixnum+)
   (input-line :initform 0
               :initarg :input-line
               :accessor stream-input-line
               :type fixnum+)))

(defmethod stream-read ((stream line-tracking-input-stream))
  (multiple-value-bind (item state)
      (stream-read (stream-underlying-stream stream))
    (declare (type character item))
    (cond ((null state)
           (cond ((char= item #\Newline)
                  (incf (the fixnum (stream-input-line stream)))
                  (setf (stream-input-column stream) 0))
                 (t (incf (the fixnum (stream-input-column stream)))))
           (values item nil))
          (t (values item state)))))
