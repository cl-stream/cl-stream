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

(defclass string-output-stream (character-stream vector-output-stream)
  ()
  (:default-initargs :element-type 'character))

(defmethod initialize-instance :before ((s string-output-stream) &rest initargs
                                        &key element-type &allow-other-keys)
  (declare (ignore initargs))
  (assert (subtypep element-type 'character)))
  
(defun string-output-stream (&key (element-type 'character)
                               (output-buffer-size *stream-default-buffer-size*))
  (make-instance 'string-output-stream
                 :element-type element-type
                 :output-buffer-size output-buffer-size))

(defmacro with-output-to-string ((var) &body body)
  "Binds VAR to a new sequence output stream with element-type
character. Returns the sequence output stream string if
BODY returns normally. The stream is closed after BODY returns
normally or before it is aborted by a control transfer of some kind."
  `(with-output-to-vector (,var 'character)
     ,@body))
