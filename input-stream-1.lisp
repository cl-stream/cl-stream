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

;;  Split for dependency on sequence-output-stream.lisp

(defmethod stream-read-until (stream end-element)
  (block nil
    (let ((type (stream-element-type stream)))
      (assert (typep end-element type))
      (with-output-to-vector (out type)
        (loop
           (multiple-value-bind (element state) (stream-read stream)
             (case state
               ((nil)
                (stream-write out element)
                (when (eq element end-element)
                  (return (values (sequence-output-stream-sequence out)
                                  nil))))
               ((:eof)
                (return (values (sequence-output-stream-sequence out)
                                :eof)))
               ((:non-blocking)
                (return (values (sequence-output-stream-sequence out)
                                :non-blocking)))
               (otherwise
                (error 'stream-input-error :stream stream)))))))))

#+test
(with-input-from-string (in "hello world !")
  (stream-read-until in #\Space))

(defgeneric stream-read-line (input-stream))

(defmethod stream-read-line ((stream character-stream))
  (stream-read-until stream #\Newline))
