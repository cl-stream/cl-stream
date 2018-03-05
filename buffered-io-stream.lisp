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

(defmethod stream-copy ((in buffered-input-stream)
                        (out buffered-output-stream))
  (let ((count 0))
    (loop
       (ecase (stream-fill-input-buffer in)
         ((nil)
          (let ((start (stream-input-index in))
                (length (stream-input-length in)))
            (ecase (stream-write-sequence out (stream-input-buffer in)
                                          :start start
                                          :end (+ start length))
              ((nil)
               (incf count length)
               (incf (stream-input-index in) length))
              ((:eof) (return (values count :eof)))
              ((:non-blocking) (return (values count :non-blocking))))))
         ((:eof) (stream-finish-output out) (return count))
         ((:non-blocking) (return :non-blocking))))))
