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

(defclass io-stream (input-stream output-stream)
  ())

(defgeneric stream-copy (in out))

(defgeneric stream-copy-n (in out limit))

(defmethod stream-copy (in out)
  (let ((count 0)
        (copy-status t))
    (loop
       (unless (eq t copy-status)
         (return))
       (multiple-value-bind (element status) (stream-read in)
         (ecase status
           ((nil) (ecase (stream-write out element)
                    ((nil) (incf count))
                    ((:eof) (setq copy-status :eof))
                    ((:non-blocking (setq copy-status :non-blocking)))))
           ((:eof) (setq copy-status nil))
           ((:non-blocking) (setq copy-status :non-blocking)))))
    (values count copy-status)))

(defmethod stream-copy-n (in out (limit integer))
  (let ((count 0)
        (copy-status t))
    (loop
       (unless (eq t copy-status)
         (return))
       (multiple-value-bind (element status) (stream-read in)
         (ecase status
           ((nil) (ecase (stream-write out element)
                    ((nil) (incf count))
                    ((:eof) (setq copy-status :eof))
                    ((:non-blocking (setq copy-status :non-blocking)))))
           ((:eof) (setq copy-status nil))
           ((:non-blocking) (setq copy-status :non-blocking))))
       (unless (< count limit)
         (return)))
    (values count copy-status)))
