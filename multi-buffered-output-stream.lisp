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

(defclass multi-buffered-output-stream (buffered-output-stream)
  ((underlying-stream :initarg :stream
                      :reader stream-underlying-stream
                      :type buffered-output-stream)))

(defmethod stream-element-type ((stream multi-buffered-output-stream))
  (stream-element-type (stream-underlying-stream stream)))

(defmethod make-stream-output-buffer ((stream multi-buffered-output-stream))
  (make-instance 'queue))

(defmethod stream-write ((stream multi-buffered-output-stream) element)
  (check-if-open stream)
  (assert (typep element (stream-element-type stream)))
  (let ((underlying-stream (stream-underlying-stream stream)))
    (unless (< (stream-output-length underlying-stream)
               (stream-output-buffer-size underlying-stream))
      (when (< 0 (stream-output-index underlying-stream))
        (setf (stream-output-index stream)
              (stream-output-index underlying-stream)))
      (stream-write (stream-output-buffer stream)
                    (stream-output-buffer underlying-stream))
      (setf (stream-output-buffer underlying-stream)
            (make-stream-output-buffer underlying-stream)
            (stream-output-index underlying-stream) 0
            (stream-output-length underlying-stream) 0))
    (incf (stream-output-length stream))
    (stream-write underlying-stream element)))

(defmethod stream-flush-output-buffer ((stream multi-buffered-output-stream))
  (let ((queue (when (slot-boundp stream 'output-buffer)
                 (stream-output-buffer stream)))
        (underlying-stream (stream-underlying-stream stream)))
    (if (or (null queue) (zerop (queue-length queue)))
        (stream-flush-output-buffer underlying-stream)
        (let ((last-buffer (stream-output-buffer underlying-stream))
              (last-buffer-length (stream-output-length underlying-stream))
              (first-buffer (queue-first queue)))
          (setf (stream-output-buffer underlying-stream) first-buffer
                (stream-output-index underlying-stream)
                (stream-output-index stream)
                (stream-output-length underlying-stream)
                (stream-output-buffer-size underlying-stream))
          (let ((r (stream-flush-output-buffer underlying-stream)))
            (when (= 0 (stream-output-length underlying-stream))
              (discard-stream-output-buffer underlying-stream)
              (setf (queue-first queue) nil)
              (stream-read queue)
              (decf (stream-output-length stream)
                    (stream-output-buffer-size underlying-stream)))
            (setf (stream-output-index stream)
                  (stream-output-index underlying-stream)
                  (stream-output-buffer underlying-stream) last-buffer
                  (stream-output-index underlying-stream) 0
                  (stream-output-length underlying-stream) last-buffer-length)
            r)))))

(defun multi-buffered-output-stream (stream)
  (make-instance 'multi-buffered-output-stream :stream stream))
