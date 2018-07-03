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

(defmethod make-stream-output-buffer ((stream multi-buffered-output-stream))
  (make-instance 'queue))

(defmethod stream-close ((stream multi-buffered-output-stream))
  (stream-discard-output-buffer stream)
  (stream-close (stream-underlying-stream stream)))

(defmethod stream-discard-output-buffer ((stream multi-buffered-output-stream))
  (stream-discard-output-buffer (stream-underlying-stream stream))
  (call-next-method))

(defmethod stream-element-type ((stream multi-buffered-output-stream))
  (stream-element-type (stream-underlying-stream stream)))

(defmethod stream-open-p ((stream multi-buffered-output-stream))
  (stream-open-p (stream-underlying-stream stream)))

(defmethod multi-buffer-write ((stream multi-buffered-output-stream))
  (let ((queue (stream-output-buffer stream))
        (us (stream-underlying-stream stream)))
    (let ((buf (cond ((= 0 (the fixnum (queue-length queue)))
                      (let ((buf (make-stream-output-buffer us)))
                        (stream-write queue buf)
                        buf))
                     (t (queue-last queue)))))
      (setf (stream-output-buffer us) buf))))

(defmethod stream-write ((stream multi-buffered-output-stream) element)
  (check-if-open stream)
  (assert (typep element (stream-element-type stream)))
  (let ((underlying-stream (stream-underlying-stream stream)))
    (multi-buffer-write stream)
    (incf (the fixnum (stream-output-length stream)))
    (stream-write underlying-stream element)))

(defmethod multi-buffer-output ((stream multi-buffered-output-stream))
  (let ((queue (when (slot-boundp stream 'output-buffer)
                 (stream-output-buffer stream)))
        (us (stream-underlying-stream stream)))
    (when (and queue (< 0 (the fixnum (queue-length queue))))
      (let* ((first (queue-first queue))
             (size (stream-output-buffer-size us))
             (index (mod (the fixnum (stream-output-index stream))
                         size))
             (length (mod (the fixnum (stream-output-length stream))
                          size)))
        (declare (type fixnum size index length))
        (setf (stream-output-buffer us) first
              (stream-output-index us) index
              (stream-output-length us) length)))))

(defmethod stream-flush-output ((stream multi-buffered-output-stream))
  (let ((queue (stream-output-buffer stream))
        (us (stream-underlying-stream stream)))
    (when (< 0 (the fixnum (queue-length queue)))
      (multi-buffer-output stream)
      (let ((r (stream-flush-output us)))
        (when (and (null r)
                   (= 0 (the fixnum (stream-output-length us))))
          (stream-discard-output-buffer us)
          (stream-read queue))
        (when (= 0 (the fixnum (queue-length queue)))
          (setf (stream-output-index stream) 0
                (stream-output-length stream) 0))
        r))))

(defun multi-buffered-output-stream (stream)
  (make-instance 'multi-buffered-output-stream :stream stream))
