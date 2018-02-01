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

(defclass queue (io-stream)
  ((extend-by :initarg :extend-by
              :initform *stream-default-buffer-size*
              :accessor queue-extend-by
              :type fixnum+)
   (buffer :reader queue-buffer
           :type array)
   (length :initform 0
           :accessor queue-length
           :type fixnum+)
   (read-index :initform 0
               :accessor queue-read-index
               :type fixnum+)
   (write-index :initform 0
                :accessor queue-write-index
                :type fixnum+)))

(defgeneric queue-first (queue))
(defgeneric (setf queue-first) (value queue))

(defmethod stream-blocking-p ((queue queue))
  nil)

(defmethod stream-element-type ((queue queue))
  (array-element-type (queue-buffer queue)))

(defmethod initialize-instance ((queue queue) &rest initargs
                                &key (element-type t)
                                  (size *stream-default-buffer-size*))
  (declare (ignore initargs))
  (call-next-method)
  (let ((buffer (make-array `(,size) :element-type element-type)))
    (setf (slot-value queue 'buffer) buffer
          (queue-extend-by queue) size))
  queue)

(defmethod stream-read ((queue queue))
  (let ((buffer (queue-buffer queue))
        (read-index (queue-read-index queue)))
    (cond ((= 0 (queue-length queue))
           (values nil :non-blocking))
          (t
           (let ((element (aref buffer read-index)))
             (decf (queue-length queue))
             (incf (queue-read-index queue))
             (when (= (queue-read-index queue) (length buffer))
               (setf (queue-read-index queue) 0))
             (values element nil))))))

(defmethod stream-write ((queue queue) element)
  (let ((buffer (queue-buffer queue)))
    (let ((length (length buffer)))
      (when (= (queue-length queue) length)
        (let ((new-length (+ length (queue-extend-by queue))))
          (adjust-array buffer `(,new-length))
          (let ((n (- length (queue-write-index queue))))
            (dotimes (i n)
              (setf (aref buffer (+ (- new-length n) i))
                    (aref buffer (+ (queue-write-index queue) i))))
            (setf (queue-read-index queue) (- new-length n))))))
    (incf (queue-length queue))
    (setf (aref buffer (queue-write-index queue)) element)
    (incf (queue-write-index queue))
    (when (= (queue-write-index queue) (length buffer))
      (setf (queue-write-index queue) 0))
    nil))

(defmethod queue-first ((queue queue))
  (aref (queue-buffer queue) (queue-read-index queue)))

(defmethod (setf queue-first) (value (queue queue))
  (setf (aref (queue-buffer queue) (queue-read-index queue))
        value))
