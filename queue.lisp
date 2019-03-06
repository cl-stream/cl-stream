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
  ((buffer-size :initarg :buffer-size
                :initform *stream-default-buffer-size*
                :accessor queue-buffer-size
                :type fixnum+)
   (buffer :type vector)
   (element-type :initarg :type
                 :initform t
                 :accessor stream-element-type)
   (length :initform 0
           :accessor queue-length
           :type fixnum+)
   (read-index :initform 0
               :accessor queue-read-index
               :type fixnum+)
   (write-index :initform 0
                :accessor queue-write-index
                :type fixnum+)))

(defgeneric make-queue-buffer (queue))
(defgeneric queue-buffer (queue))
(defgeneric queue-first (queue))
(defgeneric (setf queue-first) (value queue))

(defmethod make-queue-buffer ((queue queue))
  (let ((size (queue-buffer-size queue))
        (type (stream-element-type queue)))
    (make-array `(,size)
                :element-type type)))

(defmethod queue-buffer ((queue queue))
  (if (slot-boundp queue 'buffer)
      (slot-value queue 'buffer)
      (setf (slot-value queue 'buffer)
            (make-queue-buffer queue))))

(defmethod stream-blocking-p ((queue queue))
  nil)

(defmethod stream-read ((queue queue))
  (if (= 0 (the fixnum (queue-length queue)))
      (values nil :non-blocking)
      (let* ((buffer (the vector (queue-buffer queue)))
             (element (aref buffer (the fixnum
                                        (queue-read-index queue)))))
        (decf (the fixnum (queue-length queue)))
        (let ((index (incf (the fixnum+ (queue-read-index queue)))))
          (declare (type fixnum+ index))
          (unless (< index (the fixnum (queue-write-index queue)))
            (replace buffer buffer :start2 index)
            (setf (the fixnum (queue-read-index queue)) 0)
            (decf (the fixnum (queue-write-index queue)) index)))
        (values element nil))))

(defmethod stream-write ((queue queue) element)
  (let ((buffer (the vector (queue-buffer queue))))
    (incf (queue-length queue))
    (setf (aref buffer (queue-write-index queue)) element)
    (setf (queue-write-index queue)
          (mod (1+ (queue-write-index queue)) (length buffer))))
  nil)

(defmethod queue-first ((queue queue))
  (aref (queue-buffer queue) (queue-read-index queue)))

(defmethod (setf queue-first) (value (queue queue))
  (setf (aref (queue-buffer queue) (queue-read-index queue))
        value))

(defmethod queue-last ((queue queue))
  (let ((size (queue-buffer-size queue)))
    (when (< 0 (the fixnum (queue-length queue)))
      (aref (queue-buffer queue)
            (mod (+ size (1- (queue-write-index queue))) size)))))
