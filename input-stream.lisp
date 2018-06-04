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

(defclass input-stream (stream)
  ()
  (:documentation "Subclass of STREAM supporting READ operations."))

(define-condition stream-input-error (stream-error)
  ()
  (:documentation "An error which is signalled when an input error
occurs on a stream."))

(defgeneric stream-discard-n (input-stream count)
  (:documentation "Discard COUNT items from INPUT-STREAM."))

(defgeneric stream-read (input-stream)
  (:documentation "Tries to read one element from STREAM.
Returns two values : the element or NIL if read failed;
and a state indicator which is
 NIL if read succeeded,
 :EOF if end of file was reached, or
 :NON-BLOCKING if read would block."))

(defgeneric stream-read-sequence (input-stream seq &key start end)
  (:documentation "Reads elements from INPUT-STREAM into SEQ
from START to END. Returns two values :
 the number of elements read, and
 a state indicator which is
  NIL if STREAM-READ-SEQUENCE succeeded
  :EOF if end of file was reached
  :NON-BLOCKING if read would block."))

(defgeneric stream-read-sequence-until (input-stream end-element seq
                                        &key start end)
  (:documentation "Reads elements from INPUT-STREAM into SEQ
from START to END until END-ELEMENT is read. Returns two values :
 the number of elements read, and
 a state indicator which is
  NIL if STREAM-READ-SEQUENCE-UNTIL succeeded
  :EOF if end of file was reached
  :NON-BLOCKING if read would block."))

(defgeneric stream-read-until (input-stream end-element)
  (:documentation "Reads elements from INPUT-STREAM from START to END
until END-ELEMENT is read. Returns two values :
 a sequence of elements read, and
 a state indicator which is
  NIL if READ-UNTIL succeeded
  :EOF if end of file was reached
  :NON-BLOCKING if read would block."))

(defmethod stream-discard-n (stream (count integer))
  (check-if-open stream)
  (loop
     (unless (< 0 count)
       (return (values 0 nil)))
     (multiple-value-bind (element state) (stream-read stream)
       (declare (ignore element))
       (case state
         ((nil) (decf count))
         ((:eof) (return (values count :eof)))
         ((:non-blocking) (return (values count :non-blocking)))
         (otherwise (error 'stream-input-error :stream stream))))))

(defmethod stream-read-sequence ((stream input-stream) seq
                                 &key (start 0) (end (length seq)))
  (check-if-open stream)
  (let ((count 0))
    (loop
       (when (= start end)
         (return))
       (multiple-value-bind (element state) (stream-read stream)
         (case state
           ((nil)
            (setf (aref seq start) element)
            (incf start)
            (incf count))
           ((:eof)
            (return (values count :eof)))
           ((:non-blocking)
            (return (values count :non-blocking)))
           (otherwise
            (error 'stream-input-error :stream stream)))))))

(defmethod stream-read-sequence-until ((stream input-stream)
                                       end-element seq
                                       &key (start 0) (end (length seq)))
  (check-if-open stream)
  (assert (typep end-element (stream-element-type stream)))
  (let ((count 0))
    (loop
       (when (= start end)
         (return))
       (multiple-value-bind (element state) (stream-read stream)
         (case state
           ((nil)
            (setf (aref seq start) element)
            (incf start)
            (incf count)
            (when (eq element end-element)
              (return (values count nil))))
           ((:eof)
            (return (values count :eof)))
           ((:non-blocking)
            (return (values count :non-blocking)))
           (otherwise
            (error 'stream-input-error :stream stream)))))))
