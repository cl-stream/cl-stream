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

(defclass output-stream (stream)
  ()
  (:documentation "Subclass of STREAM supporting STREAM-WRITE operation."))

(define-condition stream-output-error (stream-error)
  ()
  (:documentation "An error which is signalled when an output error
occurs on a stream."))

(defgeneric stream-flush (stream)
  (:documentation "Ensure that any output buffer is fully transmitted.
Returns a state indicator which is NIL if flush succeeded,
:EOF if end of file was reached, or
:NON-BLOCKING if flush would block."))

(defgeneric stream-write (output-stream element)
  (:documentation "Tries to write one element to STREAM.
Returns a state indicator which is NIL if write succeeded,
:EOF if end of file was reached, or
:NON-BLOCKING if write would block."))

(defgeneric stream-write-sequence (output-stream seq
                                   &key start end)
  (:documentation "Writes elements from SEQ from START to END
to OUTPUT-STREAM. Returns two values :
 the number of elements written, and
 a state indicator which is
  NIL if WRITE-SEQUENCE succeeded
  :EOF if end of file was reached
  :NON-BLOCKING if write would block."))

(defmethod stream-flush ((stream output-stream))
  nil)

(defmethod stream-write-sequence ((stream output-stream)
                                  (seq sequence)
                                  &key start end)
  (check-if-open stream)
  (setf start (or start 0))
  (setf end (or end (length seq)))
  (let* ((count 0)
         (state
          (loop
             (unless (< (the fixnum start) (the fixnum end))
               (return))
             (let ((state (stream-write stream (aref seq start))))
               (case state
                 ((nil)
                  (incf (the fixnum start))
                  (incf (the fixnum count)))
                 ((:eof)
                  (return :eof))
                 ((:non-blocking)
                  (return :non-blocking))
                 (otherwise
                  (error 'stream-output-error :stream stream)))))))
    (when state
      (values count state))))
