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

(defun close (stream)
  (stream-close stream))

(defun flush (&optional (stream (stdout)))
  (stream-flush stream))

(defun read (&optional (stream (stdin))
               (eof-error-p t)
               (eof-value nil)
               (recursive-p nil))
  (declare (ignore recursive-p))
  (multiple-value-bind (element state) (stream-read stream)
    (ecase state
      ((nil) (values element state))
      ((:eof) (if eof-error-p
                  (error 'stream-end-error)
                  (values eof-value state)))
      ((:non-blocking) (values element state))
      (t (error 'stream-error :stream stream)))))

(defun read-sequence (seq &key (stream (stdin))
                            (start 0)
                            (end (length seq)))
  (stream-read-sequence stream seq :start start :end end))

(defun read-sequence-until (end-element seq &key (stream (stdin))
                                              (start 0)
                                              (end (length seq)))
  (stream-read-sequence-until stream end-element seq :start start
                              :end end))

(defun read-until (end-element &optional (stream (stdin)))
  (stream-read-until stream end-element))

(defun read-line (&optional (stream (stdin)))
  (stream-read-line stream))

(defvar *stderr*
  *error-output*)

(defvar *stdin*
  *standard-input*)

(defvar *stdout*
  *standard-output*)

(defun write (element &optional (stream *stdout*))
  "Tries to write one element to STREAM.
Returns a state indicator which is
 NIL if write succeeded,
 :EOF if end of file was reached, or
 :NON-BLOCKING if write would block."
  (stream-write stream element))

(defun write-sequence (seq &key (stream *stdout*) (start 0)
                             (end (length seq)))
  (stream-write-sequence stream seq :start start :end end))

(defun write-string (string &key (stream *stdout*) (start 0)
                              (end (length string)))
  (declare (type string string))
  (stream-write-sequence stream string :start start :end end))
