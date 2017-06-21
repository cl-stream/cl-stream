;;
;;  cl-stream  -  Stream class for Common Lisp
;;
;;  Copyright 2017 Thomas de Grivel <thoxdg@gmail.com>
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

(in-package :common-lisp)

(defpackage :cl-stream
  (:nicknames :stream)
  (:use
   :common-lisp)
  (:shadow
   #:close
   #:input-stream
   #:open-stream-p
   #:output-stream
   #:read
   #:read-line
   #:read-sequence
   #:stream
   #:stream-element-type
   #:stream-error
   #:stream-error-stream
   #:with-input-from-string
   #:with-output-to-string
   #:write
   #:write-sequence)
  (:export
   #:buffered-input-stream
   #:buffered-output-stream
   #:close
   #:*default-buffer-size*
   #:flush
   #:input-buffer
   #:input-stream
   #:io-stream
   #:make-stream-input-buffer
   #:make-stream-output-buffer
   #:output-buffer
   #:output-stream
   #:read
   #:read-line
   #:read-sequence
   #:read-sequence-until
   #:read-until
   #:sequence-output-stream
   #:sequence-output-stream-reset
   #:sequence-output-stream-sequence
   #:shadowing-import-from
   #:stream
   #:stream-blocking-p
   #:stream-element-type
   #:stream-closed-error
   #:stream-error
   #:stream-error-stream
   #:stream-fill-input-buffer
   #:stream-flush-output-buffer
   #:stream-input-buffer
   #:stream-input-buffer-size
   #:stream-input-index
   #:stream-input-length
   #:stream-output-buffer
   #:stream-output-buffer-size
   #:stream-output-index
   #:stream-output-length
   #:stream-read-element-from-buffer
   #:stream-write-element-to-buffer
   #:with-input-from-sequence
   #:with-input-from-string
   #:with-stream
   #:write
   #:write-sequence))
