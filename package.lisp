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
   #:stream-close
   #:stream-element-type
   #:stream-error
   #:stream-error-stream
   #:stream-read
   #:stream-read-line
   #:stream-read-sequence
   #:stream-write
   #:stream-write-sequence
   #:string-output-stream
   #:with-input-from-string
   #:with-output-to-string
   #:write
   #:write-sequence)
  (:export
   #:buffered-input-stream
   #:buffered-output-stream
   #:close
   #:discard-stream-input-buffer
   #:discard-stream-output-buffer
   #:flush
   #:input-buffer
   #:input-stream
   #:io-stream
   #:make-stream-input-buffer
   #:make-stream-output-buffer
   #:multi-buffered-output-stream
   #:output-buffer
   #:output-stream
   #:queue
   #:queue-first
   #:queue-length
   #:read
   #:read-line
   #:read-sequence
   #:read-sequence-until
   #:read-until
   #:sequence-output-stream
   #:sequence-output-stream-reset
   #:sequence-output-stream-sequence
   #:shadowing-import-from
   #:*stdin*
   #:*stdout*
   #:*stderr*
   #:stdin
   #:stdout
   #:stderr
   #:stream
   #:stream-blocking-p
   #:*stream-default-buffer-size*
   #:stream-element-type
   #:stream-closed-error
   #:stream-end-error
   #:stream-error
   #:stream-error-stream
   #:stream-fill-input-buffer
   #:stream-flush
   #:stream-flush-output-buffer
   #:stream-input-buffer
   #:stream-input-buffer-size
   #:stream-input-error
   #:stream-input-index
   #:stream-input-length
   #:stream-open-p
   #:stream-output-buffer
   #:stream-output-buffer-size
   #:stream-output-error
   #:stream-output-index
   #:stream-output-length
   #:stream-read
   #:stream-read-element-from-buffer
   #:stream-read-line
   #:stream-read-sequence
   #:stream-read-sequence-until
   #:stream-read-until
   #:stream-underlying-stream
   #:stream-write
   #:stream-write-element-to-buffer
   #:stream-write-sequence
   #:string-output-stream
   #:string-output-stream-string
   #:with-input-from-sequence
   #:with-input-from-string
   #:with-output-to-sequence
   #:with-output-to-string
   #:with-stream
   #:write
   #:write-sequence))
