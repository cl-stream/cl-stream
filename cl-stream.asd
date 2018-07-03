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

(in-package :common-lisp-user)

(defpackage :cl-stream.system
  (:use :common-lisp :asdf))

(in-package :cl-stream.system)

(defsystem :cl-stream
  :name "cl-stream"
  :author "Thomas de Grivel <thoxdg@gmail.com>"
  :version "0.1"
  :description "Stream classes for Common Lisp"
  :components
  ((:file "buffered-input-stream" :depends-on ("input-stream"))
   (:file "buffered-io-stream" :depends-on ("buffered-input-stream"
                                            "buffered-output-stream"
                                            "io-stream"))
   (:file "buffered-output-stream" :depends-on ("output-stream"))
   (:file "character-stream" :depends-on ("stream"))
   (:file "cl-stream" :depends-on ("buffered-io-stream"))
   (:file "conditions" :depends-on ("package"))
   (:file "input-stream" :depends-on ("stream"))
   (:file "input-stream-1" :depends-on ("vector-output-stream"))
   (:file "io-stream" :depends-on ("input-stream" "output-stream"))
   (:file "line-tracking-stream" :depends-on ("character-stream"
                                              "input-stream"
                                              "super-stream"))
   (:file "misc" :depends-on ("package"))
   (:file "multi-buffered-output-stream"
          :depends-on ("buffered-output-stream"))
   (:file "output-stream" :depends-on ("stream"))
   (:file "package")
   (:file "queue" :depends-on ("io-stream"))
   (:file "sequence-input-stream" :depends-on ("buffered-input-stream"))
   (:file "sequence-output-stream"
          :depends-on ("buffered-output-stream"))
   (:file "stdio" :depends-on ("io-stream"))
   (:file "stream" :depends-on ("package" "misc"))
   (:file "string-output-stream"
          :depends-on ("sequence-output-stream"))
   (:file "super-stream" :depends-on ("buffered-output-stream"))
   (:file "ub8-stream" :depends-on ("stream"))
   (:file "vector-output-stream" :depends-on ("buffered-output-stream"))))
