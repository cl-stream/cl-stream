
(in-package :cl-stream)

(defmethod stream-close ((stream cl:stream))
  (cl:close stream))

(defmethod stream-element-type ((stream cl:stream))
  (cl:stream-element-type stream))

(defmethod stream-finish-output ((stream cl:stream))
  (cl:finish-output stream))

(defmethod stream-open-p ((stream cl:stream))
  (and (cl:open-stream-p stream) t))

(defmethod stream-read ((stream cl:stream))
  (let ((element-type (cl:stream-element-type stream)))
    (cond ((subtypep element-type 'unsigned-byte)
           (let ((byte (cl:read-byte stream nil +eof+)))
             (if (eq +eof+ byte)
                 (values nil :eof)
                 (values byte nil))))
          ((subtypep element-type 'character)
           (let ((char (cl:read-char stream nil +eof+ nil)))
             (if (eq +eof+ char)
                 (values nil :eof)
                 (values char nil))))
          (t
           (error "Unknown stream element type ~S" element-type)))))

(defmethod stream-read-sequence ((stream cl:stream) (seq sequence)
                                 &key (start 0) (end (length seq)))
  (let ((pos (cl:read-sequence seq stream :start start :end end)))
    (values (- pos start) nil)))

(defmethod stream-write ((stream cl:stream) (element integer))
  (cl:write-byte element stream)
  nil)

(defmethod stream-write ((stream cl:stream) (element character))
  (cl:write-char element stream)
  nil)

(defmethod stream-write-sequence ((stream cl:stream) (seq sequence)
                                  &key (start 0) (end (length seq)))
  (cl:write-sequence seq stream :start start :end end)
  (values (- end start) nil))
