
(in-package :cl-stream)

(defclass super-stream (stream)
  ((underlying-stream :initarg :stream
                      :reader stream-underlying-stream
                      :type stream)))

(defmethod stream-clear-output ((stream super-stream))
  (stream-clear-output (stream-underlying-stream stream)))

(defmethod stream-close ((stream super-stream))
  (stream-close (stream-underlying-stream stream)))

(defmethod stream-flush ((stream super-stream))
  (stream-flush (stream-underlying-stream stream)))

(defmethod stream-flush-output ((stream super-stream))
  (stream-flush-output (stream-underlying-stream stream)))

(defmethod stream-open-p ((stream super-stream))
  (stream-open-p (stream-underlying-stream stream)))
