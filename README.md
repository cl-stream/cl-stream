# cl-stream

Common Lisp ticket to evented I/O on any kind of data.

cl-stream streams support any kind of data.
READ and WRITE both operate on exactly one stream element of type
(STREAM-ELEMENT-TYPE stream). There is no READ-BYTE or READ-CHAR type
specific read methods. Actual operations are implemented by specializing
STREAM-READ and STREAM-WRITE.

cl-stream streams support reading and writing to sequences of elements
with READ-SEQUENCE and WRITE-SEQUENCE. Actual operations are implemented
by specializing STREAM-READ-SEQUENCE and STREAM-WRITE-SEQUENCE.

cl-stream streams support non-blocking I/O through the setf-able place
(STREAM-BLOCKING-P stream). Actual operations are implemented by
specializing (SETF STREAM-BLOCKING-P).

cl-stream streams provide a new stream API which can be used on all
common lisp streams too (see cl-stream.lisp).

cl-stream streams can be used as gray-streams by using the steam-gray
system.

## Abstract stream classes

### Class: stream
Base class for all streams.

#### Generic: stream-element-type *stream* => *type*
Returns the type of elements of *stream*.

#### Condition: stream-error
Superclass for all errors related to streams.

#### Condition: stream-closed-error
An error that is signalled when trying to read from
or write to a closed stream.

#### Generic: check-if-open *stream* => nil
Checks if STREAM is open and signals an error otherwise.

#### Generic: close *stream*
Prevents further read and write operations on STREAM
causing them to raise STREAM-CLOSED-ERROR.

#### Macro: with (*var* *stream-class* &rest *initargs* &key &allow-other-keys) &body *body*
Instantiates a stream and ensures it gets closed returning from BODY.

### Class: input-stream
Subclass of STREAM supporting READ operations.

#### Condition: stream-input-error
An error which is signalled when an input error occurs on a stream.

#### Generic: read *input-stream* => *element* *state-indicator*
Tries to read one element from STREAM.
Returns two values : the element or NIL if read failed;
and a state indicator which is
 NIL if read succeeded,
 :EOF if end of file was reached, or
 :NON-BLOCKING if read would block.

#### Generic: read-sequence *input-stream* *seq* &key *start* *end*
Reads elements from INPUT-STREAM into SEQ
from START to END. Returns two values :
 the number of elements read, and
 a state indicator which is
  NIL if READ-SEQUENCE succeeded
  :EOF if end of file was reached
  :NON-BLOCKING if read would block.

#### Generic: read-sequence-until *input-stream* *end-element* *seq* &key *start* *end*
Reads elements from INPUT-STREAM into SEQ
from START to END until END-ELEMENT is read. Returns two values :
 the number of elements read, and
 a state indicator which is
  NIL if READ-SEQUENCE-UNTIL succeeded
  :EOF if end of file was reached
  :NON-BLOCKING if read would block.

#### Generic: read-until *input-stream* *end-element* => *sequence*
Reads elements from INPUT-STREAM from START to END
until END-ELEMENT is read. Returns two values :
 a sequence of elements read, and
 a state indicator which is
  NIL if READ-UNTIL succeeded
  :EOF if end of file was reached
  :NON-BLOCKING if read would block.

### Class: output-stream
Subclass of STREAM supporting WRITE operations.

#### Condition: stream-output-error
An error which is signalled when an output error
occurs on a stream.

#### Generic: write *output-stream* *element*
Tries to write one element to STREAM.
Returns a state indicator which is NIL if write succeeded,
:EOF if end of file was reached, or
:NON-BLOCKING if write would block.

## Buffered stream classes

### Special variable: \*default-buffer-size\*

### Type: fixnum+
A positive fixnum which may be 0.

### Class: buffered-input-stream

#### Generic: make-stream-input-buffer *buffered-input-stream* => *buffer*
Returns a new input buffer for stream.

#### Generic: stream-input-buffer *buffered-input-stream* => *buffer*
Returns the stream input buffer, calling
MAKE-STREAM-INPUT-BUFFER to create it if needed.

#### Generic: stream-fill-input-buffer *buffered-input-stream* => *state*
Fills the stream input buffer.
Returns NIL if successful, or
:EOF if end of file was reached, or
:NON-BLOCKING if operation would block.

### Class: buffered-output-stream
An output stream that buffers its writes until it gets flushed.

#### Generic: make-stream-output-buffer *buffered-output-stream* => *output-buffer*
Returns a new output buffer for BUFFERED-OUTPUT-STREAM.

#### Generic: stream-output-buffer *buffered-output-stream* => *output-buffer*
Returns the stream output buffer, calling
MAKE-STREAM-OUTPUT-BUFFER to create it if needed.

#### Generic: stream-flush-output-buffer *buffered-output-stream* => *state*
Tries once to flush the stream output buffer. Returns :
 NIL if successful, or
 :EOF if end of file was reached, or
 :NON-BLOCKING if operation would block.

#### Generic: flush *buffered-output-stream* => *state*
Flushes the output buffer of BUFFERED-OUTPUT-STREAM by
repeatedly calling STREAM-FLUSH-OUTPUT-BUFFER until empty. Returns
 NIL if output buffer was empty or emptied, or
 :EOF if end of file was reached, or
 :NON-BLOCKING if write would block.

### Class: sequence-input-stream
A buffered input stream that reads from a sequence.

#### Macro: with-input-from-sequence
Binds VAR to a new sequence input stream reading from SEQUENCE.
The stream is closed after BODY returns normally or before it is
aborted by a control transfer of some kind.

#### Macro: with-input-from-string
Binds VAR to a new sequence input stream reading from STRING.
The stream is closed after BODY returns normally or before it is
aborted by a control transfer of some kind.

### Class: sequence-output-stream
A buffered output stream that writes to a sequence.

#### Generic: sequence-output-stream-sequence *sequence-output-stream* => *sequence*
Returns the sequence that was written to SEQUENCE-OUTPUT-STREAM.

#### Macro: with-output-to-sequence (*var* *element-type*) &body *body*
Binds VAR to a new sequence output stream with element-type
ELEMENT-TYPE. Returns the sequence output stream sequence if
BODY returns normally. The stream is closed after BODY returns
normally or before it is aborted by a control transfer of some kind.

#### Macro: with-output-to-string (*var*) &body *body*
Binds VAR to a new sequence output stream with element-type
character. Returns the sequence output stream string if
BODY returns normally. The stream is closed after BODY returns
normally or before it is aborted by a control transfer of some kind.
