;;;; archive.lisp -- common handling for archive files

(in-package :archive)

(defclass archive ()
  ((entry-buffer :initarg :entry-buffer :reader entry-buffer
                 :type (simple-array (unsigned-byte 8) (*)))
   (file-buffer :initform (make-array 8192 :element-type '(unsigned-byte 8))
                :reader file-buffer
                :type (simple-array (unsigned-byte 8) (*)))
   (bytes-output :initform 0 :accessor bytes-output)
   (open-archive-p :initform t :accessor open-archive-p)
   (stream :initarg :stream :reader archive-stream :type stream)))

(defun initialize-entry-buffer (archive buffer-length)
  "Initialize the ENTRY-BUFFER of ARCHIVE."
  (setf (slot-value archive 'entry-buffer)
        (make-array buffer-length :element-type '(unsigned-byte 8))))

(defun open-archive (archive-type stream)
  "Return an archive.  STREAM is the underlying Lisp stream for the archive.
STREAM should not be read from or written to anymore."
  (cond
    ((or (equal (stream-element-type stream) '(unsigned-byte 8))
         ;; OpenMCL
         (equal (stream-element-type stream) '(integer 0 255)))
     (make-instance archive-type :stream stream))
    (t
     (error "The element type of ~A is not (UNSIGNED-BYTE 8)" stream))))

(defun close-archive (archive)
  "Closes the stream associated with ARCHIVE and the archive itself.
Further operations on the archive are undefined."
  (when (open-archive-p archive)
    (close (archive-stream archive))
    (setf (open-archive-p archive) nil))
  t)

(defun read-entry-block (archive)
  (with-slots (entry-buffer stream) archive
    (let ((nbytes (read-sequence entry-buffer stream)))
      (unless (= nbytes (length entry-buffer))
        (error "Corrupt archive"))
      entry-buffer)))

(defun read-data-block (archive block-length &optional (pad-func #'identity))
  "Read a (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) array of BLOCK-LENGTH
from ARCHIVE.  BLOCK-LENGTH is padded with PAD-FUNC to meet archive
requirements about alignment."
  (let ((length (funcall pad-func block-length)))
    (with-slots (file-buffer stream) archive
      (when (> length (length file-buffer))
        (let ((new-buffer (make-array (* (length file-buffer) 2)
                                      :element-type '(unsigned-byte 8))))
          (setf file-buffer new-buffer)))
      (read-sequence file-buffer stream :end length)
      (subseq file-buffer 0 block-length))))

(defun write-data-block (archive block start &optional end)
  (write-sequence block (archive-stream archive)
                  :start start :end (or end (length block))))


;;; providing streamy access for an entry
(defun make-stream-for-entry (archive entry)
  (let ((bounded-stream (make-bounded-stream (archive-stream archive)
                                             (size entry))))
    (flexi-streams:make-flexi-stream bounded-stream)))

(defmethod read-entry-from-archive :around (archive)
  (let ((entry (call-next-method)))
    (when entry
      (setf (slot-value entry 'stream)
            (make-stream-for-entry archive entry)))
    entry))

(defun entry-stream (entry &key external-format)
  "Return a stream connected to the data of ENTRY.  EXTERNAL-FORMAT is
either a name for a FLEXI-STREAMS external format or a FLEXI-STREAMS
external-format object."
  (let ((stream (slot-value entry 'stream)))
    (when external-format
      (setf (flexi-streams:flexi-stream-external-format stream)
            external-format))
    stream))


;;; doing interesting things with entries

(defun discard-unused-entry-data (archive entry rounding-function)
  (transfer-entry-data-to-stream* archive entry nil rounding-function))

(defun transfer-entry-data-to-stream* (archive entry stream rounding-function)
  (when (data-discarded-p entry)
    ;; by definition, there's nothing left
    (return-from transfer-entry-data-to-stream* (values)))
  (let* ((entry-stream (entry-stream entry))
         (wrapped-stream (flexi-streams:flexi-stream-stream entry-stream))
         (n-bytes-remaining (n-bytes-remaining wrapped-stream))
         (rounded-size (funcall rounding-function (size entry)))
         (rounded-n-bytes-remaining (- rounded-size
                                       (- (size entry) n-bytes-remaining))))
    (loop with archive-stream = (archive-stream archive)
          with buffer = (file-buffer archive)
          for bytes-read = (read-sequence buffer archive-stream
                                          :start 0
                                          :end (min (length buffer)
                                                    rounded-n-bytes-remaining))
          do (assert (not (minusp n-bytes-remaining)))
             (decf rounded-n-bytes-remaining bytes-read)
             ;; flush to the other stream
             (when stream
               (write-sequence buffer stream :start 0
                               :end (min n-bytes-remaining bytes-read)))
             (decf n-bytes-remaining bytes-read)
          while (plusp rounded-n-bytes-remaining)
          finally (progn
                    ;; make sure we didn't overrun the data of the entry
                    (assert (zerop rounded-n-bytes-remaining))
                    ;; make sure nobody can read from the entry's stream
                    (setf (n-bytes-remaining wrapped-stream) 0)
                    ;; indicate that we've already discarded the data
                    (setf (data-discarded-p entry) t)
                    (values)))))

(defun extract-files-from-archive (archive &optional (filter (constantly t)))
  (do-archive-entries (entry archive)
    (if (funcall filter (name entry))
        (extract-entry archive entry)
        (discard-entry archive entry))))

(defun extract-files-from-pathname (pathname &optional (filter (constantly t)))
  (with-open-archive (archive pathname :direction :input)
    (extract-files-from-archive archive filter)))
