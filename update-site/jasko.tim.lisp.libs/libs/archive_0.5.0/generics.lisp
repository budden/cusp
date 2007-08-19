(in-package :archive)


;;; reading

(defgeneric read-entry-from-archive (archive)
  (:documentation "Return the next entry in ARCHIVE or NIL if there is no
next entry"))

(defgeneric extract-entry (archive entry)
  (:documentation "Recreate the file represented by ENTRY in ARCHIVE as
an actual file on disk.  This file is created relative to
*DEFAULT-PATHNAME-DEFAULTS*"))

(defgeneric discard-entry (archive entry)
  (:documentation "Advance ARCHIVE's internal state past the end of
ENTRY's data.  Further reads from ENTRY's stream will return EOF."))

(defgeneric transfer-entry-data-to-stream (archive entry stream)
  (:documentation "Write the data in ARCHIVE associated with ENTRY into
STREAM."))


;;; writing

(defgeneric create-entry-from-pathname (archive pathname)
  (:documentation "Create an ENTRY that can be written to ARCHIVE, using
metadata and the name of FILENAME."))

(defgeneric write-entry-to-archive (archive entry
                                            &key write-file-data)
  (:documentation "Write ENTRY and, if WRITE-FILE DATA is true,
its associated data into ARCHIVE.  The associated data for ENTRY
is expected to exist in the pathname returned by the expression
(NAME ENTRY)."))

(defgeneric write-entry-to-buffer (entry buffer &optional start)
  (:documentation "Write the information associated with ENTRY into BUFFER,
beginning at position START."))

(defgeneric finalize-archive (archive)
  (:documentation "Perform any necessary processing for finalizing ARCHIVE.
This function must be called prior to calling CLOSE-ARCHIVE."))

