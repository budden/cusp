(defpackage :archive
  (:use :cl  #+(and lispworks (not win32)) :sys)
  (:export #:archive                    ; type

           ;; creating
           #:open-archive #:close-archive

           ;; entry slot readers
           #:name
           #:entry-stream

           ;; reading archives
           #:read-entry-from-archive
           #:extract-entry
           #:discard-entry

           ;; writing archives
           #:create-entry-from-pathname
           #:write-entry-to-archive #:finalize-archive

           ;; convenience macros
           #:do-archive-entries #:with-open-archive

           ;; external support
           #:*bytevec-to-string-conversion-function*
           #:*string-to-bytevec-conversion-function*))
