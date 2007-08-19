(in-package :archive)

(fli:define-foreign-function (c-stat "_stat")
    ((path :pointer)
     (struct-buf :pointer))
  :result-type :int)

;;FIXME: I don't think all this info is correct
;;       it might be better to define all the sub structures like dev_t
(fli:define-c-struct _stat
  (dev :unsigned-long)
  (ino :short)
  (mode :unsigned-short)
  (nlink :short)
  (uid :short)
  (gid :short)
  (rdev :int)
  (size :long)
  (atime :long); ??
  (mtime :long); ??
  (ctime :long); ??
  (blksize :long)
  (blocks :long)
  (attr :long))

(defstruct file-stat
  inode device owner-id group-id size blocks mode last-access last-change
  last-modify links device-type)

(defun convert-to-lisp-struct (stat)
  (fli:with-foreign-slots (dev ino mode nlink uid gid rdev size atime mtime ctime blksize blocks attr)
      stat
    (make-file-stat :inode ino :device dev :owner-id uid :group-id gid :size size :blocks blocks
                    :mode mode :last-access atime :last-change ctime :last-modify mtime
                    :links nlink :device-type rdev)))

(defun file-stat (file)
  (when (probe-file file)
    (fli:with-dynamic-foreign-objects ()
      (let ((stat (fli:allocate-dynamic-foreign-object :type '_stat)))
        (c-stat (fli:convert-to-foreign-string (namestring file)) stat)
        (convert-to-lisp-struct stat)))))

;; EOF
