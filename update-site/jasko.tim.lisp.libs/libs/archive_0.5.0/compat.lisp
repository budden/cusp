;;; compat.lisp -- compatibility wrappers for accessing Unix-y things

(in-package :archive)

(defconstant +permissions-mask+ 
  #+sbcl (logior sb-posix:s-irusr sb-posix:s-iwusr sb-posix:s-ixusr
                 sb-posix:s-irgrp sb-posix:s-iwgrp sb-posix:s-ixgrp
                 sb-posix:s-iroth sb-posix:s-iwoth sb-posix:s-ixoth)
  #-sbcl 511)

;;; CMUCL returns multiple values from UNIX:UNIX-STAT.  We need to
;;; package these up into something to which we can repeatedly reference.
#+cmucl
(defclass stat ()
  ((dev :initarg :dev :reader dev)
   (ino :initarg :ino :reader ino)
   (mode :initarg :mode :reader mode)
   (nlink :initarg :nlink :reader nlink)
   (uid :initarg :uid :reader uid)
   (gid :initarg :gid :reader gid)
   (rdev :initarg :rdev :reader rdev)
   (atime :initarg :atime :reader atime)
   (mtime :initarg :mtime :reader mtime)
   (ctime :initarg :ctime :reader ctime)
   (size :initarg :size :reader size)
   (blocks :initarg :blocks :reader blocks)
   (blksize :initarg :blksize :reader blksize)
   (flags :initarg :flags :reader flags)
   (gen :initarg :gen :reader gen)))

(defun stat (file)
  (let ((file (merge-pathnames file)))
    #+sbcl (sb-posix:stat file)
    #+lispworks (get-file-stat file)
    #+clisp (posix:file-stat file)
    #+cmucl (multiple-value-bind (successp dev ino mode nlink uid gid
                                           rdev atime mtime ctime size
                                           blocks blksize flags gen)
                (unix:unix-stat file)
              (unless successp
                (error "Could not get information on ~A" file))
              (make-instance 'stat
                             :dev dev :ino ino :mode mode :nlink nlink
                             :uid uid :gid gid :rdev rdev
                             :atime atime :mtime mtime :ctime ctime
                             :size size :blocks blocks :blksize blksize
                             :flags flags :gen gen))
    #-(or sbcl lispworks clisp cmucl) (error "Not implemented")))


;;; messing with stat modes
(defun stat-file-type (mode)
  (logand mode #o170000))

(defun isdir (mode)
  (= (stat-file-type mode) #o40000))

(defun isreg (mode)
  (= (stat-file-type mode) #o100000))

(defun islink (mode)
  (= (stat-file-type mode) #o0140000))

(defun ischarfile (mode)
  (= (stat-file-type mode) #o20000))

(defun isblockfile (mode)
  (= (stat-file-type mode) #o0060000))

(defun isfifo (mode)
  (= (stat-file-type mode) #o0010000))


;;; stat field accessors
(defun stat-mode (stat)
  #+sbcl (sb-posix::stat-mode stat)
  #+lispworks (file-stat-mode stat)
  #+clisp (posix:convert-mode (posix:file-stat-mode state))
  #+cmucl (mode stat)
  #-(or sbcl lispworks clisp cmucl) (error "Not implemented"))

(defun stat-uid (stat)
  #+sbcl (sb-posix::stat-uid stat)
  #+lispworks (file-stat-owner-id stat)
  #+clisp (posix:file-stat-uid stat)
  #+cmucl (uid stat)
  #-(or sbcl lispworks clisp cmucl) (error "Not implemented"))

(defun stat-gid (stat)
  #+sbcl (sb-posix::stat-gid stat)
  #+lispworks (file-stat-group-id stat)
  #+clisp (posix:file-stat-gid stat)
  #+cmucl (gid stat)
  #-(or sbcl lispworks clisp cmucl) (error "Not implemented"))

(defun stat-size (stat)
  #+sbcl (sb-posix::stat-size stat)
  #+lispworks (file-stat-size stat)
  #+clisp (posix:file-stat-size stat)
  #+cmucl (size stat)
  #-(or sbcl lispworks clisp cmucl) (error "Not implemented"))

(defun stat-mtime (stat)
  #+sbcl (sb-posix::stat-mtime stat)
  #+lispworks (file-stat-last-modify stat)
  #+clisp (posix:file-stat-mtime stat)
  #+cmucl (mtime state)
  #-(or sbcl lispworks clisp cmucl) (error "Not implemented"))

(defun stat-ino (stat)
  #+sbcl (sb-posix::stat-ino stat)
  #+lispworks (file-stat-inode stat)
  #+clisp (posix:file-stat-ino stat)
  #+cmucl (ino stat)
  #-(or sbcl lispworks clisp cmucl) (error "Not implemented"))

(defun stat-nlink (stat)
  #+sbcl (sb-posix::stat-nlink stat)
  #+lispworks (file-stat-links stat)
  #+clisp (posix:file-stat-nlink stat)
  #+cmucl (nlink stat)
  #-(or sbcl lispworks clisp cmucl) (error "Not implemented"))
