;;;; $Id: lispworks.lisp 294 2007-09-17 19:50:34Z ehuelsmann $
;;;; $URL: svn+ssh://ehuelsmann@common-lisp.net/project/usocket/svn/usocket/tags/0.3.7/backend/lispworks.lisp $

;;;; See LICENSE for licensing information.

(in-package :usocket)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "comm"))

#+win32
(fli:register-module "ws2_32")

(fli:define-foreign-function (get-host-name-internal "gethostname" :source)
      ((return-string (:reference-return (:ef-mb-string :limit 257)))
       (namelen :int))
      :lambda-list (&aux (namelen 256) return-string)
      :result-type :int
      #+win32 :module #+win32 "ws2_32")

(defun get-host-name ()
  (multiple-value-bind (retcode name)
      (get-host-name-internal)
    (when (= 0 retcode)
      name)))

#+win32
(defun remap-maybe-for-win32 (z)
  (mapcar #'(lambda (x)
              (cons (mapcar #'(lambda (y)
                                (+ 10000 y))
                            (car x))
                    (cdr x)))
          z))

(defparameter +lispworks-error-map+
  #+win32
  (append (remap-maybe-for-win32 +unix-errno-condition-map+)
          (remap-maybe-for-win32 +unix-errno-error-map+))
  #-win32
  (append +unix-errno-condition-map+
          +unix-errno-error-map+))



(defun handle-condition (condition &optional (socket nil))
  "Dispatch correct usocket condition."
  (typecase condition
    (simple-error (destructuring-bind (&optional host port err-msg errno)
                      (simple-condition-format-arguments condition)
                    (declare (ignore host port err-msg))
                    (let* ((usock-err
                            (cdr (assoc errno +lispworks-error-map+
                                        :test #'member))))
                      (if usock-err
                          (if (subtypep usock-err 'error)
                              (error usock-err :socket socket)
                            (signal usock-err :socket socket))
                        (error 'unknown-error
                               :socket socket
                               :real-error condition)))))))

(defun socket-connect (host port &key (element-type 'base-char))
  (let ((hostname (host-to-hostname host))
        (stream))
    (setf stream
          (with-mapped-conditions ()
             (comm:open-tcp-stream hostname port
                                   :element-type element-type)))
    (if stream
        (make-stream-socket :socket (comm:socket-stream-socket stream)
                            :stream stream)
      (error 'unknown-error))))

(defun socket-listen (host port
                           &key reuseaddress
                           (reuse-address nil reuse-address-supplied-p)
                           (backlog 5)
                           (element-type 'base-char))
  (let* ((reuseaddress (if reuse-address-supplied-p reuse-address reuseaddress))
         (comm::*use_so_reuseaddr* reuseaddress)
         (hostname (host-to-hostname host))
         (sock (with-mapped-conditions ()
                  #-lispworks4.1 (comm::create-tcp-socket-for-service
                                  port :address hostname :backlog backlog)
                  #+lispworks4.1 (comm::create-tcp-socket-for-service port))))
    (make-stream-server-socket sock :element-type element-type)))

(defmethod socket-accept ((usocket stream-server-usocket) &key element-type)
  (let* ((sock (with-mapped-conditions (usocket)
                  (comm::get-fd-from-socket (socket usocket))))
         (stream (make-instance 'comm:socket-stream
                                :socket sock
                                :direction :io
                                :element-type (or element-type
                                                  (element-type usocket)))))
    (make-stream-socket :socket sock :stream stream)))

;; Sockets and their streams are different objects
;; close the stream in order to make sure buffers
;; are correctly flushed and the socket closed.
(defmethod socket-close ((usocket stream-usocket))
  "Close socket."
  (close (socket-stream usocket)))

(defmethod socket-close ((usocket usocket))
  (with-mapped-conditions (usocket)
     (comm::close-socket (socket usocket))))

(defmethod get-local-name ((usocket usocket))
  (multiple-value-bind
      (address port)
      (comm:get-socket-address (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-peer-name ((usocket stream-usocket))
  (multiple-value-bind
      (address port)
      (comm:get-socket-peer-address (socket usocket))
    (values (hbo-to-vector-quad address) port)))

(defmethod get-local-address ((usocket usocket))
  (nth-value 0 (get-local-name usocket)))

(defmethod get-peer-address ((usocket stream-usocket))
  (nth-value 0 (get-peer-name usocket)))

(defmethod get-local-port ((usocket usocket))
  (nth-value 1 (get-local-name usocket)))

(defmethod get-peer-port ((usocket stream-usocket))
  (nth-value 1 (get-peer-name usocket)))

(defun get-hosts-by-name (name)
  (with-mapped-conditions ()
     (mapcar #'hbo-to-vector-quad
             (comm:get-host-entry name :fields '(:addresses)))))
