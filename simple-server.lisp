(use-package 'sb-bsd-sockets)

(defvar *bind-address* (make-inet-address "127.0.0.1"))
(defvar *server-socket* (make-inet-socket :stream :tcp))
(defvar *port* 1299)

(defun accept-n-recv(server &optional (recvsize 65536))
  (let* ((sender (socket-accept server))
	 (info (socket-receive sender nil recvsize)))
    (progn 
      (socket-close sender)
      info)))

(defun start-server()
  (progn
    (socket-bind *server-socket* *bind-address* *port*)
    (socket-listen *server-socket* 5)))



