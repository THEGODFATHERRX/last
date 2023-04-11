(ql:quickload "usocket")



(defun send-text-to-socket (text socket)
  (let ((socket-stream (usocket:socket-stream socket)))
    (format
      socket-stream
      (format nil "~a~%" text))  ; adding a line break at the end for prettiness
    (force-output socket-stream)))


(defun logger (text &rest args)
  "Simple wrapper around format func to simplify logging"
  (apply 'format (append (list t (concatenate 'string text "~%")) args)))


(defun close-socket (socket)
  "Close a socket without raising an error if something goes wrong"
  (handler-case
      (usocket:socket-close socket)
    (error (e)
      (logger "ignoring the error that happened while trying to close socket: ~a" e)))
  (logger "socket closed"))


(defun evaluate_code (msg) 
  (eval. (read-from-string msg)))

(defun process-client-socket (client-socket)
  "Process client socket that got some activity"
  ;; NOTE: read-line blocks until end-of-line character is received
  ;; see http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
  ;; for read-byte-at-a-time solution
  (let ((message (read-line (usocket:socket-stream client-socket))))
    (logger "got a message: ~a" message)
  ; (send-text-to-socket message client-socket) ))
    (send-text-to-socket (eval. (read-from-string message) 'user) client-socket) ))

(defun run-tcp-server (host port)
  "Run TCP server in a loop, listening to incoming connections.
  This is single-threaded version. Better approach would be to run
  process-client-socket in a separate thread every time there is activity
  on the client socket.
  All client sockets are kept in all-sockets list."
  (let* ((master-socket (usocket:socket-listen host port :backlog 256))
         (all-sockets `(,master-socket)))
    (loop
      (loop for sock in (usocket:wait-for-input all-sockets :ready-only t)
            do (if (eq sock master-socket)
                 ; new connection initiated
                 (let ((client-socket
                         (usocket:socket-accept master-socket :element-type 'character)))
                   (push client-socket all-sockets)
                   (logger "new socket initiated: ~a" client-socket))
                 ; client socket activity
                 (handler-case
                   (process-client-socket sock)
                   (t (e)
                      (logger "error during processing ~a" e)
                      (setf all-sockets (delete sock all-sockets))
                      (close-socket sock)) ))))))


(defun run-server-in-thread (host port)
  "Run TCP server in a separate thread"
  (let ((thread-name (format nil "tcp-server")))
    (logger "starting tcp server in a separate thread '~a'" thread-name)
    (sb-thread:make-thread
      (lambda () (run-tcp-server host port))
      :name thread-name)))


(defun main (&rest argv)
  (declare (ignorable argv))
  (sb-thread:join-thread 
    (run-server-in-thread "0.0.0.0" 12324))
    :default nil)

;---- hash table
;(defparameter *everything* (make-hash-table))
(defparameter *file-name* "~/Code/SKET/UNIVERSE/EVERYTHING" )
;(setf (gethash 'flower *everything*) (make-hash-table))
;(setf (gethash 'user *everything*) (make-hash-table))

(defun save-file ()
 (defparameter *p-everything* '())
 
 (loop for key1 being the hash-keys of *everything*
         using (hash-value value1)
         :do
         (print key1)
         (terpri)
         (setf (getf *p-everything* key1) '())
         (loop for key2 being the hash-keys of value1
           using (hash-value value2)
           :do
           (format t "~S ~S ~%" key2 value2)
           (setf (getf (getf *p-everything* key1) key2) value2)))

 (with-open-file (stream *file-name* :direction :io)
 (format stream "~s" *p-everything*)))


(defun load-file ()
 (defparameter *everything* (make-hash-table))
 (defparameter *p-everything* '())
 
 (with-open-file (stream *file-name* :direction :input)
 (setf *p-everything* (read stream)))
 
 (loop for (key1 value1) on *p-everything* by #'cddr
                do (setf (gethash key1 *everything*) (make-hash-table))
                   (loop for (key2 value2) on value1 by #'cddr
                    do (setf (gethash key2 (gethash key1 *everything*)) value2))))

(defun print-universe () (loop for key1 being the hash-keys of *everything*
        using (hash-value value1)
        :do
        (print key1)
        (terpri)
        (loop for key2 being the hash-keys of value1
          using (hash-value value2)
          :do
          (format t "~S ~S ~%" key2 value2))))



; -- game eval
(defun eval-list (lst &optional caller)
  (if (null lst)
      nil
    (cons (eval. (first lst) caller) (eval-list (rest lst) caller))))

(defun cond-eval (clauses &optional caller)
  (cond ((eval. (first (first clauses)) caller) (eval. (second (first clauses)) caller))
        (t (cond-eval (rest clauses) caller))))

(defun apply. (object method &rest args)
  (apply. (get object method) caller args))
;((eq (first expr) 'apply) (apply (eval (second expr) caller) (eval (third expr) caller) (eval-list (cdr (cdr (cdr expr)))) caller))

(defun eval. (expr caller)
  (cond ((atom expr) expr)
        ((eq (first expr) 'quote) (second expr))
        ((eq (first expr) 'atom) (atom (eval. (second expr) caller)))
        ((eq (first expr) 'eq) (eq (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'first) (car (eval. (second expr) caller)))
        ((eq (first expr) 'rest) (cdr (eval. (second expr) caller)))
        ((eq (first expr) 'cons) (cons (eval. (second expr) caller) (eval. (third expr) caller)))

        ((eq (first expr) 'cond) (cond-eval (cdr expr) caller))
        ((eq (first expr) 'begin) (last (mapcar (lambda (e) (eval. e caller)) (cdr expr))))

        ((eq (first expr) 'get) (get1 caller (eval. (second expr) caller)))
        ((eq (first expr) 'set) (set1 caller (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'run) (run1 caller  (eval. (second expr) caller) (mapcar (lambda (e) (eval. e caller)) (rest (rest expr)))))
        ((eq (first expr) 'cal) (cal1 caller (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'print) (print (eval. (second expr) caller)))
;       (t (apply (eval. (first expr) caller) (eval-list (cdr expr) caller) caller))
))

(defun get1 (caller property) ;(get color) 
  (gethash property (gethash caller *everything*)))

(defun set1 (caller property value) ;(set color red)
  (setf (gethash property (gethash caller *everything*)) value))

(defun run1 (caller func &optional params) ;(run hello1); where hello1 = '(lambda () "hello world")
  (setf lamb (gethash func (gethash caller *everything*)))
  ;(PRINT (third lamb))
  (setf code (sublis (pairlis (second lamb) params) (third lamb)))
  ;(print code)
  (eval. code caller)
  )

(defun cal1 (caller object expr) ;(call_func ball (bounce))
(run1 object 'exe (list caller expr)) ; first get the function run_func from the object called
)

