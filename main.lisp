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


(defun println (object)
  (princ object)
  (format t "=> ~%"))


(defun process-client-socket (client-socket)
  "Process client socket that got some activity"
  ;; NOTE: read-line blocks until end-of-line character is received
  ;; see http://mihai.bazon.net/blog/howto-multi-threaded-tcp-server-in-common-lisp
  ;; for read-byte-at-a-time solution
  (let ((message (read-line (usocket:socket-stream client-socket))))
    (logger "got a message: ~a" message)
  ; (send-text-to-socket message client-socket) ))
    (send-text-to-socket (format nil "=> ~a" (eval. (read-from-string message) 'GOD)) client-socket) ))

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
    (run-server-in-thread "0.0.0.0" 12321))
    :default nil)

;---- hash table
(ql:quickload "cl-store")

(defparameter *everything* (make-hash-table))
(defparameter *file-name* "everything.out")


(defun save-file ()
  (cl-store:store *everything* *file-name* ))

(defun load-file ()
  (defparameter *everything* (cl-store:restore *file-name* )))

(defun print-universe () (loop for key1 being the hash-keys of *everything*
                               using (hash-value value1)
                               :do
                               (print key1)
                               (terpri)
                               (loop for key2 being the hash-keys of value1
                                     using (hash-value value2)
                                     :do
                                     (format t "~S ~S ~%" key2 value2))))


(defun println (object)
  (princ object)
  (format t "~%=> "))

                                        ; -- game eval
(defun eval-list (lst &optional caller)
  (if (null lst)
      nil
      (cons (eval. (first lst) caller) (eval-list (rest lst) caller))))

(defun cond-eval (clauses &optional caller)
  (cond ((eval. (first (first clauses)) caller) (eval. (second (first clauses)) caller))
        (t (cond-eval (rest clauses) caller))))

 
(defun eval. (expr caller)
  (cond
        ((atom expr) expr)
        ; basic
        ((eq (first expr) 'quote) (second expr))
        ((eq (first expr) 'atom) (atom (eval. (second expr) caller)))
        ((eq (first expr) 'eq) (eq (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'car) (car (eval. (second expr) caller)))
        ((eq (first expr) 'cdr) (cdr (eval. (second expr) caller)))
        ((eq (first expr) 'cons) (cons (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) 'eval) (eval. (second expr) caller)) ;(eval (fn param))
        ((eq (first expr) 'cond) (cond-eval (cdr expr) caller))
        ((eq (first expr) 'begin) (last (mapcar (lambda (e) (eval. e caller)) (cdr expr))))
        ; numbers
        ((eq (first expr) '+) (+ (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) '-) (- (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) '*) (* (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) '/) (/ (eval. (second expr) caller) (eval. (third expr) caller)))
        ((eq (first expr) '+) (+ (eval. (second expr) caller) (eval. (third expr) caller)))        
        ((eq (first expr) '^) (expt (eval. (second expr) caller) (eval. (third expr) caller)))
        ; time and randomness
        ((eq (first expr) 'time)  (get-universal-time))
        ((eq (first expr) 'rand) (+ (random 8999999999) 1000000000))
        
        ; universe
        ((eq (first expr) 'get) (get1 caller (second expr)))
        ((eq (first expr) 'set) (set1 caller  (second expr) (eval. (third expr) caller)))
        ((eq (first expr) 'run) (run1 caller   (second expr)  (eval. (third expr) caller)))
        ((eq (first expr) 'call) (cal1 caller (second expr)  (third expr)))
        ((eq (first expr) 'create) (create1 caller  (second expr))) 
        ((multiple-value-bind (value bool) (gethash (car expr) (gethash caller *everything*))
           (when bool
             (run2 caller value (cdr expr)))))))

(defun get1 (caller property) ;(get color) 
  (gethash property (gethash caller *everything*)))

(defun set1 (caller property value) ;(set color red)
  (setf (gethash property (gethash caller *everything*)) value))

(defun run1 (caller func &optional params) ;(run func '(p1 p2 p3))
  (setf lamb (gethash func (gethash caller *everything*)))
  (setf code (sublis (pairlis (second lamb) params) (third lamb)))
  (eval. code caller))

(defun run2 (caller lamb &optional params) ;(run func '(p1 p2 p3))
  (setf code (sublis (pairlis (second lamb) params) (third lamb)))
  (eval. code caller))

(defun cal1 (caller object expr) ;(call ball (bounce))
  (run1 object 'exe (list caller expr)) ; first get the function exe from the object called
  ) ; caller expression

(defun create1 (caller name) ;(create ball)
  (unless (gethash name *everything*)
    (setf (gethash name *everything*) (make-hash-table))
    (setf (gethash 'exe (gethash name *everything*)) 
        `(lambda (c m) ; caller expression
          (cond ((eq ,caller c) (eval m)))))))



(defun repl ()
  (loop (println (eval. (read) 'GOD))))


;; TODO: pretty-print hashtable, move, look, speak, sun/time
                                        
;; move: 3 objects: object, curr room, next room.
;; (move obj curr next)

