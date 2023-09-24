;; Load the required libraries
(ql:quickload '(:bordeaux-threads :usocket))



;; Define a function to handle client connections
(defun handle-client (client-socket)
  (let* ((stream (usocket:socket-stream client-socket))
	 (user (log-in stream)))

    (when (null user) 
      (format stream "credentials incorrect")
      (force-output stream)
      (return-from handle-client))

    (format stream "logged in as ~a~%" user)
    (force-output stream)
    (setf (gethash 'stream (gethash user *everything*)) stream) 

    (loop
      (let ((message (read stream)))
	(format t "msg recieved~%")
        (if (or (null message) (eq message 'QUIT))
            (progn
              (format t "Client disconnected~%")
              (return))
            (progn 
	      (format stream "> ~a~%" (handler-case (eval. message user)
					(error (c)
					       c))) ; cant sent error to stream
	      (force-output stream)))))))

(defun log-in (stream)
  (format stream "Enter (username password): ")
  (force-output stream)
  (let* ((credential (read stream))
	 (username (car credential))
	 (password (cadr credential)))
    (format t "~a ~a~%" username password) 
    (cond
      ((null password) 
       (multiple-value-bind (value bool) (gethash username *everything*)
           (unless bool 
	     (setf (gethash username *everything*) (make-hash-table))
	     username
	     ))) ; create user
      ((multiple-value-bind (value bool) (gethash username *everything*)
           (when (and bool (eq password (gethash 'password value))) ; if username exists, check password
	     (format t "~a~%" (gethash 'password value))
             username))))))


;; Define the main server function
(defun start-echo-server (port)
  (let ((server-socket (usocket:socket-listen "0.0.0.0" port :REUSEADDRESS t)))
    (format t "Server listening on port ~a~%" port)
    (loop
      (let ((client-socket (usocket:socket-accept server-socket)))
        (bt:make-thread (lambda ()
                          (handle-client client-socket)))))))



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




; -- game eval


(defun cond-eval (clauses caller)
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
        ((eq (first expr) 'run) (run1 caller (gethash (second expr) (gethash caller *everything*)) (eval. (third expr) caller)))
        ((eq (first expr) 'call) (cal1 caller (second expr)  (third expr)))
        ((eq (first expr) 'create) (create1 caller  (second expr))) 
	((eq (first expr) 'msg) (format (gethash 'stream (gethash caller *everything*)) "~a~%" (second expr)) (force-output stream))
	
	;save
	((eq (first expr) 'save) (when (eq caller 'god) (save-file)))
	;reload
	((eq (first expr) 'reload) (when (eq caller 'god) (save-file) (load "main.lisp") (load-file)))


	; (func param) run func if defined on user itself
        ((multiple-value-bind (value bool) (gethash (car expr) (gethash caller *everything*))
           (when bool
             (run1 caller value (cdr expr)))))))

(defun get1 (caller property) ;(get color) 
  (gethash property (gethash caller *everything*)))

(defun set1 (caller property value) ;(set color red)
  (setf (gethash property (gethash caller *everything*)) value))

(defun run1 (caller lamb &optional params) ;(run func '(p1 p2 p3))
  (eval. 
    (sublis (pairlis (second lamb) params) (third lamb)) ; replace x y in code with values given
    caller))

(defun cal1 (caller object expr) ;(call ball (bounce))
  (run1 object (gethash 'exe (gethash object *everything*)) (list caller expr)) ; send code to func exe on object
  ) 

(defun create1 (caller name) ;(create ball)
  (unless (gethash name *everything*)
    (setf (gethash name *everything*) (make-hash-table))
    (setf (gethash 'exe (gethash name *everything*)) 
        `(lambda (c m) ; exe is the function that is called on the object when (call object '(code)) is used.
          (cond ((eq ,caller c) (eval m)))))
    t))
#|
(defun create1 (caller name) 
  (multiple-value-bind (value bool) (gethash name *everything*)
           (unless bool
             (setf value (make-hash-table))
	     (setf (gethash 'exe value) 
		   `(lambda (c m) ; caller expression
		      (cond ((eq ,caller c) (eval m))))) 
	     t)))

|#
; -- repl
(defun println (object)
  (princ object)
  (format t "~%=> "))

(defun repl ()
  (loop (println (eval. (read) 'GOD))))

; -- run
;; Start the echo server on port 12345
(defun main ()
  (progn 
    (defparameter *everything* (make-hash-table))
    (load-file)
    (start-echo-server 12345)
    (save-file)))

;; TODO: pretty-print hashtable, move, look, speak, sun/time
                                        
;; move: 3 objects: object, curr room, next room.
;; (move obj curr next)

