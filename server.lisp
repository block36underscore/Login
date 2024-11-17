;; The server sided stuff for my login instance

(load "~/quicklisp/setup.lisp")

(ql:quickload '(clack com.inuoe.jzon mito))

(defconstant table-name "users")
(defconstant message-wrong-password "Incorrect username or password")

(mito:connect-toplevel :sqlite3
                       :database-name "user-data")

(mito:deftable user ()
  ((username :col-type (:text) 
             :primary-key T)
   (password :col-type (:text))
   (status   :col-type (:text))
   (gif      :col-type (:integer))))

(defun new-user (username password 
                &key (status "Hello I am new") 
                     (gif "none"))
  (make-instance 'user :username username 
                       :password password 
                       :status   status
                       :gif      gif))

(defun get-user (username &optional password)
  (if (not password) 
    (mito:find-dao 'user :username username)
    (mito:find-dao 'user :username username
                         :password password)))

(mito:ensure-table-exists 'user)

(defun update-user (user-data)
  (if (get-user (slot-value user-data 'username))
    (progn 
      (mito:delete-dao user-data)
      (mito:save-dao user-data))
    (mito:insert-dao user-data)))

(defparameter *clack-server*
  (clack:clackup (lambda (env) (funcall 'handler env))
    :port 4242))

(defun serialize-status (user-data)
  (format nil 
          "[~S,~S,~S,~A,~D]"
          (slot-value user-data 'status)
          (slot-value user-data 'username)
          "2024-11-11 23:58:45"
          "null"
          0))

(defun serialize-statuses ()
  (with-output-to-string (output)
    (let ((statuses (mito:retrieve-dao 'user))
          (first T))
      (terpri)
      (princ "[" output)
      (mapcar 
        (lambda (user-data)
          (unless first 
            (princ "," output))
          (setf first nil)
          (princ (serialize-status user-data) output))
        statuses)
      (princ "]" output))))

(defun handler (env)
  (destructuring-bind (&key request-method path-info request-uri
                            query-string headers raw-body &allow-other-keys) env
    (princ (format nil "~S request for ~S: ~S" request-method request-uri query-string))
    (terpri)
    (cond
      ((string= "/" path-info)
        `(200 
          nil 
          (,(file-get-contents "frontend/index.html"))))

      ((string= "/logiverse.js" path-info)
        `(200 
          (:content-type "text/javascript") 
          (,(file-get-contents "frontend/logiverse.js"))))

      ((string= "/login" path-info) 
        (let ((result (handle-login (com.inuoe.jzon:parse raw-body))))
          (if result
            (return-from handler result)))
        `(200
          (:content-type "text/plain")
          ("")))

      ((string= "/update" path-info) 
        (let ((result (handle-update (com.inuoe.jzon:parse raw-body))))
          (if result
            (return-from handler result)))
        `(200
          (:content-type "text/plain")
          ("")))

      ((string= "/getUsers" path-info)
       (princ (serialize-statuses))
       (terpri)
       `(200
         (:content-type "application/json")
         (,(serialize-statuses))))

      (T
        `(404
          nil
          (,(format nil "Not found <br><br> Method: ~S Path: ~S URI: ~A Query: ~S~%Headers: ~S"
                    request-method path-info request-uri query-string
                    (alexandria:hash-table-alist headers))))))))

(defun file-get-contents (filename)
  (with-open-file (stream filename)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun error-response (message)
  `(400
    (:content-type "application/json")
    (,(format nil "{ \"error\": ~S }" message))))

(defun success-response ()
  `(200
    (:content-type "application/json")
    ("{}")))

(defun contains-banned-chars (str)
  (or 
    (find #\Return str)
    (find #\Tab str)
    (find #\Linefeed str)
    ;(find #\ZERO_WIDTH_JOINER str)
    ;(find #\ZERO_WIDTH_SPACE str)
    ;(find #\ZERO_WIDTH_NON_JOINER str)
    ;(find #\ZERO_WIDTH_SPACE str)
    (find #\Space str)))

(defun handle-update (req-body)
  (unless (get-user (gethash "username" req-body))
    (return-from handle-update 
      (error-response (format nil "The user \"~A\" does not exist" 
        (gethash "username" req-body)))))

  (let ((status    (gethash "status"   req-body))
        (gif       (gethash "gif"      req-body))
        (user-data (get-user (gethash "username" req-body) 
                             (gethash "password" req-body))))

    (if user-data
      (progn 
        (setf (slot-value user-data 'status) status)
        (if (string= gif "NULL")
          (setf gif "none"))
        (setf (slot-value user-data 'gif)    gif)
        (update-user user-data)
        (return-from handle-update (success-response)))
      (return-from handle-update (error-response message-wrong-password)))))

(defun handle-login (req-body)
  (let 
    ((username (gethash "username" req-body))
     (password (gethash "password" req-body)))

    (unless (and username password) 
      (return-from handle-login (error-response "Missing username or password")))

    (if (contains-banned-chars username) 
      (return-from handle-login (error-response "Invalid username")))
    (if (> (length username) 50) 
      (return-from handle-login (error-response "Username is longer than 50 characters")))
    (if (contains-banned-chars password) 
      (return-from handle-login (error-response "Invalid password")))
    (if (> (length password) 50) 
      (return-from handle-login (error-response "Password is longer than 50 characters")))
    (princ (format nil "username: ~A password: ~A" username password))
    (terpri)

    (if (get-user username)
      (if (get-user username password)
        (return-from handle-login (success-response))
        (return-from handle-login (error-response message-wrong-password)))
      (progn 
        (update-user (new-user username password))
        (return-from handle-login (success-response))))))


(sleep most-positive-fixnum)
