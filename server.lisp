;; The server sided stuff for my login instance

(load "~/quicklisp/setup.lisp")

(ql:quickload '(clack alexandria com.inuoe.jzon))

(defparameter *clack-server*
  (clack:clackup (lambda (env) (funcall 'handler env))
    :port 4242))

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

      ((alexandria:starts-with-subseq "/logiverse.js" path-info)
        `(200 
          (:content-type "text/javascript") 
          (,(file-get-contents "frontend/logiverse.js"))))

      ((alexandria:starts-with-subseq "/login" path-info) 
        (let ((result (handle-login (com.inuoe.jzon:parse raw-body))))
          (if result
            (return-from handler result)))
        `(200
          (:content-type "text/plain")
          ("")))

      ((alexandria:starts-with-subseq "/getUsers" path-info) 
       `(200
         (:content-type "application/json")
         ("[[\"This is a successful test\", \"me! :3\", \"2024-11-11 23:58:45\", null, 0]]")))

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

(defun handle-login (req-body)
  (let 
    ((username (gethash "username" req-body))
     (password (gethash "password" req-body)))

    (unless (and username password) 
      (return-from handle-login (error-response "Missing username or password")))

    (if (contains-banned-chars username) (return-from handle-login (error-response "Invalid username")))
    (if (contains-banned-chars password) (return-from handle-login (error-response "Invalid password")))
    (princ (format nil "username: ~A password: ~A" username password))
    (terpri)))

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

(sleep most-positive-fixnum)
