;; The server sided stuff for my login instance

(load "~/quicklisp/setup.lisp")

(ql:quickload '(clack alexandria))

(defparameter *clack-server*
  (clack:clackup (lambda (env) (funcall 'handler env))
    :port 4242))

(defun handler (env)
  (destructuring-bind (&key request-method path-info request-uri
                            query-string headers &allow-other-keys) env
    (princ (format nil "~S request for ~S: ~S" request-method request-uri query-string))
    (terpri)
    (cond
      ((alexandria:starts-with-subseq "/index.html" path-info)
        `(200 
          nil 
          (,(file-get-contents "frontend/index.html"))))

      ((alexandria:starts-with-subseq "/logiverse.js" path-info)
        `(200 
          (:content-type "text/javascript") 
          (,(file-get-contents "frontend/logiverse.js"))))

      ((alexandria:starts-with-subseq "/update" path-info) 
       (princ "Update")
       (terpri))

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

(sleep most-positive-fixnum)
