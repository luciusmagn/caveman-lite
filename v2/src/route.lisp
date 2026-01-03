(in-package :cl-user)
(defpackage caveman2.route
  (:use :cl)
  (:import-from :caveman2.app
                :find-package-app)
  (:import-from :caveman2.nested-parameter
                :parse-parameters)
  (:export :defroute
           :*parsed-parameters-symbol-name*))
(in-package :caveman2.route)

(defun add-app-if-omitted (routing-rule)
  (if (or (and (listp routing-rule)
               (stringp (car routing-rule)))
          (stringp routing-rule))
      `(cons (find-package-app ,*package*) ,(if (listp routing-rule)
                                                `(list ,@routing-rule)
                                                `(list ,routing-rule)))
      (if (listp routing-rule)
          `(list ,@routing-rule)
          `(list ,routing-rule))))

;; Add &allow-other-keys if &key exists.
(defun make-lambda-list (lambda-list)
  (if (and lambda-list
           (member 'cl:&key lambda-list :test #'eq)
           (not (eq (car (last lambda-list)) 'cl:&allow-other-keys)))
      (append lambda-list '(cl:&allow-other-keys))
      lambda-list))

(defun parse-key-arguments (lambda-list)
  (loop for (arg . rest-args) on lambda-list
        if (eq arg 'cl:&key)
          do (return
               (loop for arg in rest-args
                     until (and (symbolp arg)
                                (eq (symbol-package arg) (find-package :common-lisp))
                                (char= (aref (symbol-name arg) 0) #\&))
                     collect arg))))

(defun params-form (params-symb lambda-list)
  (let ((pair (gensym "PAIR")))
    `(nconc ,@(loop for arg in (parse-key-arguments lambda-list)
                    collect (destructuring-bind (arg &optional default specified)
                                (if (consp arg) arg (list arg))
                              (declare (ignore default specified))
                              `(let ((,pair (assoc ,(if (or (string= arg :captures)
                                                            (string= arg :splat))
                                                        (intern (symbol-name arg) :keyword)
                                                        (symbol-name arg))
                                                   ,params-symb
                                                   :test #'string=)))
                                 (if ,pair
                                     (list ,(intern (symbol-name arg) :keyword) (cdr ,pair))
                                     nil)))))))

(defparameter *parsed-parameters-symbol-name* #.(string :_parsed))

(defun need-parsed-parameters (lambda-list)
  (member-if (lambda (p)
               (and (symbolp p)
                    (string= *parsed-parameters-symbol-name* p)))
             lambda-list))

(defmacro defroute (&rest args)
  (let ((params (gensym "PARAMS")))
    (typecase (car args)
      (symbol
       (destructuring-bind (name routing-rule lambda-list &rest body) args
         `(prog1
              ,(multiple-value-bind (body declarations documentation)
                   (alexandria:parse-body body :documentation t)
                 `(defun ,name (,params)
                    (declare (ignorable ,params))
                    ,@(if documentation (list documentation))
                    ,@(if lambda-list
                          `((destructuring-bind ,(make-lambda-list lambda-list)
                                ,(if (need-parsed-parameters lambda-list)
                                  `(append (list
                                            ,(intern *parsed-parameters-symbol-name* :keyword)
                                            (parse-parameters ,params))
                                           ,(params-form params lambda-list))
                                  (params-form params lambda-list))
                              ,@declarations
                              ,@body))
                         body)))
            (setf (apply #'ningle:route
                         (append
                          ,(add-app-if-omitted routing-rule)
                          (list :identifier ',name)))
                  (function ,name)))))
      (list  (destructuring-bind (routing-rule lambda-list &rest body) args
               (multiple-value-bind (body declarations documentation)
                    (alexandria:parse-body body :documentation t)
                  `(setf (apply #'ningle:route
                                ,(add-app-if-omitted routing-rule))
                         (lambda (,params)
                           (declare (ignorable ,params))
                           ,@(if documentation (list documentation))
                           ,@(if lambda-list
                                 `((destructuring-bind ,(make-lambda-list lambda-list)
                                       ,(if (need-parsed-parameters lambda-list)
                                            `(append (list
                                                      ,(intern *parsed-parameters-symbol-name* :keyword)
                                                      (parse-parameters ,params))
                                                     ,(params-form params lambda-list))
                                            (params-form params lambda-list))
                                     ,@declarations
                                     ,@body))
                                 body))))))
      (T `(defroute (,(car args)) ,@(cdr args))))))
