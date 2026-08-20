(defpackage :wtf
  (:use :cl))

(in-package :wtf)

(defstruct node
  form
  parent
  children)

(defstruct machine
  env
  root
  steps)

(defun lookup (name env)
  (let ((x (assoc name env)))
    (if x
        (cdr x)
        (error "WTF: unbound ~A" name))))

(defun extend (names values env)
  (append (mapcar #'cons names values) env))

(defun make-tree (form &optional parent)
  (let ((node (make-node :form form :parent parent)))
    (when (consp form)
      (setf (node-children node)
            (mapcar (lambda (x)
                      (make-tree x node))
                    form)))
    node))

(defun wtf-eval (form machine)
  (incf (machine-steps machine))
  (eval-form form machine))

(defun eval-form (form machine)
  (let ((env (machine-env machine)))
    (cond
      ((numberp form)
       form)

      ((stringp form)
       form)

      ((symbolp form)
       (lookup form env))

      ((null form)
       nil)

      ((eq (car form) 'quote)
       (cadr form))

      ((eq (car form) 'if)
       (if (wtf-eval (cadr form) machine)
           (wtf-eval (caddr form) machine)
           (wtf-eval (cadddr form) machine)))

      ((eq (car form) 'lambda)
       (list :closure
             (cadr form)
             (caddr form)
             env))

      ((eq (car form) 'define)
       (let ((name (cadr form))
             (value (wtf-eval (caddr form) machine)))
         (push (cons name value)
               (machine-env machine))
         value))

      (t
       (let ((fn (wtf-eval (car form) machine))
             (args (mapcar
                    (lambda (x)
                      (wtf-eval x machine))
                    (cdr form))))
         (wtf-apply fn args machine))))))

(defun wtf-apply (fn args machine)
  (cond
    ((and (consp fn)
          (eq (car fn) :closure))
     (let ((params (second fn))
           (body (third fn))
           (env (fourth fn)))
       (let ((old (machine-env machine)))
         (setf (machine-env machine)
               (extend params args env))
         (unwind-protect
              (wtf-eval body machine)
           (setf (machine-env machine) old)))))

    ((functionp fn)
     (apply fn args))

    (t
     (error "WTF: ~A is not callable" fn))))

(defun bootstrap ()
  (make-machine
   :env `((+ . ,#'+)
          (- . ,#'-)
          (* . ,#'*)
          (/ . ,#'/)
          (= . ,#'=)
          (< . ,#'<)
          (> . ,#'>)
          (cons . ,#'cons)
          (car . ,#'car)
          (cdr . ,#'cdr)
          (list . ,#'list))
   :steps 0))

(defun wtf (form)
  (let ((machine (bootstrap)))
    (setf (machine-root machine)
          (make-tree form))
    (wtf-eval form machine)))

(format t "~&WTF: End Of The World~%")
(format t "Booting recursive evaluator...~%")
