(defpackage :eotw
  (:use :cl)
  (:export
   :new-world
   :evaluate
   :tick
   :record
   :world
   :world-environment
   :world-program
   :world-stack
   :world-history
   :world-generation
   :world-running
   :world-steps))

(in-package :eotw)

(defstruct world
  environment
  program
  stack
  history
  generation
  running
  steps)

(defstruct event
  type
  form
  result
  parent
  children
  depth
  generation)

(defstruct closure
  parameters
  body
  environment)

(defun new-world ()
  (make-world
   :environment
   `((+ . ,#'+)
     (- . ,#'-)
     (* . ,#'*)
     (/ . ,#'/)
     (= . ,#'=)
     (< . ,#'<)
     (> . ,#'>)
     (cons . ,#'cons)
     (car . ,#'car)
     (cdr . ,#'cdr)
     (list . ,#'list)
     (null . ,#'null))
   :program nil
   :stack nil
   :history nil
   :generation 0
   :running nil
   :steps 0))

(defun record (world event)
  (push event (world-history world))
  event)

(defun lookup (name environment)
  (let ((binding (assoc name environment)))
    (if binding
        (cdr binding)
        (error "EOTW: unbound symbol ~A" name))))

(defun bind (names values environment)
  (append
   (mapcar #'cons names values)
   environment))

(defun push-stack (world value)
  (push value (world-stack world))
  value)

(defun pop-stack (world)
  (pop (world-stack world)))

(defun tick (world)
  (incf (world-generation world))
  (incf (world-steps world)))

(defun evaluate (form world)
  (let* ((parent (first (world-stack world)))
         (event
           (make-event
            :type :evaluation
            :form form
            :parent parent
            :depth (length (world-stack world))
            :generation (world-generation world))))

    (record world event)

    (when parent
      (push event
            (event-children parent)))

    (push-stack world event)

    (unwind-protect
         (let ((result (eval-form form world)))
           (setf (event-result event) result)
           result)
      (pop-stack world))))

(defun eval-form (form world)
  (tick world)

  (cond
    ((null form)
     nil)

    ((numberp form)
     form)

    ((stringp form)
     form)

    ((keywordp form)
     form)

    ((symbolp form)
     (lookup form
             (world-environment world)))

    ((consp form)
     (eval-list form world))

    (t
     (error "EOTW: cannot evaluate ~S" form))))

(defun eval-list (form world)
  (let ((operator (car form))
        (arguments (cdr form)))

    (case operator

      (quote
       (cadr form))

      (if
       (if (evaluate (cadr form) world)
           (evaluate (caddr form) world)
           (evaluate (cadddr form) world)))

      (progn
       (let ((result nil))
         (dolist (expression arguments result)
           (setf result
                 (evaluate expression world)))))

      (lambda
       (make-closure
        :parameters (cadr form)
        :body (caddr form)
        :environment (world-environment world)))

      (define
       (let* ((name (cadr form))
              (value (evaluate (caddr form) world))
              (binding (assoc name
                              (world-environment world))))
         (if binding
             (setf (cdr binding) value)
             (push (cons name value)
                   (world-environment world)))
         value))

      (set
       (let ((name (cadr form))
             (value (evaluate (caddr form) world)))
         (let ((binding
                 (assoc name
                        (world-environment world))))
           (if binding
               (setf (cdr binding) value)
               (error "EOTW: cannot set unbound symbol ~A"
                      name)))
         value))

      (t
       (let ((function
               (evaluate operator world))
             (values
               (mapcar
                (lambda (argument)
                  (evaluate argument world))
                arguments)))
         (apply-function function values world))))))

(defun apply-function (function arguments world)
  (cond

    ((closure-p function)
     (let ((old-environment
             (world-environment world)))
       (setf (world-environment world)
             (bind
              (closure-parameters function)
              arguments
              (closure-environment function)))

       (unwind-protect
            (evaluate
             (closure-body function)
             world)

         (setf (world-environment world)
               old-environment))))

    ((functionp function)
     (apply function arguments))

    (t
     (error
      "EOTW: ~S is not callable"
      function))))

(defun start (world)
  (setf (world-running world) t)
  world)

(defun stop (world)
  (setf (world-running world) nil)
  world)

(defun reset (world)
  (setf
   (world-program world) nil
   (world-stack world) nil
   (world-history world) nil
   (world-generation world) 0
   (world-steps world) 0
   (world-running world) nil)
  world)

(defun run (program world)
  (setf (world-program world) program)
  (start world)
  (evaluate program world))

(defun history (world)
  (reverse (world-history world)))
