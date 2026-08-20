(defpackage :recursive-compiler
  (:use :cl))

(in-package :recursive-compiler)

(defparameter *source*
  '(lambda (form)
     (cond
       ((numberp form)
        `((push ,form)))

       ((consp form)
        (append
         (compile-expr (cadr form))
         (compile-expr (caddr form))
         (case (car form)
           (+ '((add)))
           (- '((sub)))
           (* '((mul)))
           (/ '((div)))
           (otherwise
            (error "unknown operator")))))

       (t
        (error "invalid form")))))

(defun compile-expr (form)
  (cond
    ((numberp form)
     `((push ,form)))

    ((consp form)
     (append
      (compile-expr (cadr form))
      (compile-expr (caddr form))
      (case (car form)
        (+ '((add)))
        (- '((sub)))
        (* '((mul)))
        (/ '((div)))
        (otherwise
         (error "unknown operator")))))

    (t
     (error "invalid form"))))

(defun generation (source n)
  (format t "Generation ~4D~%" n)
  (if (= n 1000)
      source
      (generation source (1+ n))))

(format t "~%RECURSIVE COMPILER~%")
(format t "==================~%")

(defparameter *final*
  (generation *source* 0))

(format t "~%1000 generations completed.~%")
