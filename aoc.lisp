(in-package #:org.numbra.perso.aoc)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defun year-day-to-package (year day)
  (symb "AOC" year "/EX" day))

(defmacro gen-packages (year)
  (let ((list (loop :for day :from 1 :to 25
                    :collect
                    `(defpackage ,(year-day-to-package year day)
                       (:use :cl #:org.numbra.perso)
                       (:export
                        ,(symb :answer-ex- day :-1)
                        ,(symb :answer-ex- day :-2))))))
    `(progn
       ,@list)))

;;; Only run this code once, in the directory in which you want to code
;;; It creates a whole lot of files, filled with some code

(defun create-files-templates (year)
  (loop :initially (let ((src-dir (make-pathname :directory '(:relative "src"))))
                     (ensure-directories-exist src-dir))
        :for day :from 1 :to 25
        :for filename = (make-pathname :directory '(:relative "src")
                                       :name (format nil "ex~a" day)
                                       :type "lisp")
        :unless (probe-file filename)
          :do
             (format t "Creating file ~a~%" filename)
             (with-open-file (s
                              filename
                              :direction :output
                              :if-exists :error
                              :if-does-not-exist :create)
               (format s "(in-package #:~(~A~))~%~%" (year-day-to-package year day))
               (format s "(defun answer-ex-~a-1 ())~%~%" day)
               (format s "(defun answer-ex-~a-2 ())~%~%" day))))
