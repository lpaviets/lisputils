(in-package #:org.numbra.perso.aoc)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(defun symb (&rest args)
  (values (intern (apply #'mkstr args) :keyword)))

(defun year-day-to-package (year day)
  (symb "AOC" year "/EX" day))

(defun days-per-aoc-year (year)
  (if (<= year 2024)
      25
      12))

(defmacro gen-packages (year)
  (let* ((days (days-per-aoc-year year))
         (list (loop :for day :from 1 :to days
                     :collect
                     `(defpackage ,(year-day-to-package year day)
                        (:use :cl #:org.numbra.perso)
                        (:local-nicknames (#:aoc #:org.numbra.perso.aoc)
                                          (#:m #:org.numbra.perso.machine))
                        (:export
                         ,(symb :answer-ex- day :-1)
                         ,(symb :answer-ex- day :-2))))))
    `(progn
       ,@list)))

;;; Only run this code once, in the directory in which you want to code
;;; It creates a whole lot of files, filled with some code

(defun ex-name (day)
  (format nil "ex~2,'0D" day))

(defun create-files-sources (year)
  (loop :initially (let ((src-dir (make-pathname :directory '(:relative "src"))))
                     (ensure-directories-exist src-dir))
        :for day :from 1 :to (days-per-aoc-year year)
        :for day-str = (ex-name day)
        :for dirname = (make-pathname :directory `(:relative "src" ,day-str))
        :for filename = (make-pathname :directory `(:relative "src" ,day-str)
                                       :name day-str
                                       :type "lisp")
        :do (ensure-directories-exist dirname)
        :unless (probe-file filename)
          :do
             (format t "Creating file ~A~%" filename)
             (with-open-file (s
                              filename
                              :direction :output
                              :if-exists :error
                              :if-does-not-exist :create)
               (format s "(in-package #:~(~A~))~%~%"
                       (year-day-to-package year day))
               (format s "(defparameter *input* #P\"input\")~%")
               (format s "(defparameter *test* #P\"test\")~%~%")
               (format s "(defun answer-ex-~a-1 (file))~%~%" day)
               (format s "(defun answer-ex-~a-2 (file))~%~%" day))))


(defun create-file-packages (year)
  (let ((filename (make-pathname :name "packages" :type "lisp")))
    (unless (probe-file filename)
      (format t "Creating file ~a~%" filename)
      (with-open-file (s
                       filename
                       :direction :output
                       :if-exists :error
                       :if-does-not-exist :create)
        (format s ";;;; Packages.lisp~%")
        (format s "(org.numbra.perso.aoc:gen-packages ~A)" year)))))

(defun create-file-asd (year)
  (let ((filename (make-pathname :name (format nil "aoc~A" year)
                                 :type "asd"))
        (register-module (lambda (day)
                           (let ((name (ex-name day)))
                             (list :module (read-from-string name)
                                   :pathname name
                                   :components
                                   `((:file ,name)))))))
    (unless (probe-file filename)
      (format t "Creating file ~a~%" filename)
      (with-open-file (s
                       filename
                       :direction :output
                       :if-exists :error
                       :if-does-not-exist :create)
        (format s ";;;; Packages.lisp")
        (let ((*print-case* :downcase))
          (pprint
           `(asdf:defsystem ,(read-from-string (format nil "#:aoc~A" year))
              :serial t
              :depends-on (#:numbra)
              :components ((:file "packages")
                           (:module src
                            :pathname "src"
                            :components ,(loop :for day :from 1 :to (days-per-aoc-year year)
                                               :collect
                                               (funcall register-module day)))))
           s))))))

(defun create-files-templates (year)
  (create-files-sources year)
  (create-file-packages year)
  (create-file-asd year))
