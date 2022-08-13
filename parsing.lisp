;;; Parsing

(in-package #:org.numbra.perso.io)

;; Reading from files
(defun read-file-as-lines (filename &key parse)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop :for line = (read-line in nil nil)
          :while line
          :collect (if parse
                       (funcall parse line)
                       line))))

(defun read-file-one-line (filename &key parse)
  "Read the first line of FILENAME, applying PARSE to it if non-NIL"
  (with-open-file (in filename)
    (let ((line (read-line in nil nil)))
      (if parse
          (funcall parse line)
          line))))

(defun read-file-as-lines-blocks (filename &key parse)
  "Read file into a list of list of strings.
Each empty line in FILENAME is considered to be the end of a block"
  (with-open-file (in filename)
    (flet ((read-block ()
             (loop :for line = (read-line in nil nil)
                   :while (and line (> (length line) 0))
                   :collect (if parse (funcall parse line)
                                line))))
      (loop :for block = (read-block)
            :while block
            :collect block))))

(defun read-file-as-integers (filename)
  (read-file-as-lines filename :parse 'parse-integer))

(defun read-file-as-sexprs (filename)
  "Read file as a list of s-expressions.
Each line <line> is read a the s-expr (<line>)"
  (with-open-file (in filename)
	(loop :for line = (read-line in nil nil)
		  :while line
          :collect
          (read-from-string (format nil "(~a)" line)))))

(defun read-array (list &optional (digits t))
  "Read a 2D-array. If DIGITS is non-nil, parses elements as digits"
  (loop :with array = (make-array (list (list-length list)
                                        (length (car list))))
        :for line :in list
        :for i :from 0 :do
          (loop :for c :across line
                :for j :from 0
                :for val = (if digits
                               (- (char-int c) (char-int #\0))
                               c)
                :do
                   (setf (aref array i j) val))
        :finally (return array)))

(defun read-file-as-array (filename &optional (digits t))
  (read-array (read-file-as-lines filename) digits))

;; Other parsing utilities

(defun split-word-int (line)
  (ppcre:register-groups-bind (word (#'parse-integer int))
      ("\(\\w+\) \(\\d+\)" line)
    (cons word int)))

(defun coma-separated-int-line (line)
  (mapcar 'parse-integer (ppcre:split " *, *" line)))
