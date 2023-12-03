;;; Parsing

(in-package #:org.numbra.perso.io)

;;;; Reading from files
;;;; Most functions take a PARSE keyword argument, supposed to be a function
;;;; This function is applied to each line of the input before collecting it
(defun read-file-as-lines (filename &key parse delete-empty)
  "Read file into a list of lines."
  (with-open-file (in filename)
    (loop :for line = (read-line in nil nil)
          :while line
          :unless (and delete-empty
                       (string= line ""))
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

(defun read-file-as-lines-blocks (filename &key parse parse-block)
  "Read file into a list of list of strings.
Each empty line in FILENAME is considered to be the end of a block
If non-NIL, PARSE is applied to each line, and PARSE-BLOCK to each block"
  (with-open-file (in filename)
    (flet ((read-block ()
             (loop :for line = (read-line in nil nil)
                   :while (and line (> (length line) 0))
                   :collect (if parse (funcall parse line)
                                line))))
      (loop :for block = (read-block)
            :while block
            :collect (if parse-block
                         (funcall parse-block block)
                         block)))))

(defun read-file-as-integers (filename)
  (read-file-as-lines filename :parse 'parse-integer))

(defun read-file-as-sexprs (filename &key parse)
  "Read file as a list of s-expressions.
Each line <line> is read a the s-expr (<line>)
If non-NIL, PARSE is applied to each line before reading it, i.e., the
line is read as (FORMAT NIL \"(~A)\" (FUNCALL PARSE <line>))"
  (with-open-file (in filename)
	(loop :for line = (read-line in nil nil)
		  :while line
          :collect
          (read-from-string (format nil "(~A)" (if parse
                                                   (funcall parse line)
                                                   line))))))

(defun read-array (list &key as-digits)
  "Read a 2D-array. If DIGITS is non-nil, parses elements as digits"
  (loop :with array = (make-array (list (list-length list)
                                        (length (car list))))
        :for line :in list
        :for i :from 0 :do
          (loop :for c :across line
                :for j :from 0
                :for val = (if as-digits
                               (or (digit-char-p c)
                                   (error "Expected a digit, got ~A instead" c))
                               c)
                :do (setf (aref array i j) val))
        :finally (return array)))

(defun read-file-as-array (filename &key as-digits)
  (read-array (read-file-as-lines filename) :as-digits as-digits))

;; Other parsing utilities

(defun split-word-int (line)
  (ppcre:register-groups-bind (word (#'parse-integer int))
      ("\(\\w+\) \(\\d+\)" line)
    (cons word int)))

(defun collect-integers-in-line (line)
  "Return the list of all the matched integers in LINE.
An integer is anything optionnally starting with an optional -,
and consisting only of digits.

Example:

(collect-integers-in-line \"125-4x89 -9k:-09\")
=> (125 -4 89 -9 -9)"
  (let (acc)
    (ppcre:do-matches-as-strings (i "-?\\d+" line (reverse acc))
      (push (parse-integer i) acc))))

(defun coma-separated-int-line (line)
  (mapcar 'parse-integer (ppcre:split " *, *" line)))
