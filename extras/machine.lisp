(in-package #:org.numbra.perso.machine)

(defclass instruction ()
  ((name :reader name
         :initarg :name
         :type symbol)
   (command :reader command
            :initarg :command
            :type (or symbol function)
            :documentation "See `DEFINSTRUCTION'.")))

(defclass assembly ()
  ((instructions :reader instructions
                 :initarg :instructions
                 :type hash-table
                 :documentation "Maps symbols corresponding to instructions
to functions acting on a machine.")))

(defclass machine ()
  ((assembly :reader assembly
             :initarg :assembly
             :documentation "ASSEMBLY that runs on this machine")
   (code :accessor %code
         :initarg :code
         :documentation "Vector containing the list of instructions to execute.")
   (memory :accessor %memory
           :initarg :memory
           :documentation "Memory of the machine. It is separate from the code, although a priori
some instructions could act on both")
   (ptr :accessor %ptr
        :initarg :ptr
        :documentation "Index of the instruction to execute")
   (registers :accessor %registers
              :initarg :registers
              :type hash-table
              :documentation "Map symbols corresponding to register names to values."))
  (:default-initargs
   :ptr 0))

(defun memory (machine cell)
  (aref (%memory machine) cell))

(defun (setf memory) (val machine cell)
  (setf (aref (%memory machine) cell) val))

(defun register (machine reg)
  (gethash reg (%registers machine)))

(defun (setf register) (val machine reg)
  (setf (gethash reg (%registers machine)) val))

(defun code (machine num)
  (aref (%code machine) num))

(defun (setf code) (val machine num)
  (setf (aref (%code machine) num) val))

(defmacro definstruction (name
                          (machine (memory code ptr registers))
                          (&key (advance-ptr 1))
                          lambda-list
                          &body body)
  "Create an instruction NAME, whose command executes BODY.

In BODY, the variables MACHINE, MEMORY, CODE, PTR and REGISTERS are
respectively bound to the current machine, and its respective slots,
when the instruction is executed.

ADVANCE-PTR is the amount by which the machine's ptr is incremented
*after* the instruction has terminated. More fine-grained control
on the ptr can be done by using the locally-bound PTR value.

Within BODY, the local function VALUE can be used to access the value
of an argument regardless of whether it is a register or not. Any
symbol is considered to be a register, any other value is directly
returned."
  (with-gensyms (gx)
    `(make-instance 'instruction
                    :name ',name
                    :command
                    (lambda (,machine ,@lambda-list)
                      (with-accessors ((,memory %memory)
                                       (,code %code)
                                       (,ptr %ptr)
                                       (,registers %registers))
                          ,machine
                        (flet ((value (,gx)
                                 (typecase ,gx
                                   (symbol (register ,machine ,gx))
                                   (t ,gx))))
                          ,@body
                          (incf ,ptr ,advance-ptr)))))))

(defmacro defassembly ((machine (memory code ptr registers))
                       (&key instructions)
                       &body implementation)
  (with-gensyms (ginstrs gassembly)
    `(let* ((,ginstrs (make-hash-table :test 'eq
                                       :size (length ',instructions)))
            (,gassembly (make-instance 'assembly :instructions ,ginstrs)))
       ,@(loop :for inst :in implementation
               :for inst-name = (car inst)
               :collect
               `(setf (gethash ',inst-name ,ginstrs)
                      (definstruction ,inst-name
                          (,machine (,memory ,code ,ptr ,registers))
                          ,@(cdr inst))))
       ,gassembly)))


(defun make-machine (assembly code memory-size memory-init ptr registers)
  "Create a MACHINE object.

MEMORY-SIZE is the size of memory.

MEMORY-INIT is used to initialized the memory. If it is a function, it
is called with one argument, MEMORY-SIZE, and should return an array
correctly initialized. Otherwise, MEMORY-INIT is used as the :initial-element
argument of `make-array'.

REGISTERS is a list of register names. Each element can instead be a pair
(NAME VALUE). If no VALUE is given, the registers are initialized to
0."
  (let ((reg-table (make-hash-table :size (length registers)))
        (mem-array (if (functionp memory-init)
                       (funcall memory-init memory-size)
                       (make-array memory-size :initial-element memory-init))))
    (loop :for name :in registers
          :do (if (consp name)
                  (setf (gethash (car name) reg-table) (cadr name))
                  (setf (gethash name reg-table) 0)))
    (make-instance 'machine
                   :assembly assembly
                   :registers reg-table
                   :memory mem-array
                   :code code
                   :ptr ptr)))

(defun run-instruction (machine)
  (let* ((ptr (%ptr machine))
         (inst (aref (%code machine) ptr))
         (inst-name (car inst))
         (inst-args (cdr inst))
         (inst-assembly (gethash inst-name (instructions (assembly machine)))))
    (apply (command inst-assembly) machine inst-args)))

(defun run-machine (machine &optional stop-condition)
  (loop :until (if (functionp stop-condition)
                   (funcall stop-condition)
                   (= (%ptr machine) (length (%code machine))))
        :do (run-instruction machine)))
