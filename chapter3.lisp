;;;; Chapter 3: Developing a Simple CLOS Program: Locks

;;; A lock is a mechanism for controlling access to a shared
;;; resource. The interface should support the following operations:
;;;    - create: create a new lock.
;;;    - seize: size a lock; when successful, the lock object is the
;;;      return value.
;;;    - release: release the lock, if owned by same process trying to
;;;      release the lock.

;;; The chapter defines two basic types of locks:
;;;     - simple lock: has a name, and is either busy or free. if
;;;       busy, it keeps track of the owner.
;;;     - null lock: doesn't actually lock.

;;;; class implementations

(defclass lock ()
  ((name :initarg :name :reader lock-name))
  (:documentation "Superclass for all locks."))

;;; defclass - class definition macro
;;; lock     - class name
;;; ()       - direct superclass
;;; ((name :initarg :name :reader lock-name))
;;;          - slot specifier list:
;;;              - :initarg :name
;;;                initialise the value of the slot using the :name
;;;                keyword in make-instance.
;;;              - :reader lock-name
;;;                creates an accessor using the name lock-name.
;;; (:documentation "Superclass for all locks.")
;;;          - class option providing a docstring.

(defclass null-lock (lock)
  ()
  (:documentation "A NULL lock; this lock will always be free."))

;;; inherited from lock class:
;;;     - name slot
;;;     - :initarg :name
;;;     - lock-name method

(defclass simple-lock (lock)
  ((owner :initform nil :accessor lock-owner))
  (:documentation "Lock subclass that is either free or busy."))

;;;     - :initform nil: allows you to give a default initial value if
;;;       one isn't provided.
;;;     - :accessor lock-owner: access lock-owner for reading and
;;;       writing the slot's value.

;;; slot options:
;;;     - :reader - read-only accessor method
;;;     - :accessor - read and write methods
;;;     - :writer - write-only accessor method

;;;; constructors
(defun make-null-lock (name)
  (make-instance 'null-lock :name name))

(defun make-simple-lock (name)
  (make-instance 'simple-lock :name name))

(defvar *null-lock*   (make-null-lock "global null lock"))
(defvar *simple-lock* (make-simple-lock "global simple lock"))

;;;; illustrating inheritance
(defun display-lock-inheritance ()
  (format t "type-of *null-lock*: ~a~%" (type-of *null-lock*))
  (format t "type-of *simple-lock*: ~a~%" (type-of *simple-lock*))
  (format t "simple-lock is a lock? ~a~%" (typep *simple-lock* 'lock))
  (format t "simple-lock is a null-lock? ~a~%" (typep *simple-lock* 'null-lock)))

;;;; generic methods

;;; note that these now shape future methods: all methods based on
;;; this generic must have congruent parameterisation.

(defgeneric seize (lock)
  (:documentation
"Seizes the lock.
Returns the lock when the operation successed. Some locks wait until they
succeed, while others returns nil on failure."))

(defgeneric release (lock &optional failure-mode)
  (:documentation
"Releases the lock if it is currently owned by this process. Returns t on
success. If unsuccessful and failure-mode is :no-error, returns nil. If
 unsuccessful and failure-mode is :error, signals an error. The default
error mode is :no-error."))

;;; alternatively, creating a method automatically creates the
;;; corresponding generic.

;;;; method definitions

(defmethod seize ((l null-lock)) l) ; return lock, no waiting
(defmethod release ((l null-lock) &optional failure-mode)
  (declare (ignore failure-mode))   ; never fails for null locks
  t)

;;; lambda list declares method's scope via specialised
;;; parameters. methods are applicable if there is parity between the
;;; method's specialised parameters and the corresponding generic
;;; function's argument list.


;;;; the book's implementation of the simple lock is predicated on
;;;; their use of some imagined multiprocess primitives. this was,
;;;; after all, 1991. the berlin wall had fallen only a few short
;;;; years ago, iraq was invaded, and linux was just a gleam in Linus'
;;;; eye. okay, it was probably in some realised albeit nascent state,
;;;; but the point is multiprocessor primitives were likely not
;;;; common. although, the book was published by mit press and written
;;;; by an employee of symbolics... i digress. i will fake some of the
;;;; multiprocessor code.
(defvar *lock-current-process* nil "place holder for current process")

(defun switch-process (p)
  (setq *lock-current-process* p))
(defun current-process () *lock-current-process*)
(defun current-processp (p) (equal *lock-current-process* p))

;;; now we add some code for the simple-lock
(defmethod locked-by-current-processp ((l simple-lock) process)
  (when (eql (lock-owner l) process)
    (error "can't seize ~a: already seized by current process." l)))

(defmacro set-if (value oldval newval)
  `(cond ((equal ,value ,oldval) (setf ,value ,newval))
         (t nil)))

(defmethod seize ((l simple-lock))
  (locked-by-current-processp l *lock-current-process*)
  ;; in a real lock, this would be a in a do that would block until
  ;; the lock is acquired, i.e.

  ;; (do ()
  ;;     ((set-if (lock-owner l) nil *lock-current-process))
  ;;   (process-wait "Seizing lock" #'(lambda () (null (lock-owner l)))))
  ;;  l)
  (set-if (lock-owner l) nil *lock-current-process*))

(defmethod release ((l simple-lock) &optional (failure-mode :no-error))
  (or (set-if (lock-owner l) *lock-current-process* nil)
      (ecase failure-mode
        (:no-error nil)
        (:error (error "~a is not owned by this processes" l)))))
