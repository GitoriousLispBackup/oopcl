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

(defvar *null-lock*)
(defvar *simple-lock*)
(setq *simple-lock* (make-simple-lock "global simple lock"))

;;;; illustrating inheritance
(defun display-lock-inheritance ()
  (format t "type-of *null-lock*: ~a~%" (type-of *null-lock*))
  (format t "type-of *simple-lock*: ~a~%" (type-of *simple-lock*))
  (format t "simple-lock is a lock? ~a~%" (typep *simple-lock* 'lock))
  (format t "simple-lock is a null-lock? ~a~%" (typep *simple-lock* 'null-lock)))
