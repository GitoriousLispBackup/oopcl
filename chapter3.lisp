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
(switch-process :foo)
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

;;; defaults can't be specified in generics, but can be in methods.

;;;; overriding default print representation: CLOS specifies the
;;;; print-object generic function. it takes the object and a stream,
;;;; prints the object to the stream, and returns the object.
(defmethod print-object ((l lock) stream)
  (format stream "#<~S ~A>"
          (type-of l)
          (if (slot-boundp l 'name)
              (lock-name l)
              "(no name)")))

;;; would be helpful to provide a function for describing the
;;; object. could implement a show-lock function, the book mentions it
;;; is better to use the CLOS-provided describe: hackers already know
;;; it. however, modern day CLOS doesn't like this so we'll provide
;;; show-lock.

;; (defmethod describe ((l lock))
;;   (format t "~& ~S is a lock of type ~S name ~A."
;;           l (type-of l)
;;           (if (slot-boundp l 'name)
;;               (lock-name l)
;;               "(no name)"))
;;   (values))

(defmethod show-lock ((l lock))
  (format t "~& ~S is a lock of type ~S named ~A."
          l (type-of l)
          (if (slot-boundp l 'name)
              (lock-name l)
              "(no name)"))
  (values))

;;; after-methods allow more specialised results in the call chain.

(defmethod show-lock :after ((l simple-lock))
  (let ((owner (lock-owner l)))
    (if owner
        (format t "~&The lock is owned by ~A." owner)
        (format t "~&The lock is free."))
    owner))

;;; the first show-lock is the least specific and is called on the
;;; lock. the :after keyword tells the second show-lock to operate
;;; after the first. without this keyword, we would only see the
;;; second method.

;;; for example, null-lock's show-lock overrides the superclass's
;;; method.

(defmethod show-lock ((l null-lock))
  (format t "~&null lock ~A."
          (if (slot-boundp l 'name)
              (format nil "named ~A" (lock-name l))
              "with no name"))
  (values))

;;; rule 1 of class precedence: a class always has higher priority
;;; over its superclass.

;;;; mixin/aggregate classes

;;; order-lock-mixin is a mixin class; one that is designed to be
;;; combined with other classes to provide enhancements to their
;;; functionality.

(defclass ordered-lock-mixin ()
  ((level :initarg :level
          :reader lock-level
          :type integer))
  (:documentation "Avoid deadlock by checking lock order."))

;;; The type option hints to CLOS what the type of the slot should
;;; be. There is no guarantee that this will be enforced.

;;; two aggregate classes: classes built from multiple superclasses.
(defclass ordered-lock (ordered-lock-mixin simple-lock)
  ()
  (:documentation
"Avoids deadlock by ensuring that a process seizes locks in a specific
order. When seizing, waits if the lock is busy."))

(defclass ordered-null-lock (ordered-lock-mixin null-lock)
  ()
  (:documentation
"Avoids deadlock by ensuring that a process seizes locks in a specific
order. Does not actually seize anything, but does check that the lock
ordering is obeyed."))

;;; aggregate classes rarely include additional methods; instead those
;;; methods are contained in the superclasses.
(defun make-ordered-null-lock (name level)
  (make-instance 'ordered-null-lock :name name
                                    :level level))

(defun make-ordered-lock (name level)
  (make-instance 'ordered-lock :name name
                               :level level))

;;; rule 2 of class precedence: each class definition sets the
;;; precedence order of its direct superclass.

;;; this precedence is given in the defclass. therefore, the class
;;; precedence of ordered-lock is:
;;;   ordered-lock -> ordered-lock-mixin -> simple-lock lock
;;;   standard-object -> t

(defmethod show-lock :after ((l ordered-lock-mixin))
  (format t "~&Its lock level is ~D." (lock-level l)))

(defvar *ordered-lock* (make-ordered-lock "ordered lock" 3))

;;;; implementing ordered lock table behaviour

(defvar *process-lock-table* (make-hash-table)
  "Each key is a process identifier; value is a list of ordered locks it owns.")

;; the next three functions should have some sort of process
;; preemption prevention.
(defun add-process-lock (process lock)
  (push lock (gethash process *process-lock-table*)))

(defun delete-process-lock (process lock)
  (let ((hash-entry
         (gethash process *process-lock-table*)))
    (setf (gethash process *process-lock-table*)
          (delete lock hash-entry))))

(defun get-process-locks (process)
  (gethash process *process-lock-table*))

;; relies on process lock list being ordered
(defun get-highest-lock (process)
  (first (get-process-locks process)))

(defmethod seize :before ((l ordered-lock-mixin))
  (locked-by-current-processp l *lock-current-process*)
  (let ((highest-lock (get-highest-lock *lock-current-process*)))
    (when (and highest-lock
               (<= (lock-level l) (lock-level highest-lock)))
      (error "Out of order: Can't seize ~A while owning ~A"
             l highest-lock))))

(defmethod seize :after ((l ordered-lock-mixin))
  (add-process-lock *lock-current-process* l))

(defmethod release :after ((l ordered-lock-mixin) &optional failure-mode)
  (declare (ignore failure-mode))
  (delete-process-lock *lock-current-process* l))

;;;; example: print queue

(defclass print-request-queue ()
  ((lock :accessor print-queue-lock
          :initform (make-simple-lock "Print Queue"))
   (requests :accessor print-requests :initform nil)))

(defun make-print-queue () (make-instance 'print-request-queue))
(defvar *print-queue* (make-print-queue))

(defun enqueue-print-request (r)
  (let ((lock (print-queue-lock *print-queue*)))
    (unwind-protect
         (progn (seize lock)
                (push r (print-requests *print-queue*)))
      (release lock :no-error))))

(defun dequeue-print-request (r)
  (let ((lock (print-queue-lock *print-queue*)))
    (unwind-protect
         (progn
           (seize lock)
           (setf (print-requests *print-queue*)
                 (delete r (print-requests *print-queue*))))
      (release lock :no-error))))

(defmacro with-lock ((lock) &body body)
  (let ((lock-var (gensym)))
    `(let ((,lock-var ,lock))
       (unwind-protect
            (progn (seize ,lock-var)
                   ,@body)
         (release ,lock-var :no-error)))))

(defun enqueue-print-request% (r)
  (with-lock ((print-queue-lock *print-queue*))
    (push r (print-requests *print-queue*))))

(defun dequeue-print-request% (r)
  (with-lock ((print-queue-lock *print-queue*))
    (setf (print-requests *print-queue*)
          (delete r (print-requests *print-queue*)))))

;;; don't lock queue so a processing holding up the print queue will
;;; show up.
(defmethod show-lock ((queue print-request-queue))
  (let ((owner (lock-owner (print-queue-lock queue)))
        (requests (print-requests queue)))
    (if owner
        (format t "~&Process ~A owns queue.~%" owner))
    (format t (if (null requests)
                  "~&There are no print requests.~%"
                  "~&Pending print requests:~%"))
    (dolist (x requests)
      (format t "~&~A " x))))

;;;; no distinction is made between private and public elements, so
;;;; the API documentation should only describe public interfaces.

;;;; guidelines on designing protocols
;;;  - restrict user access to internal data structures.
;;;  - provide constructors to control data going into a new instance.
;;;  - design protocol to anticipate needs of the users.
;;;  - allow protocol to evolve to meet the reasonable needs of users.
;;;  - design for extensibility.
