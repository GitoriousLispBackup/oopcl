;;;; chapter 4: implementation choices: methods v. slots

(defclass shape () ())

(defclass triangle (shape)
  ((side-a :accessor side-a :initarg :size-a)
   (side-b :accessor side-b :initarg :size-b)
   (side-c :accessor side-c :initarg :size-c)
   (number-of-sides :reader number-of-sides
                    :initform 3
                    :allocation :class)
   (area :reader area :initarg :area)))

(defun three-sides-to-angle (a b c)
  (acos (/ (- (+ (expt b 2) (expt c 2))
              (expt a 2))
              (* 2 b c))))

(defun area-of-triangle (a b c)
  (let ((angle-C (three-sides-to-angle c a b)))
    (* a b (sin angle-C) .5)))

(defun make-triangle (a b c)
  (make-instance 'triangle :size-a a
                           :size-b b
                           :size-c c
                           :area (area-of-triangle a b c)))

;;; alternatively, area via method (no area slot)
;; (defmethod area ((tri triangle))
;;   (area-of-triangle (side-a tri)
;;                     (side-b tri)
;;                     (side-c tri)))
;;; also note that :after methods should be defined to update the area
;; when side-a, side-b, and size-c are changed. the default
;; reader/writer can be overridden, but it is almost always better to
;; just use an after/before method.

;;; the function slot-value can be used to read or write slot
;;; values. it also skips any before/after functions.

;;; three ways to access slots:

(defmethod angle-A ((tri triangle))
  (three-sides-to-angle (side-a tri)
                        (side-b tri)
                        (side-c tri)))

(defmethod angle-B ((tri triangle))
  (with-accessors ((a side-a)
                   (b side-b)
                   (c side-c))
      tri
      (three-sides-to-angle b a c)))

(defmethod angle-C ((tri triangle))
  (with-slots (side-a side-b side-c) tri
    (three-sides-to-angle side-c side-a side-b)))

;;; alternatively
;; (defmethod angle-C ((tri triangle))
;;   (three-sides-to-angle (slot-value tri 'side-c)
;;                         (slot-value tri 'side-a)
;;                         (slot-value tri 'side-b)))

(defvar *unit-triangle* (make-triangle 1 1 1))

;;; trying to read an unbound slot is an error, governed by the
;;; slot-unbound generic.

;;;; multimethods: specialise more than one parameter
;;; example: installation scenario. two software products, adventure
;;; and life, that need to be installed on genera and unix. in
;;; the future, more products and operating systems might be
;;; supported, so extensibility is a requirement.
;;; software is represented by the life and adventure classes, built
;;; on the super basic-product.
;;; operating systems represented by the generic and unix classes,
;;; built on the super basic-os.

(defclass basic-product () ()
  (:documentation "superclass for all software products."))

(defclass life (basic-product) ()
  (:documentation "life, in all its Lispy glory."))

(defclass adventure (basic-product) ()
  (:documentation "adventures in the land of lisp..."))

(defclass basic-os () ()
  (:documentation "superclass for all operating systems."))

(defclass genera (basic-os) ()
  (:documentation "Symbolic's flagship."))

(defclass unix (basic-os) ()
  (:documentation "Worse is better..."))

;;; top level function for installing any product on any os
(defgeneric install (software-product operating-system)
  (:documentation "Install the product on the operating system."))

(defmethod install ((sw basic-product) (os basic-os))
  (format t "generic installation code."))

(defmethod install ((sw basic-product) non-os)
  (error "Cannot install because ~A is not a supported operating system." non-os))

(defmethod install (non-product (os basic-os))
  (error "Cannot install because ~A is not recognised as a valid product." non-product))

(defmethod install (non-product non-os)
  (error "Cannot install: ~A is not a recognised product, ~A is not a supported operating system."
         non-product non-os))

(defvar *genera* (make-instance 'genera))
(defvar *unix* (make-instance 'unix))
(defvar *life* (make-instance 'life))
(defvar *adventure* (make-instance 'adventure))

(defclass ms-word () ())
(defvar *ms-word* (make-instance 'ms-word))
(defclass ms-win () ())
(defvar *ms-win* (make-instance 'ms-win))

(defmethod install ((sw basic-product) (os basic-os))
  (restore-product sw os)
  (compile-product sw os)
  (configure-site sw os)
  (verify-product sw os))

(defgeneric get-source-pathname (product os)
  (:documentation "Returns a string."))

(defmethod get-source-pathname ((sw life) (os unix))
  "/bin/games/life.lisp")

(defmethod get-source-pathname ((sw adventure) (os unix))
  "/bin/games/adventure.lisp")

(defmethod get-source-pathname ((sw life) (os genera))
  "sys:games;life.lisp")

(defmethod get-source-pathname ((sw adventure) (os genera))
  "sys:games;adventure.lisp")

(defgeneric restore-product (sw os))
(defgeneric compile-product (sw os))
(defgeneric configure-site (sw os))
(defgeneric verify-product (sw os))

(defmethod restore-product (sw (os genera))
  (format t "genera-specific functions: loading from ~a" (get-source-pathname sw os)))

(defmethod restore-product (sw (os unix))
  (format t "unix-specific functions: loading from ~a" (get-source-pathname sw os)))
