

(in-package #:mop)

;;; AMOP pg. 240ff.
(defgeneric validate-superclass (class superclass)
 (:documentation 
  "This generic function is called to determine whether the class
  superclass is suitable for use as a superclass of class."))

(defmethod validate-superclass ((class class) (superclass class))
  (or (eql superclass +the-T-class+)
      (eql (class-of class) (class-of superclass))
      (or (and (eql (class-of class) +the-standard-class+)
               (eql (class-of superclass) +the-funcallable-standard-class+))
          (and (eql (class-of class) +the-funcallable-standard-class+)
               (eql (class-of superclass) +the-standard-class+)))))

;;; This is against the letter of the MOP, but very much in its spirit.
(defmethod validate-superclass ((class class)
                                (superclass forward-referenced-class))
  t)

(defmethod shared-initialize :before ((instance class)
                                      slot-names
                                      &key direct-superclasses
                                      &allow-other-keys)
  (declare (ignore slot-names))
  (dolist (superclass direct-superclasses)
    (assert (validate-superclass instance superclass) (instance superclass)
            "Class ~S is not compatible with superclass ~S"
            instance superclass)))


;;; Full-blown method combinations.

(defclass standard-method-combination (method-combination)
  ((options :initarg :options :reader method-combination-options)
   (%generic-functions
    :initform nil
    :accessor method-combination-%generic-functions)))

(defmethod update-generic-function-for-redefined-method-combination
    ((function standard-generic-function)
     (previous standard-method-combination)
     (current standard-method-combination))
  "Reinitialize FUNCTION."
  (reinitialize-instance function))

(defmethod update-instance-for-different-class :after
    ((previous standard-method-combination)
     (current standard-method-combination)
     &key &allow-other-keys)
  "Inform every generic function that its method combination has changed."
  (mapc
   (lambda (function)
     (update-generic-function-for-redefined-method-combination
      function previous current))
   (method-combination-%generic-functions current)))

(defclass short-method-combination (standard-method-combination)
  ())

(defclass long-method-combination (standard-method-combination)
  ())

(defclass method-combination-type (standard-class) ())

(defclass standard-method-combination-type (method-combination-type)
  ((type-name
    :initarg :type-name
    :reader method-combination-type-name)
   (lambda-list
    :initform nil
    :initarg :lambda-list
    :reader method-combination-type-lambda-list)
   (%constructor
    ;; A reader without "type" in the name seems more readable to me.
    :reader method-combination-%constructor)
   (%effective-method-builder
    :initarg :effective-method-builder
    :reader method-combination-type-%effective-method-builder)
   (%instances
    :initform (make-hash-table :test #'equal)
    :reader method-combination-type-%instances)
   (sys::%documentation
    :initarg :documentation
    :initform nil)))

(defclass short-method-combination-type (standard-method-combination-type)
  ((operator
    :initarg :operator
    :reader short-method-combination-type-operator)
   (identity-with-one-argument
    :initarg :identity-with-one-argument
    :reader short-method-combination-type-identity-with-one-argument)))

(defmethod validate-superclass
    ((class short-method-combination-type) (superclass standard-class))
  "Validate the creation of method combinations implemented as
SHORT-METHOD-COMBINATION-TYPE."
  (subtypep superclass 'short-method-combination))

(defclass long-method-combination-type (standard-method-combination-type)
  ((sys::lambda-list :initarg :lambda-list)
   (method-group-specs :initarg :method-group-specs)
   (args-lambda-list :initarg :args-lambda-list)
   (generic-function-symbol :initarg :generic-function-symbol)
   (function :initarg :function)
   (arguments :initarg :arguments)
   (declarations :initarg :declarations)
   (forms :initarg :forms)))

(defmethod validate-superclass
    ((class long-method-combination-type) (superclass standard-class))
  "Validate the creation of method combinations implemented as
LONG-METHOD-COMBINATION-TYPE."
  (subtypep superclass 'long-method-combination))


(defparameter *method-combination-types* (make-hash-table))

(defun find-method-combination-type (name &optional (errorp t))
  "Find a NAMEd method combination type.
If ERRORP (the default), signal an error if no such method combination type is
found. Otherwise, return NIL."
  (or (gethash name *method-combination-types*)
      (when errorp
	(error "There is no method combination type named ~A." name))))

(defmethod validate-superclass
    ((class standard-method-combination-type) (superclass standard-class))
  t)

(let* ((ssmc (make-instance 'standard-method-combination-type
	       :direct-superclasses (list (find-class 'standard-method-combination))
	       :type-name 'standard
	       :documentation "The standard method combination type."))
       (smc (make-instance ssmc :options nil)))
  (setf (method-combination-%generic-functions smc)
	(std-slot-value *the-standard-method-combination* '%generic-functions))
  (setf (gethash nil (method-combination-type-%instances ssmc)) smc)
  (setf (gethash 'standard *method-combination-types*) ssmc)
  (setq *the-standard-method-combination* smc)
  ;;  (print "### Size of cache:")
  ;;  (princ (length (method-combination-%generic-functions smc)))
  ;;  (terpri)
  #+()(mapc (lambda (gf) (setf (std-slot-value gf 'sys::%method-combination) smc))
	(method-combination-%generic-functions smc)))



(export '(;; classes
          funcallable-standard-object
          funcallable-standard-class
          forward-referenced-class
          slot-definition
          standard-method
          standard-accessor-method
          standard-reader-method
          standard-writer-method

          compute-effective-slot-definition
          compute-class-precedence-list
          compute-default-initargs
          compute-effective-slot-definition
          compute-discriminating-function
          compute-applicable-methods
          compute-applicable-methods-using-classes
          compute-effective-method
          make-method-lambda
          compute-slots
          finalize-inheritance
          validate-superclass

          slot-value-using-class
          slot-boundp-using-class
          slot-makunbound-using-class

          ensure-class
          ensure-class-using-class
          ensure-generic-function-using-class

          class-default-initargs
          class-direct-default-initargs
          class-direct-slots
          class-direct-subclasses
          class-direct-superclasses
          class-finalized-p
          class-precedence-list
          class-prototype
          class-slots

          add-direct-subclass
          remove-direct-subclass

          generic-function-argument-precedence-order
          generic-function-declarations
          generic-function-lambda-list
          generic-function-method-class
          generic-function-method-combination
          generic-function-name

          method-function
          method-generic-function
          method-lambda-list
          method-specializers
          method-qualifiers
          accessor-method-slot-definition

          reader-method-class
          writer-method-class

          direct-slot-definition-class
          effective-slot-definition-class
          slot-definition-allocation
          slot-definition-initargs
          slot-definition-initform
          slot-definition-initfunction
          slot-definition-location
          slot-definition-name
          slot-definition-readers
          slot-definition-type
          slot-definition-writers
          slot-definition-documentation

          standard-instance-access
          funcallable-standard-instance-access

          intern-eql-specializer
          eql-specializer-object
          specializer-direct-methods
          specializer-direct-generic-functions
          add-direct-method
          remove-direct-method

	  standard-method-combination
	  short-method-combination
	  long-method-combination
	  method-combination-type
	  standard-method-combination-type
	  short-method-combination-type
	  long-method-combination-type
          find-method-combination
	  update-generic-function-for-redefined-method-combination
          extract-lambda-list
          extract-specializer-names

          add-dependent
          remove-dependent
          map-dependents
          update-dependent))

(provide 'mop)





