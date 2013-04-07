;;;; outland-data.lisp

(in-package #:outland-data)

(defmethod cffi::slot-count ((object t))
  (declare (ignore object))
  -1)

(defclass fdata nil
  ((pointer :initform nil :initarg :pointer :reader pointer)
   (len :initform nil :initarg :len :reader f-length)
   (type :initform nil :initarg :type :reader f-type)))

(defun foreign-wrap (ptr type &optional (count -1))
  (make-instance 'fdata :pointer ptr :type type :len count))

;; [TODO] Add follow pointers feature.
(defun fslot (slot fval)
  (let ((slot-type (cffi:foreign-slot-type (f-type fval) slot)))
    (if (eql -1 (f-length fval)) 
        (make-instance
         'fdata
         :pointer (cffi:foreign-slot-pointer (pointer fval) 
                                             (f-type fval)
                                             slot)
         :type slot-type
         :len (cffi:foreign-slot-count (f-type fval) slot))
        (error "Cannot get slot of foreign array"))))

;; [TODO] support more dimensions
(defun aref-f (array &rest subscripts)
  (let ((1d (first subscripts)))
    (make-instance
     'fdata
     :pointer (cffi:mem-aptr (pointer array) (f-type array) 1d)
     :type (f-type array)
     :len -1)))

(defmethod print-object ((object fdata) stream)
  (if (> (f-length object) -1)
      (format stream "#.<GL-ARRAY :type ~s :length ~a>"
              (slot-value object 'type)
              (slot-value object 'len))
      (format stream "#.<GL-VALUE :type ~s>"
              (slot-value object 'type))))
