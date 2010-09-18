#|
interpolate.lisp
provides String Interpolation
inspired by Scheme Interpreter "Gauche" <practical-scheme.net>
|#

(defpackage interpolate
  (:use :common-lisp)
  (:export :string-interpolate))

(in-package :interpolate)

(defun %string-interpolate (str)
  (labels ((rec (in acc)
	     (let ((c (read-char in nil)))
	       (cond ((null c) (list (get-output-stream-string acc)))
		     ((char= c #\,)
		      (let ((c2 (peek-char nil in nil)))
			(cond ((null c2) (write-char c acc) (rec in acc))
			      ((char= #\, c2)
			       (write-char (read-char in nil) acc) (rec in acc))
			      (t (cons (get-output-stream-string acc) (insert in acc))))))
		     (t (write-char c acc) (rec in acc)))))
           (insert (in acc)
	     (let* ((form (read-preserving-whitespace in t))
		    (rest (rec in acc)))
	       (cons `(format nil "~A" ,form) rest))))
    `(concatenate 'string ,@(rec (make-string-input-stream str)
				 (make-string-output-stream)))))

(defun string-interpolate (str)
(if (stringp str)
    (%string-interpolate str)
  (error "malformed string-interpolate: ~s"
	 `(string-interpolate ,str))))

(defun |#`-reader| (stream sub-char numarg)
  (declare (ignore sub-char numarg))
  (let ((string (read-preserving-whitespace stream t nil t)))
    (string-interpolate string)))

(set-dispatch-macro-character #\# #\` #'|#`-reader|)