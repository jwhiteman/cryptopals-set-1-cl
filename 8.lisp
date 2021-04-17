(ql:quickload :cl-ppcre)

(defun to-bytes (hexs)
  (mapcar #'(lambda (hex)
	      (parse-integer hex :radix 16))
	  (cl-ppcre:all-matches-as-strings ".." hexs)))

(defun ecb? (line)
  (catch 'result
    (labels ((_ (i acc gacc lst)
	       (cond ((null lst) nil)
		     ((= i 16)
		      (if (member acc gacc :test #'equal)
			  (throw 'result t)
			  (_ 0 nil (cons acc gacc) lst)))
		     (t
		      (_ (1+ i) (cons (car lst) acc) gacc (cdr lst))))))
      (_ 0 nil nil (to-bytes line)))))

(with-open-file (instream "8.dat")
  (do ((line (read-line instream nil :eof)
	     (read-line instream nil :eof))
       (line-number 1 (1+ line-number)))
      ((equal line :eof))
    (when (ecb? line)
      (format t "~%ECB detected on line ~A!: ~A~%" line-number line))))

;; > "ECB detected on line 133!: d88061974..."
