(ql:quickload :cl-ppcre)

(defun duplicate-blocks? (line &optional (block-size 16))
  (let ((seen (make-hash-table :test #'equal))
        (num-blocks (/ (length line) block-size)))
    (do ((i 0 (1+ i)))
      ((= i num-blocks) nil)
      (let ((block-n (subseq line
                             (* i block-size)
                             (* (1+ i) block-size))))
        (if (gethash block-n seen)
          (return-from duplicate-blocks? t)
          (setf (gethash block-n seen) t))))))

(with-open-file (instream "8.txt")
  (do ((line (read-line instream nil :eof)
             (read-line instream nil :eof))
       (line-number 1 (1+ line-number)))
    ((equal line :eof))
    (when (duplicate-blocks? line)
      (format t "~%Duplicate blocks detected on line ~A: ~A~%" line-number line))))

;; > "Duplicate blocks detected on line 133: d88061974..."
