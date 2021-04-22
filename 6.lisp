(ql:quickload :cl-base64)
(ql:quickload :cl-ppcre)

(defun to-bytes (str)
  (map 'list #'char-code str))

(defvar *bytes*
  (with-open-file (instream "6.txt")
    (to-bytes
      (cl-base64:base64-stream-to-string instream))))

(defun different-right-bit? (byte1 byte2)
  (/= (logand byte1 1) (logand byte2 1)))

(defun byte-distance (byte1 byte2)
  (labels ((_ (b1 b2 i acc)
              (if (zerop i)
                acc
                (_ (ash b1 -1)
                   (ash b2 -1)
                   (1- i)
                   (if (different-right-bit? b1 b2)
                     (1+ acc)
                     acc)))))
    (_ byte1 byte2 8 0)))

(defun hamming (bytes1 bytes2)
  (labels ((_ (bytes1 bytes2 acc)
              (if (null bytes1)
                acc
                (_ (cdr bytes1)
                   (cdr bytes2)
                   (+ acc (byte-distance (car bytes1)
                                         (car bytes2)))))))
    (_ bytes1 bytes2 0)))

(defun in-slices (fn n lst)
  (labels ((_ (i lacc lst prev)
              (cond ((null lst) nil)
                    ((= i n)
                     (let ((cur (nreverse lacc)))
                       (funcall fn prev cur)
                       (_ 0 nil lst cur)))
                    (t
                      (_ (1+ i)
                         (cons (car lst) lacc)
                         (cdr lst)
                         prev)))))
    (_ 0 nil lst nil)))

(defun map-slice (n lst)
  (let (acc)
    (in-slices #'(lambda (prev cur)
                   (declare (ignore prev))
                   (push cur acc))
               n
               lst)
    (nreverse acc)))

(defun most-likely-key-size (bytes)
  (let (gacc)
    (do ((key-size 2 (1+ key-size)))
        ((= key-size 40))
      (let ((acc 0)
            (cnt 0))
        (in-slices #'(lambda (bytes1 bytes2)
                       (when (and bytes1 bytes2)
                         (incf acc (/ (hamming bytes1 bytes2) key-size))
                         (incf cnt)))
                   key-size
                   bytes)
        (push (cons (/ acc cnt) key-size) gacc)))
    (cdar (sort gacc #'< :key #'car))))

(defun repeat-key-xor (bytes key)
  (format nil "~{~A~}"
          (mapcar #'(lambda (byte)
                      (code-char (logxor byte key)))
                  bytes)))

(defun score (str)
  (length
    (cl-ppcre:all-matches-as-strings "(?i)[etaoin shrldu]" str)))

;; crack the key:
(format t "~{~A~}~%"
        (mapcar #'(lambda (bytes)
                    (let ((max 0)
                          (the-key nil))
                      (do ((key-guess 0 (1+ key-guess)))
                          ((= key-guess 127) (code-char the-key))
                        (let ((res (score
                                     (repeat-key-xor bytes key-guess))))
                          (when (> res max)
                            (setf max res
                                  the-key key-guess))))))
                (apply #'mapcar
                       #'list
                       (map-slice
                         (most-likely-key-size *bytes*) *bytes*))))

;; > "Terminator X: Bring the noise"
