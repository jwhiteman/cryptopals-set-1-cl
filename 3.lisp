(ql:quickload :cl-ppcre)

(defvar input
  (concatenate 'string "1b37373331363f78151b7f2b783431333d"
                       "78397828372d363c78373e783a393b3736"))

(defun decrypt (input key-byte)
  (format nil "~{~a~}"
          (mapcar #'(lambda (hex)
                      (code-char
                        (logxor (parse-integer hex :radix 16)
                                key-byte)))
                  (cl-ppcre:all-matches-as-strings ".." input))))

(defun score (str)
  (length
    (cl-ppcre:all-matches-as-strings "(?i)[etaoin shrldu]" str)))

(let ((msg)
      (max 0))
  (do ((i 0 (1+ i)))
      ((> i 127) (format t "~A~%" msg))
    (let* ((res (decrypt input i))
           (scr (score res)))
      (when (> scr max)
        (setf msg res
              max scr)))))

;; > "Cooking MC's like a pound of bacon"
