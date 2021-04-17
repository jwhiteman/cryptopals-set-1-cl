(ql:quickload :cl-ppcre)

(defvar input "1c0111001f010100061a024b53535009181c")
(defvar key "686974207468652062756c6c277320657965")
(defvar expected-result "746865206b696420646f6e277420706c6179")

(defun hex-list (hex)
  (cl-ppcre:all-matches-as-strings ".." hex))

(defun fixed-xor (input key)
  (mapcar #'(lambda (l r)
              (logxor (parse-integer l :radix 16)
                      (parse-integer r :radix 16)))
          (hex-list input)
          (hex-list key)))

(let ((result (format nil "~(~{~x~}~)"
                      (fixed-xor input key))))
  (if (equal result expected-result)
    (format t "pass~%")
    (format t "fail~%")))
