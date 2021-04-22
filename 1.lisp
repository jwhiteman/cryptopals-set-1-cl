(ql:quickload :cl-ppcre)
(ql:quickload :cl-base64)

(defvar input
  (concatenate 'string "49276d206b696c6c696e6720796f75722062"
	       "7261696e206c696b65206120706f69736f6e"
	       "6f7573206d757368726f6f6d"))

(defvar expected-output
  (concatenate 'string "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtl"
	       "IGEgcG9pc29ub3VzIG11c2hyb29t"))

(let ((result
        (cl-base64:string-to-base64-string
          (format nil "~{~a~}"
                  (mapcar #'(lambda (h)
                              (code-char (parse-integer h :radix 16)))
                          (cl-ppcre:all-matches-as-strings ".." input))))))
  (format t (if (equal result expected-output)
              "pass~%"
              "fail~%")))
