(ql:quickload :ironclad)
(ql:quickload :cl-base64)

(defun str->bytes (keystr)
  (map '(vector (unsigned-byte 8)) #'char-code keystr))

(defun ecb (keystr)
  (ironclad:make-cipher :AES :key (str->bytes keystr) :mode :ECB))

;; NOTE: key & str both need to be 16 chars long
(defun aes-ecb-encrypt (key str)
  (let ((str-bytes (str->bytes str))
        (cipher (ecb key)))
    (ironclad:encrypt-in-place cipher str-bytes)
    (values (map 'string #'code-char str-bytes)
            str-bytes)))

(defun aes-ecb-decrypt (key str)
  (let ((str-bytes (str->bytes str))
        (cipher (ecb key)))
    (ironclad:decrypt-in-place cipher str-bytes)
    (values (map 'string #'code-char str-bytes)
            str-bytes)))

(with-open-file (instream "7.txt")
  (let ((c1 (cl-base64:base64-stream-to-string instream)))
    (format t "~a" (decrypt "YELLOW SUBMARINE" c1))))

;; > "I'm back and I'm ringin' the bell"...
