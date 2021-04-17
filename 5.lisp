(defparameter input
  (format nil "~A~%~A"
          "Burning 'em, if you ain't quick and nimble"
          "I go crazy when I hear a cymbal"))

(defparameter key "ICE")

(defparameter expected-output
  (concatenate 'string "0b3637272a2b2e63622c2e69692a23693a2a3c"
                       "6324202d623d63343c2a26226324272765272a"
                       "282b2f20430a652e2c652a3124333a653e2b20"
                       "27630c692b20283165286326302e27282f"))

(defun str-bytes (key-str)
  (map 'vector #'char-code key-str))

(defun repeat-key-xor (input key)
  (let* ((key-bytes (str-bytes key))
         (input-bytes (str-bytes input))
         (input-length (length input-bytes))
         (key-length (length key-bytes))
         (result (make-array input-length)))
    (do ((i 0 (1+ i)))
        ((= i input-length) result)
      (setf (svref result i)
            (logxor (svref input-bytes i)
                    (svref key-bytes (mod i key-length)))))))

(let ((result (format nil "~(~{~2,'0x~}~)"
                      (coerce (repeat-key-xor input key)
                              'list))))
  (if (equal result expected-output)
    (format t "pass~%")
    (format t "fail~%")))

;; > pass
