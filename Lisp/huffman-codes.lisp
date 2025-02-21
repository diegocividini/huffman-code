;; filepath: /Users/diegocividini/Desktop/progetto_lp/Lisp/huffman-codes.lisp
;;; Carbutti Lucia, Cividini Diego

; Huffman codes
;; Mandatory Method names
;;; hucodec-decode bits huffman-tree -> message (list of symbols)
;;; hucodec-encode message huffman-tree -> bits (list of 0s and 1s)
;;; hucodec-encode-file filename huffman-tree -> bits (list of 0s and 1s)
;;; hucodec-generate-huffman-tree symbols-n-weights (list of sw(symbol . weight)) -> huffman-tree
;;; hucodec-generate-symbol-bits-table huffman-tree -> symbol-bits-table (list of sb(symbols . bits))
;;; hucodec-print-huffman-tree huffman-tree &optional (indent 0) -> NIL




;;; Carbutti Lucia, Cividini Diego

; Huffman codes
;; Mandatory Method names
;;; hucodec-decode bits huffman-tree -> message (list of symbols)
(defun hucodec-decode (bits huffman-tree)
  (labels ((decode-helper (bits node)
             (cond ((null bits) (if (atom node) (list node) nil))
                   ((atom node) (cons node (decode-helper bits huffman-tree)))
                   (t (decode-helper (cdr bits) (if (eq (car bits) 0)
                                                    (car node)
                                                    (cdr node)))))))
    (decode-helper bits huffman-tree)))

;;; hucodec-encode message huffman-tree -> bits (list of 0s and 1s)
(defun hucodec-encode (message huffman-tree)
  (let ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
    (apply #'append (mapcar (lambda (symbol)
                              (cdr (assoc symbol symbol-bits-table)))
                            message))))

;;; hucodec-encode-file filename huffman-tree -> bits (list of 0s and 1s)
(defun hucodec-encode-file (filename huffman-tree)
  (with-open-file (stream filename)
    (let ((content (read-line stream)))
      (hucodec-encode (coerce content 'list) huffman-tree))))

;;; hucodec-generate-huffman-tree symbols-n-weights (list of sw(symbol . weight)) -> huffman-tree
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  (labels ((merge-trees (trees)
             (if (null (cdr trees))
                 (car trees)
                 (let* ((sorted-trees (sort trees #'< :key #'cdr))
                        (first (car sorted-trees))
                        (second (cadr sorted-trees))
                        (rest (cddr sorted-trees))
                        (new-tree (cons (cons (car first) (car second)) (+ (cdr first) (cdr second)))))
                   (merge-trees (cons new-tree rest))))))
    (car (merge-trees (mapcar (lambda (sw) (cons (car sw) (cdr sw))) symbols-n-weights)))))

;;; hucodec-generate-symbol-bits-table huffman-tree -> symbol-bits-table (list of sb(symbols . bits))
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((generate-table (node prefix)
             (if (atom node)
                 (list (cons node prefix))
                 (append (generate-table (car node) (append prefix '(0)))
                         (generate-table (cdr node) (append prefix '(1)))))))
    (generate-table huffman-tree '())))

;;; hucodec-print-huffman-tree huffman-tree &optional (indent 0) -> NIL
(defun hucodec-print-huffman-tree (huffman-tree &optional (indent 0))
  (labels ((print-tree (node indent)
             (if (atom node)
                 (format t "~&~v@T~A" indent node)
                 (progn
                   (print-tree (car node) (+ indent 2))
                   (print-tree (cdr node) (+ indent 2))))))
    (print-tree huffman-tree indent)))

;;; Test cases for Huffman coding methods

(defun run-tests ()
  (let* ((symbols-n-weights '((a . 1) (e . 1) (d . 2)))
         (huffman-tree (hucodec-generate-huffman-tree symbols-n-weights))
         (symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree))
         (message '(a d d e))
         (encoded-message (hucodec-encode message huffman-tree))
         (decoded-message (hucodec-decode encoded-message huffman-tree))
         (test-filename "/Users/diegocividini/Desktop/file.txt")
         (encoded-file-message (hucodec-encode-file test-filename huffman-tree)))

    ;; Test hucodec-generate-huffman-tree
    (format t "Huffman Tree: ~a~%" huffman-tree)

    ;; Test hucodec-generate-symbol-bits-table
    (format t "Symbol Bits Table: ~a~%" symbol-bits-table)

    ;; Test hucodec-encode
    (format t "Encoded Message: ~a~%" encoded-message)

    ;; Test hucodec-decode
    (format t "Decoded Message: ~a~%" decoded-message)

    ;; Test hucodec-print-huffman-tree
    (format t "Huffman Tree Structure:~%")
    (hucodec-print-huffman-tree huffman-tree)

    ;; Test hucodec-encode-file
    (format t "Encoded File Message: ~a~%" encoded-file-message)

    ;; Check if the decoded message matches the original message
    (if (equal message decoded-message)
        (format t "Test Passed: Decoded message matches the original message.~%")
        (format t "Test Failed: Decoded message does not match the original message.~%"))

    ;; Check if the encoded file message matches the encoded message
    (if (equal encoded-message encoded-file-message)
        (format t "Test Passed: Encoded file message matches the encoded message.~%")
        (format t "Test Failed: Encoded file message does not match the encoded message.~%"))))

(run-tests)