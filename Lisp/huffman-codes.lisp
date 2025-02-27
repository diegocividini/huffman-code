;; filepath: /Users/diegocividini/Desktop/progetto_lp/Lisp/huffman-codes.lisp
;;; Carbutti Lucia, Cividini Diego

; Huffman codes
;; Mandatory Method names
;;; hucodec-decode bits huffman-tree -> message (list of symbols)
;;; hucodec-encode message huffman-tree -> bits (list of 0s and 1s)
;;; hucodec-encode-file filename huffman-tree -> bits (list of 0s and 1s)
;;; hucodec-generate-huffman-tree symbols-n-weights (list of (symbol . weight)) -> huffman-tree
;;; hucodec-generate-symbol-bits-table huffman-tree -> symbol-bits-table (list of (symbols . bits))
;;; hucodec-print-huffman-tree huffman-tree &optional (indent 0) -> NIL


(defun hucodec-generate-huffman-tree (symbols-n-weights)
  "Generate a Huffman tree from a list of (symbol . weight) pairs."
  (let ((queue (mapcar (lambda (sw) (list (cdr sw) (car sw))) symbols-n-weights)))
    (loop while (> (length queue) 1) do
         (setf queue (sort queue #'< :key #'car))
         (let* ((left (pop queue))
                (right (pop queue))
                (new-node (list (+ (car left) (car right)) left right)))
           (push new-node queue)))
    (first queue)))

(defun hucodec-generate-symbol-bits-table (huffman-tree)
  "Generate a symbol-bits table from a Huffman tree."
  (labels ((traverse (node prefix)
             (if (symbolp (second node))
                 (list (cons (second node) prefix))
                 (append (traverse (second node) (append prefix '(0)))
                         (traverse (third node) (append prefix '(1)))))))
    (traverse huffman-tree '())))

(defun hucodec-encode (message huffman-tree)
  "Encode a message (list of symbols) using a Huffman tree."
  (let ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
    (apply #'append (mapcar (lambda (symbol)
                              (cdr (assoc symbol symbol-bits-table)))
                            message))))

(defun hucodec-decode (bits huffman-tree)
  "Decode a list of bits using a Huffman tree."
  (let ((node huffman-tree)
        (message '()))
    (dolist (bit bits)
      (setf node (if (zerop bit)
                     (second node)
                     (third node)))
      (when (symbolp (second node))
        (push (second node) message)
        (setf node huffman-tree)))
    (nreverse message)))

(defun hucodec-print-huffman-tree (huffman-tree &optional (indent 0))
  "Print the Huffman tree in a user-friendly way."
  (labels ((collect-symbols (node)
             (if (symbolp (second node))
                 (list (second node))
                 (append (collect-symbols (second node))
                         (collect-symbols (third node)))))
           (print-node (node indent)
             (if (symbolp (second node))
                 (format t "~&~v@TNode: ~a, Weight: ~a" indent (second node) (car node))
                 (progn
                   (format t "~&~v@TNode: ~a, Weight: ~a" indent (collect-symbols node) (car node))
                   (print-node (second node) (+ indent 2))
                   (print-node (third node) (+ indent 2))))))
    (print-node huffman-tree indent)))

(defun file-to-symbol-list (stream)
  "Read the content of a file and convert it to a list of symbols."
  (let ((content (read-char stream nil 'end))) ;; Properly handle EOF
    (if (eq content 'end)
        nil
        (cons (intern (string-upcase (string content)))  ;; Ensure consistent symbol case
              (file-to-symbol-list stream)))))  ;; Recursive call


(defun hucodec-encode-file (filename huffman-tree)
  "Encode the content of a file using a Huffman tree."
  (with-open-file (stream filename 
                  :direction :input 
                  :if-does-not-exist :error)
  (let ((message (file-to-symbol-list stream)))
    (format t "Message from file: ~a~%" message)
    (hucodec-encode message huffman-tree))))

(defun run-test ()
  "Run a test with the input 'adde' and the list of symbol weights '((:a . 1) (:e . 1) (:d . 2))'."
  (let ((symbols-n-weights '((a . 1) (e . 1) (d . 2)))
        (message '(a d d e)))
    ;; Generate Huffman tree
    (let ((huffman-tree (hucodec-generate-huffman-tree symbols-n-weights)))
      ;; Print Huffman tree
      (format t "Huffman Tree:~%")
      (hucodec-print-huffman-tree huffman-tree)
      
      ;; Generate symbol-bits table
      (let ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
        (format t "~%Symbol-Bits Table: ~a~%" symbol-bits-table)
        
        ;; Encode message
        (let ((encoded-bits (hucodec-encode message huffman-tree)))
          (format t "Encoded Bits: ~a~%" encoded-bits)
          
          ;; Decode bits
          (let ((decoded-message (hucodec-decode encoded-bits huffman-tree)))
            (format t "Decoded Message: ~a~%" decoded-message)))
        
        ;; Encode file content
        (let ((filename "/Users/diegocividini/Desktop/file.txt"))
          ;; Encode the file content
          (let ((encoded-file-bits (hucodec-encode-file filename huffman-tree)))
            (format t "Encoded File Bits: ~a~%" encoded-file-bits)))))))

;; Example usage
(run-test)