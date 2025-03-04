;;; Carbutti Lucia, Cividini Diego

; Huffman codes
;; Mandatory Method names
;;; hucodec-decode bits huffman-tree -> message (list of symbols)
;;; hucodec-encode message huffman-tree -> bits (list of 0s and 1s)
;;; hucodec-encode-file filename huffman-tree -> bits (list of 0s and 1s)
;;; hucodec-generate-huffman-tree symbols-n-weights (list of (symbol . weight)) -> huffman-tree
;;; hucodec-generate-symbol-bits-table huffman-tree -> symbol-bits-table (list of (symbols . bits))
;;; hucodec-print-huffman-tree huffman-tree &optional (indent 0) -> NIL

(defun flatten-list (lst)
  "Flatten a nested list into a simple list."
  (cond
    ((null lst) nil)
    ((listp (first lst))
     (append (flatten-list (first lst)) (flatten-list (rest lst))))
    (t (cons (first lst) (flatten-list (rest lst))))))

(defun hucodec-generate-huffman-tree (symbols-n-weights)
  "Generate a Huffman tree from a list of (symbol . weight) pairs."
  (labels ((merge-nodes (queue)
             (if (<= (length queue) 1)
                 queue
                 (let* ((sorted-queue (sort queue #'< :key #'car))
                        (left (pop sorted-queue))
                        (right (pop sorted-queue))
                        (new-node (list (+ (car left) (car right)) left right)))
                   (merge-nodes (cons new-node sorted-queue))))))
    (first (merge-nodes (mapcar (lambda (sw)
                                  (list (cdr sw)
                                        (if (symbolp (car sw))
                                            (char (symbol-name (car sw)) 0)
                                            (car sw))))
                                symbols-n-weights)))))

(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((traverse (node prefix)
             (if (characterp (second node))
                 (list (cons (second node) prefix))
                 (append (traverse (second node) (append prefix '(0)))
                         (traverse (third node) (append prefix '(1)))))))
    (traverse huffman-tree '())))

(defun hucodec-encode (message huffman-tree)
  (let ((symbol-bits-table (hucodec-generate-symbol-bits-table huffman-tree)))
    (apply #'append (mapcar (lambda (item)
                              (let ((char (if (symbolp item)
                                              (char (symbol-name item) 0)
                                              item)))
                                (cdr (assoc char symbol-bits-table))))
                            (flatten-list message)))))

(defun hucodec-decode (bits huffman-tree)
  (let ((node huffman-tree)
        (message '()))
    (dolist (bit bits)
      (setf node (if (zerop bit)
                     (second node)
                     (third node)))
      (when (characterp (second node))
        (push (second node) message)
        (setf node huffman-tree)))
    (nreverse message)))

(defun hucodec-print-huffman-tree (huffman-tree &optional (indent 0))
  (labels ((collect-symbols (node)
             (if (characterp (second node))
                 (list (second node))
                 (append (collect-symbols (second node))
                         (collect-symbols (third node)))))
           (print-node (node indent)
             (if (characterp (second node))
                 (format t "~&~v@TNode: ~a, Weight: ~a" indent (second node) (car node))
                 (progn
                   (format t "~&~v@TNode: ~a, Weight: ~a" indent (collect-symbols node) (car node))
                   (print-node (second node) (+ indent 2))
                   (print-node (third node) (+ indent 2))))))
    (print-node huffman-tree indent)))

(defun file-to-char-list (stream)
  "Read the content of a file and convert it to a list of characters."
  (let ((char (read-char stream nil :end-of-file)))
    (if (eql char :end-of-file)
        nil
        (cons char (file-to-char-list stream)))))

(defun hucodec-encode-file (filename huffman-tree)
  "Encode the content of a file using a Huffman tree."
  (with-open-file (stream filename :direction :input :if-does-not-exist :error)
    (let ((message (file-to-char-list stream)))
      (format t "Message from file: ~a~%" message)
      (hucodec-encode message huffman-tree))))

(defun run-test ()
  "Run a test with the input 'adde' and the list of symbol weights '((#\a . 1) (#\e . 1) (#\d . 2))'."
  (let ((symbols-n-weights '((#\a . 1) (#\e . 1) (#\d . 2) (#\Space . 1)))
        (message '((#\a #\a) (#\d #\d) #\e)))
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
            (format t "Encoded File Bits: ~a~%" encoded-file-bits)
            (let ((decoded-file-message (hucodec-decode encoded-file-bits huffman-tree)))
              (format t "Decoded File Message: ~a~%" decoded-file-message))))))))

;; Example usage
(run-test)