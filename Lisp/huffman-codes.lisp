;;; Carbutti Lucia, Cividini Diego

; Huffman codes
;; Mandatory Method names
;;; hucodec-decode bits huffman-tree -> message
;;; hucodec-encode message huffman-tree -> bits
;;; hucodec-encode-file filename huffman-tree -> bits
;;; hucodec-generate-huffman-tree symbols-n-weights -> huffman-tree
;;; hucodec-generate-symbol-bits-table huffman-tree -> symbol-bits-table
;;; hucodec-print-huffman-tree huffman-tree &optional (indent 0) -> NIL

(defun flatten-list (lst)
  (cond
    ((null lst) nil)
    ((listp (first lst))
     (append (flatten-list (first lst)) (flatten-list (rest lst))))
    (t (cons (first lst) (flatten-list (rest lst))))))

(defun hucodec-generate-huffman-tree (symbols-n-weights)
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
                 (format t "~&~v@TNode: ~a, Weight: ~a"
			 indent (second node) (car node))
                 (progn
                   (format t "~&~v@TNode: ~a, Weight: ~a"
			   indent (collect-symbols node) (car node))
                   (print-node (second node) (+ indent 2))
                   (print-node (third node) (+ indent 2))))))
    (print-node huffman-tree indent)))

(defun file-to-char-list (stream)
  (let ((char (read-char stream nil :end-of-file)))
    (if (eql char :end-of-file)
        nil
        (cons char (file-to-char-list stream)))))

(defun hucodec-encode-file (filename huffman-tree)
  (with-open-file (stream filename
			  :direction :input
			  :if-does-not-exist :error)
    (let ((message (file-to-char-list stream)))
      (format t "Message from file: ~a~%" message)
      (hucodec-encode message huffman-tree))))
