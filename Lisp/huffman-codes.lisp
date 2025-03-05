; -*- Mode: Lisp -*-
;;; Carbutti Lucia, Cividini Diego

; huffman-codes.lisp starts here

(defstruct huffman-node
  weight
  symbol
  left
  right)

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
                 (let* ((sorted-queue
			 (sort queue #'< :key #'huffman-node-weight))
                        (left (pop sorted-queue))
                        (right (pop sorted-queue))
                        (new-node (make-huffman-node
                                   :weight (+ (huffman-node-weight left)
					      (huffman-node-weight right))
                                   :left left
                                   :right right)))
                   (merge-nodes (cons new-node sorted-queue))))))
    (first (merge-nodes (mapcar (lambda (sw)
                                  (make-huffman-node
                                   :weight (cdr sw)
                                   :symbol (if (symbolp (car sw))
                                               (char (symbol-name (car sw)) 0)
                                               (car sw))))
                                symbols-n-weights)))))

(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((traverse (node prefix)
             (if (huffman-node-symbol node)
                 (list (cons (huffman-node-symbol node) prefix))
                 (append (traverse (huffman-node-left node)
				   (append prefix '(0)))
                         (traverse (huffman-node-right node)
				   (append prefix '(1)))))))
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
                     (huffman-node-left node)
                     (huffman-node-right node)))
      (when (huffman-node-symbol node)
        (push (huffman-node-symbol node) message)
        (setf node huffman-tree)))
    (nreverse message)))

(defun collect-symbols (node)
  (if (huffman-node-symbol node)
      (list (huffman-node-symbol node))
      (append (collect-symbols (huffman-node-left node))
              (collect-symbols (huffman-node-right node)))))

(defun print-node (node indent prefix)
  (if (huffman-node-symbol node)
      (format t "~&~v@T~a Node: ~a, Weight: ~a"
              indent prefix (huffman-node-symbol node)
	      (huffman-node-weight node))
      (progn
        (format t "~&~v@T~a Node: ~a, Weight: ~a"
                indent prefix (collect-symbols node) (huffman-node-weight node))
        (print-node (huffman-node-left node) (+ indent 2) "L: ")
        (print-node (huffman-node-right node) (+ indent 2) "R: "))))

(defun hucodec-print-huffman-tree (huffman-tree &optional (indent 0))
  (print-node huffman-tree indent "Root: "))


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
      (hucodec-encode message huffman-tree))))

; huffman-codes.lisp ends here