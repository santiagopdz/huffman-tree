;;;; Riccardo Piatti 909687
;;;; -*- Mode: Lisp -*-
;;;; huffman-codes.lisp

;;;; Definizione delle strutture dati per Huffman Tree
(defun make-leaf (symbol weight)
  (list 'leaf symbol weight))

(defun leaf-p (node)
  (eq (car node) 'leaf))

(defun leaf-symbol (leaf)
  (cadr leaf))

(defun weight (node)
  (if (leaf-p node)
      (caddr node)
      (cadddr node)))

(defun symbols (node)
  (if (leaf-p node)
      (list (leaf-symbol node))
      (caddr node)))

(defun make-code-tree (left right)
  (list left right (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(defun node-left (node)
  (car node))

(defun node-right (node)
  (cadr node))

;;;; Decodifica una sequenza di bit usando un albero di Huffman
(defun hucodec-decode (bits huffman-tree)
  (labels ((decode-1 (bits current-branch decoded-symbols)
             (cond
               ((null bits)
                (if (leaf-p current-branch)
                    (reverse (cons (leaf-symbol current-branch) decoded-symbols))
                    (error "Invalid bit sequence: decoding incomplete.")))
               ((leaf-p current-branch)
                (decode-1 bits huffman-tree (cons (leaf-symbol current-branch) decoded-symbols)))
               (t
                (let ((next-branch (choose-branch (first bits) current-branch)))
                  (decode-1 (rest bits) next-branch decoded-symbols))))))
    (decode-1 bits huffman-tree '())))

;;;; Sceglie il ramo successivo nell'albero Huffman basato su un bit
(defun choose-branch (bit branch)
  (cond ((= 0 bit) (node-left branch))
        ((= 1 bit) (node-right branch))
        (t (error "Bad bit ~D." bit))))

;;;; Genera una tabella simbolo-bit da un albero di Huffman
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (labels ((traverse (node path)
             (if (leaf-p node)
                 (list (cons (leaf-symbol node) (reverse path)))
                 (append (traverse (node-left node) (cons 0 path))
                         (traverse (node-right node) (cons 1 path))))))
    (traverse huffman-tree '())))

;;;; Funzione per leggere il contenuto di un file mantenendo tutti i simboli
(defun read-file-symbols (stream)
  (let ((char (read-char stream nil nil)))  ;; Legge carattere per carattere
    (if char
        (cons char (read-file-symbols stream))
        '())))  ;; Ritorna la lista di caratteri letti

;;;; Codifica un file generando automaticamente l'albero di Huffman
(defun hucodec-encode-file (filename huffman-tree)
  (with-open-file (stream filename :direction :input)
    (hucodec-encode (read-file-symbols stream) huffman-tree)))

;;;; Codifica un messaggio usando un albero di Huffman
(defun hucodec-encode (message huffman-tree)
  (let ((table (hucodec-generate-symbol-bits-table huffman-tree)))
    (let ((missing (remove-if (lambda (sym) (assoc sym table :test #'equal)) message)))
      (if missing
          (error "I seguenti simboli non sono presenti nell'albero di Huffman: ~A" missing)
          (apply #'append (mapcar (lambda (sym) (cdr (assoc sym table :test #'equal))) message))))))

;;;; Genera un albero di Huffman da una lista di coppie simbolo-peso
(defun hucodec-generate-huffman-tree (symbols-n-weights)
  (if (null symbols-n-weights)
      (error "Cannot generate Huffman tree from empty symbol-weight list."))
  (successive-merge (make-leaf-set symbols-n-weights)))

;;;; Unisce ricorsivamente i nodi per costruire un albero di Huffman
(defun successive-merge (nodes)
  (if (= (length nodes) 1)
      (car nodes)
      (let* ((first (car nodes))
             (second (cadr nodes))
             (rest (cddr nodes))
             (merged (make-code-tree first second)))
        (successive-merge (adjoin-set merged rest)))))

;;;; Costruisce un insieme ordinato di foglie a partire da coppie simbolo-peso
(defun make-leaf-set (pairs)
  (let ((sorted (stable-sort (copy-list pairs) #'< :key #'cdr)))
    (mapcar (lambda (pair) (make-leaf (car pair) (cdr pair))) sorted)))

;;;; Inserisce un nodo in un insieme ordinato in base al peso
(defun adjoin-set (x set)
  (cond ((null set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (t (cons (car set) (adjoin-set x (cdr set))))))

;;;; Stampa la struttura dell'albero di Huffman per debugging
(defun hucodec-print-huffman-tree (huffman-tree &optional (indent-level 0))
  (if (leaf-p huffman-tree)
      (format t "~V@T~A (~D)~%" indent-level (leaf-symbol huffman-tree) (weight huffman-tree))
      (progn
        (format t "~V@T[NODE] (~D)~%" indent-level (weight huffman-tree))
        (hucodec-print-huffman-tree (node-left huffman-tree) (+ indent-level 2))
        (hucodec-print-huffman-tree (node-right huffman-tree) (+ indent-level 2)))))