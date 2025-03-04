;;;; huffman-tests.lisp - Test suite per huffman-codes.lisp

(load "Progetto Lisp V4.lisp")


(defun run-tests ()
  (format t "~%===== TEST HUFFMAN =====~%")

  ;;1)Test: Generazione dell'albero di Huffman
  (format t "~%--- Generazione dell'albero di Huffman ---~%")
  (defparameter symbols '(((a a a) . 8) ((b b b) . 3) ((c . c) . 1) ((d d d) . 1) (e . 1) (f . 1) (g . 1) (h . 1)))
  (defparameter ht (hucodec-generate-huffman-tree symbols))
  (format t "Albero generato con successo.~%")
  (hucodec-print-huffman-tree ht)

  ;;2) Test: Generazione della tabella simboli-bits
  (format t "~%--- Generazione della tabella simboli-bits ---~%")
  (defparameter symbol-bits-table (hucodec-generate-symbol-bits-table ht))
  (format t "Tabella simboli-bits: ~A~%" symbol-bits-table)

  ;;3) Test: Encoding e Decoding di un messaggio
  (format t "~%--- Encoding e Decoding ---~%")
  (defparameter message '((a a a) (b b b) (a a a) (c . c) (d d d) (a a a) e (a a a) f (a a a) (b b b) (b b b) (a a a) (a a a) g (a a a) h))
  (format t "Messaggio originale: ~A~%" message)
  
  (defparameter encoded (hucodec-encode message ht))
  (format t "Messaggio codificato: ~A~%" encoded)

  (defparameter decoded (hucodec-decode encoded ht))
  (format t "Messaggio decodificato: ~A~%" decoded)
  
  (format t "Verifica di correttezza: ~A~%" (equal message decoded))

  ;;4) Test: Verifica del comportamento specificato dal professore
  (format t "~%--- Test di conformità alla specifica ---~%")
  (format t "Il messaggio originale e il messaggio decodificato sono uguali? ~A~%"
          (equal message (hucodec-decode (hucodec-encode message ht) ht)))

  ;;5) Test: Codifica da file
  (format t "~%--- Test di codifica da file ---~%")
  (with-open-file (out "test-message.txt" :direction :output :if-exists :supersede)
    (format out "~A" message))
  
  (let ((file-encoded (hucodec-encode-file "test-message.txt" ht)))
    (format t "Messaggio da file codificato: ~A~%" file-encoded))

  ;;6)Test: Errori su simboli non presenti
  (format t "~%--- Test errori su simboli non presenti ---~%")
  (handler-case
      (progn
        (hucodec-encode '(z) ht)
        (format t "Errore mancato su simbolo non presente!~%"))
    (error (e) (format t "Errore rilevato correttamente: ~A~%" e)))

  ;;7)Test: Albero vuoto
  (format t "~%--- Test errore su albero vuoto ---~%")
  (handler-case
      (progn
        (hucodec-generate-huffman-tree '())
        (format t "Errore mancato su albero vuoto!~%"))
    (error (e) (format t "Errore rilevato correttamente: ~A~%" e)))

  (format t "~%===== FINE TEST =====~%"))

(run-tests)