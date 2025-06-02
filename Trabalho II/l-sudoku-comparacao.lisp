(defun permutacao-valida (permutacao restricoes)
  
  
  ;; Função que verifica se a ordem dos números em uma linha (uma permutação) atende as restrições
  ;; Para o jogo funcionar corretamente, cada restrição define uma relação entre dois índices da permutação 
  ;; Por exemplo, posição 0 deve ser maior que posição 4
  ;; Etc
  
  (every (lambda (restricao)
           (let* ((i (first restricao)) ;; Índice do primeiro elemento na restrição
                  (j (second restricao)) ;; Índice do segundo elemento na restrição
                  (op (third restricao)) ;; Operador que indica a relação (#\> ou #\<)
                  (val-i (nth i permutacao)) ;; Valor da posição i na permutação
                  (val-j (nth j permutacao))) ;; Valor da posição j na permutação
             ;; Avalia a relação entre val-i e val-j de acordo com o operador
             (cond
               ((char= op #\>) (> val-i val-j)) ;; Verifica se val-i é maior que val-j
               ((char= op #\<) (< val-i val-j)) ;; Verifica se val-i é menor que val-j
               (t t)))) ;; Se não houver operador válido, assume verdadeiro
         restricoes)) ;; Verifica todas as restrições fornecidas


(defun gerar-permutacoes (lista)
  
  
  ;; Função que gera  gera todas as permutações possíveis de uma lista.
  ;; Exemplo, para (1 2 3) as permutações seriam [(1 2 3) (1 3 2) (2 1 3) ...]
  
  (if (null lista) ;; Caso base - a lista vazia retorna uma lista contendo outra lista vazia
      '(())
      ;; Caso recursivo - para cada elemento da lista combina-o com as permutações do restante
      (mapcan (lambda (elemento) 
                ;; Para cada permutação do restante da lista, adiciona o elemento atual no início
                (mapcar (lambda (permutacao)
                          (cons elemento permutacao))
                        ;; Remove o elemento atual e gera as permutações do restante
                        (gerar-permutacoes (remove elemento lista :count 1))))
              lista)))


(defun dividir-em-blocos (n lista)
  
  
  ;; Função que divide uma lista em blocos de tamanho n
  ;; Para n = 3 e lista = (1 2 3 4 5 6), deve retornar [(1 2 3) (4 5 6)]

  (if (null lista) ;; Caso base - a lista vazia retorna uma lista vazia
      '()
      ;; Pega os primeiros n elementos como o primeiro bloco depois aplica recursivamente no restante
      (cons (subseq lista 0 n)
            (dividir-em-blocos n (subseq lista n)))))


(defun verificar-grade (grade restricoes)
  
  
  ;; Verifica se uma grade (a matriz) atende as restrições
  ;; As restrições são aplicadas entre os índices absolutos da grade

  (let ((n (length grade))) ;; n é o número de linhas (ou colunas) da grade
    (every (lambda (restricao)
             (let* ((i (first restricao)) ;; Índice linear do primeiro elemento
                    (j (second restricao)) ;; Índice linear do segundo elemento
                    (op (third restricao)) ;; Operador da relação (#\> ou #\<)
                    ;; Converte os índices lineares para (linha, coluna) e obtém os valores correspondentes
                    (val-i (nth (mod i n) (nth (floor i n) grade)))
                    (val-j (nth (mod j n) (nth (floor j n) grade))))
               ;; Avalia a relação entre val-i e val-j
               (cond
                 ((char= op #\>) (> val-i val-j))
                 ((char= op #\<) (< val-i val-j))
                 (t t)))) ;; Se a restrição for inválida assume valor verdadeiro
           restricoes)))


(defun resolver-sudoku (n valores-iniciais restricoes)
  
  
  ;; Função principal
  ;; Resolve o Sudoku de tamanho n, com seus valores iniciais e restrições
  
  (let* ((numeros (loop for i from 1 to n collect i)) ;; Lista de números possíveis para o Sudoku (Ex. 1 2 3 4...)
         (linhas (dividir-em-blocos n valores-iniciais)) ;; Converte os valores iniciais em uma matriz de n x n
         ;; Gera permutações para cada linha da grade 
         ;; Linhas com valores fixos permanecem como estão
         (permutacoes-todas-linhas (mapcar (lambda (linha)
                                             (if (some (lambda (x) (/= x 0)) linha) ;; Se a linha contém valores fixos
                                                 (list linha) ;; apenas usa a linha como está
                                                 (gerar-permutacoes numeros))) ;; Caso contrário gera permutações
                                           linhas)))
    ;; Função recursiva para tentar resolver o Sudoku
    (labels ((tentar-resolver (permutacoes-restantes solucao)
               (if (null permutacoes-restantes) ;; Caso base: todas as permutações foram processadas
                   ;; Verifica se a solução completa atende às restrições
                   (if (verificar-grade (dividir-em-blocos n solucao) restricoes)
                       (list solucao) ;; Se válido, retorna a solução encontrada
                       nil) ;; Caso contrário, não há solução para este caminho
                   ;; Para cada permutação da linha atual tenta resolver o restante recursivamente
                   (mapcan (lambda (permutacao)
                             (tentar-resolver (rest permutacoes-restantes)
                                              (append solucao permutacao))) ;; Adiciona a permutação à solução atual
                           (first permutacoes-restantes))))) ;; Processa a primeira linha disponível
      ;; Chama a função recursiva com todas as permutações possíveis e uma solução inicial vazia
      (let ((resultado (tentar-resolver permutacoes-todas-linhas '())))
        ;; Retorna o resultado como uma matriz n x n (ou nula se não houver solução)
        (if resultado
            (dividir-em-blocos n (first resultado))
            nil)))))


(defun imprimir-sudoku (grade)
  
  ;; Função para mostrar a solução do Sudoku na tela
  ;;Imprime a grade do Sudoku linha por linha
  
  (dolist (linha grade)
    (format t "~a~%" linha)))


(defun principal ()
  
  ;; Função principal que começa e resolve o sudoku com as restrições
  

  (let* ((n 4) ;; Tamanho do Sudoku 4x4
         (valores-iniciais (make-list (* n n) :initial-element 0)) ;; Inicializa a grade com zeros (vazia)
         (restricoes '((0 4 #\>) ;; Aqui estão as restrições entre os índices
                       (0 1 #\<)
                       (1 5 #\<)
                       (2 6 #\<)
                       (3 2 #\>)
                       (3 7 #\>)
                       (4 5 #\<)
                       (6 7 #\>)
                       (8 9 #\>)
                       (8 12 #\>)
                       (9 13 #\<)
                       (10 14 #\<)
                       (10 11 #\<)
                       (11 15 #\>)
                       (12 13 #\>)
                       (14 15 #\>))))
    
    ;; Soluciona o sudoku com os valores e restrições fornecidos.
    (let ((solucao (resolver-sudoku n valores-iniciais restricoes)))
      
      ;; Mostra a solução ou informa ao jogador que não há solução possível
      (if solucao
          (imprimir-sudoku solucao)
          (format t "Não há solução possível")))))

;; Executa o programa
(principal)
