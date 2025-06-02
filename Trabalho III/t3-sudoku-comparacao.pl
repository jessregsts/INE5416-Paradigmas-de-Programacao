:- use_module(library(clpfd)). % Importa a biblioteca CLP(FD) para restrições em domínios finitos.

% Verifica se uma permutação satisfaz as restrições de ordem
permutacao_valida(_, []). % Caso base: não há mais restrições para verificar.
permutacao_valida(Lista, [(I, J, Op)|Restricoes]) :- % Verifica a primeira restrição e continua verificando as demais.
    nth1(I, Lista, ValorI), % Obtém o elemento na posição I da lista.
    nth1(J, Lista, ValorJ), % Obtém o elemento na posição J da lista.
    operacao_valida(ValorI, ValorJ, Op), % Verifica se a operação entre os valores é válida.
    permutacao_valida(Lista, Restricoes). % Verifica o restante das restrições.

% Define como validar uma operação de comparação
operacao_valida(ValorI, ValorJ, '>') :- ValorI #> ValorJ. % Verifica se o primeiro valor é maior que o segundo.
operacao_valida(ValorI, ValorJ, '<') :- ValorI #< ValorJ. % Verifica se o primeiro valor é menor que o segundo.

% Verifica se uma grade (lista de linhas) satisfaz as restrições de ordem
verificar_restricoes(_, []). % Caso base: não há mais restrições para verificar.
verificar_restricoes(Linhas, [(I, J, Op)|Restricoes]) :- % Verifica a primeira restrição e continua verificando as demais.
    nth1(I, Linhas, LinhaI), % Obtém a linha na posição I.
    nth1(J, Linhas, LinhaJ), % Obtém a linha na posição J.
    operacao_valida(LinhaI, LinhaJ, Op), % Verifica se a operação entre as linhas é válida.
    verificar_restricoes(Linhas, Restricoes). % Verifica o restante das restrições.

% Adiciona valores iniciais à solução
adicionar_valores_iniciais([], []). % Caso base: não há mais valores para adicionar.
adicionar_valores_iniciais([0|Vs], [_|Xs]) :- % Ignora valores 0 (células vazias no Sudoku).
    adicionar_valores_iniciais(Vs, Xs).
adicionar_valores_iniciais([V|Vs], [V|Xs]) :- % Copia os valores não nulos para a solução.
    V \= 0,
    adicionar_valores_iniciais(Vs, Xs).

% Resolve o Sudoku com restrições de ordem
resolver_sudoku(N, ValoresIniciais, Restricoes, Solucao) :- % Encontra a solução para um Sudoku de tamanho NxN.
    length(Solucao, N2), % Define o tamanho da solução como N² (número total de células).
    N2 #= N * N, % Confirma que N² é o tamanho total do tabuleiro.
    Solucao ins 1..N, % Restringe os valores das células para estarem entre 1 e N.
    dividir_em_blocos(N, Solucao, Linhas), % Divide a solução em linhas.
    maplist(all_distinct, Linhas), % Garante que todas as linhas tenham valores distintos.
    transpose(Linhas, Colunas), % Transpõe as linhas para obter as colunas.
    maplist(all_distinct, Colunas), % Garante que todas as colunas tenham valores distintos.
    adicionar_valores_iniciais(ValoresIniciais, Solucao), % Aplica os valores iniciais fornecidos.
    verificar_restricoes(Solucao, Restricoes), % Verifica as restrições de ordem.
    label(Solucao). % Encontra uma solução rotulando as variáveis.

% Divide uma lista em blocos de tamanho N
dividir_em_blocos(_, [], []). % Caso base: lista vazia resulta em nenhum bloco.
dividir_em_blocos(N, Lista, [Bloco|Blocos]) :- % Divide a lista em um bloco de tamanho N e continua com o restante.
    length(Bloco, N), % Define o tamanho do bloco.
    append(Bloco, Resto, Lista), % Divide a lista em um bloco e o restante.
    dividir_em_blocos(N, Resto, Blocos). % Continua dividindo o restante.

% Imprime a grade do Sudoku linha por linha
imprimir_sudoku([]). % Caso base: não há mais linhas para imprimir.
imprimir_sudoku([Linha|Linhas]) :- % Imprime a primeira linha e continua com as demais.
    writeln(Linha),
    imprimir_sudoku(Linhas).

% Função principal
principal :- 
    ValoresIniciais = [0, 0, 0, 0, % Valores iniciais do Sudoku (0 indica célula vazia).
                       0, 0, 0, 0,
                       0, 0, 0, 0,
                       0, 0, 0, 0],
    Restricoes = [(1, 5, '>'), (1, 2, '<'), (2, 6, '<'), (3, 7, '<'), % Restrições de ordem entre células.
                  (4, 3, '>'), (4, 8, '>'), (5, 6, '<'), (7, 8, '>'),
                  (9, 10, '>'), (9, 13, '>'), (10, 14, '<'), (11, 15, '<'),
                  (11, 12, '<'), (12, 16, '>'), (13, 14, '>'), (15, 16, '>')],
    N = 4, % Tamanho do Sudoku (4x4).
    (resolver_sudoku(N, ValoresIniciais, Restricoes, Solucao) -> % Tenta resolver o Sudoku.
        dividir_em_blocos(N, Solucao, Linhas), % Divide a solução final em linhas.
        imprimir_sudoku(Linhas) % Imprime a solução.
    ;
        writeln('Não há solução possível para este Sudoku com as restrições dadas.') % Mensagem caso não haja solução.
    ).

:- principal. % Executa a função principal.
