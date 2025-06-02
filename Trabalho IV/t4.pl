% Declaro as cores dos coletes disponíveis
colete(branco).
colete(azul).
colete(verde).
colete(amarelo).
colete(vermelho).

% Declaro os nomes disponíveis
nome(bruce).
nome(tiago).
nome(vicente).
nome(felipe).
nome(ricardo).

% Declaro os segmentos disponíveis
segmento(energiaeletrica).
segmento(papelecelulose).
segmento(bancos).
segmento(educacao).
segmento(medicamentos).

% Declaro a porcentagem de retorno disponíveis
retorno(30).
retorno(40).
retorno(60).
retorno(50).
retorno(20).

% Declaro as corretoras disponíveis
corretora(cerebrol).
corretora(brain).
corretora(cucaynvest).
corretora(cucapactual).
corretora(rc).

% Declaro as idades disponíveis
idade(40).
idade(30).
idade(35).
idade(45).
idade(50).

% Definos as relação:

%X está à ao lado de Y
aoLado(X,Y,Lista) :- nextto(X,Y,Lista); nextto(Y,X,Lista).

%X está à esquerda de Y (em qualquer posição à esquerda)
aEsquerda(X,Y,Lista) :- nth0(IndexX,Lista,X), % Obtenho o índice de X
                        nth0(IndexY,Lista,Y),  % Obtenho o índice de Y
                        IndexX < IndexY. % Verifico se X está antes de Y
                        
%X está à direita de Y (em qualquer posição à direita)
aDireita(X,Y,Lista) :- aEsquerda(Y,X,Lista). % Inverso de a esquerda

%X está no canto se ele é o primeiro ou o último da lista
noCanto(X,Lista) :- last(Lista,X). % Pego último elemento
noCanto(X,[X|_]).  % Pego o primeiro elemento

% Verifico se todos os elementos de uma lista são diferentes
todosDiferentes([]).
todosDiferentes([H|T]) :- not(member(H,T)), todosDiferentes(T).  % Checa se H não está no restante da lista recusivamente

% Solução principal
solucao(ListaSolucao) :- 
    ListaSolucao = [  % Define a estrutura da solução como uma lista de analistas
        analista(Colete1, Nome1, Segmento1, Retorno1, Corretora1, Idade1),
        analista(Colete2, Nome2, Segmento2, Retorno2, Corretora2, Idade2),
        analista(Colete3, Nome3, Segmento3, Retorno3, Corretora3, Idade3),
        analista(Colete4, Nome4, Segmento4, Retorno4, Corretora4, Idade4),
        analista(Colete5, Nome5, Segmento5, Retorno5, Corretora5, Idade5)
    ],

    % Condições baseadas nas pistas fornecidas:

    % O analista de colete Azul está em algum lugar à esquerda do analista que tem conta na corretora CUCA Pactual.
    aEsquerda(analista(azul, _, _, _, _, _), analista(_, _, _, _, cucapactual, _), ListaSolucao),

    % O homem de colete Amarelo está exatamente à direita do analista que acompanha o segmento dos Bancos.
    aDireita(analista(amarelo, _, _, _, _, _), analista(_, _, bancos, _, _, _), ListaSolucao),

    % Em uma das pontas está o analista que investe através da RC Investimentos.
    noCanto(analista(_, _, _, _, rc, _), ListaSolucao),

    % O homem mais velho está em algum lugar à direita do homem de colete Verde.
    aDireita(analista(_, _, _, _, _, 50), analista(verde, _, _, _, _, _), ListaSolucao),

    % Tiago está exatamente à esquerda do analista de 35 anos.
    aEsquerda(analista(_, tiago, _, _, _, _), analista(_, _, _, _, _, 35), ListaSolucao),

    % Em uma das pontas está o analista que usa a Cerebrol Corretora para investir.
    noCanto(analista(_, _, _, _, cerebrol, _), ListaSolucao),

    % Na segunda posição está o homem mais novo.
    Idade2 = 30,

    % O homem de Azul está em algum lugar entre o Bruce e o homem de 35 anos, nessa ordem.
    aDireita(analista(azul, _, _, _, _, _), analista(_, bruce, _, _, _, _), ListaSolucao),
    aEsquerda(analista(azul, _, _, _, _, _), analista(_, _, _, _, _, 35), ListaSolucao),

    % O analista de colete Verde está em algum lugar à esquerda do analista que vislumbra um retorno de 50% no segmento no qual é especialista.
    aEsquerda(analista(verde, _, _, _, _, _), analista(_, _, _, 50, _, _), ListaSolucao),

    % O analista de colete Azul está ao lado do analista que investe através da Cucaynvest.
    aoLado(analista(azul, _, _, _, _, _), analista(_, _, _, _, cucaynvest, _), ListaSolucao),

    % Na segunda posição está o analista que acredita em uma alta de 40% no setor no qual é especialista.
    Retorno2 = 40,

    % Felipe está exatamente à direita do analista de colete Verde.
    aDireita(analista(_, felipe, _, _, _, _), analista(verde, _, _, _, _, _), ListaSolucao),

    % O analista do segmento de Papel e Celulose está em algum lugar entre o analista de Energia Elétrica e o analista de Bancos, nessa ordem.
    aDireita(analista(_, _, papelecelulose, _, _, _), analista(_, _, energiaeletrica, _, _, _), ListaSolucao),
    aEsquerda(analista(_, _, papelecelulose, _, _, _), analista(_, _, bancos, _, _, _), ListaSolucao),

    % O analista de colete Amarelo está em algum lugar à esquerda do analista que acredita em uma alta de 20% no segmento no qual é especialista.
    aEsquerda(analista(amarelo, _, _, _, _, _), analista(_, _, _, 20, _, _), ListaSolucao),

    % Ricardo tem 50 anos.
    member(analista(_, ricardo, _, _, _, 50), ListaSolucao),

    % O analista de colete Verde está ao lado do analista que investe através da CUCA Pactual.
    aoLado(analista(verde, _, _, _, _, _), analista(_, _, _, _, cucapactual, _), ListaSolucao),

    % O analista que acredita no maior retorno está em algum lugar à direita do analista de colete Azul.
    aDireita(analista(_, _, _, 60, _, _), analista(azul, _, _, _, _, _), ListaSolucao),

    % O segmento de Educação é estudado pelo analista que está em algum lugar à direita do homem de colete Verde.
    aDireita(analista(_, _, educacao, _, _, _), analista(verde, _, _, _, _, _), ListaSolucao),

    % O analista que usa a RC Investimentos é especialista no segmento de Medicamentos.
    member(analista(_, _, medicamentos, _, rc, _), ListaSolucao),

    % O homem de 45 anos está ao lado do homem de Verde.
    aoLado(analista(_, _, _, _, _, 45), analista(verde, _, _, _, _, _), ListaSolucao),

    % O analista de colete Branco espera um retorno de 30% no segmento no qual é especialista.
    member(analista(branco, _, _, 30, _, _), ListaSolucao),

    % Testa todas as possibilidades...
    % Geração de combinações e verificação de unicidade:
    colete(Colete1), colete(Colete2), colete(Colete3), colete(Colete4), colete(Colete5),
    todosDiferentes([Colete1, Colete2, Colete3, Colete4, Colete5]),
    
    nome(Nome1), nome(Nome2), nome(Nome3), nome(Nome4), nome(Nome5),
    todosDiferentes([Nome1, Nome2, Nome3, Nome4, Nome5]),
    
    segmento(Segmento1), segmento(Segmento2), segmento(Segmento3), segmento(Segmento4), segmento(Segmento5),
    todosDiferentes([Segmento1, Segmento2, Segmento3, Segmento4, Segmento5]),
    
    retorno(Retorno1), retorno(Retorno2), retorno(Retorno3), retorno(Retorno4), retorno(Retorno5),
    todosDiferentes([Retorno1, Retorno2, Retorno3, Retorno4, Retorno5]),
    
    corretora(Corretora1), corretora(Corretora2), corretora(Corretora3), corretora(Corretora4), corretora(Corretora5),
    todosDiferentes([Corretora1, Corretora2, Corretora3, Corretora4, Corretora5]),
    
    idade(Idade1), idade(Idade2), idade(Idade3), idade(Idade4), idade(Idade5),
    todosDiferentes([Idade1, Idade2, Idade3, Idade4, Idade5]).