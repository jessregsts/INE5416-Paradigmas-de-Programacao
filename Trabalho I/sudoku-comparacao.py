import math  # Importa o módulo math para usar funções matemáticas

def eh_valido(tabuleiro, linha, coluna, numero, condicoes):
    
    """
    Verifica se um número pode ser colocado em uma posição específica no tabuleiro levando em consideração as regras do Sudoku
    """
    
    n = len(tabuleiro)  # Tamanho do tabuleiro (n x n)
    
    tamanho_bloco = int(math.sqrt(n))  # Calcula o tamanho dos blocos menores

    # Verifica se o número já está na linha
    if numero in tabuleiro[linha]:
        return False

    # Verifica se o número já está na coluna
    if numero in [tabuleiro[i][coluna] for i in range(n)]:
        return False

    # Verifica se o número já está no bloco
    inicio_linha, inicio_coluna = (linha // tamanho_bloco) * tamanho_bloco, (coluna // tamanho_bloco) * tamanho_bloco
    
    # Define o início do bloco ao qual a célula pertence
    for i in range(inicio_linha, inicio_linha + tamanho_bloco):  # Percorre as linhas do bloco
        for j in range(inicio_coluna, inicio_coluna + tamanho_bloco):  # Percorre as colunas do bloco
            if tabuleiro[i][j] == numero:  # Se o número já estiver no bloco não é válido
                return False

    # Verifica as condições de desigualdade
    for (x1, y1), (x2, y2) in condicoes:
        # Se a célula atual é (x1, y1) verifica se cumpre a condição de ser menor que (x2, y2)
        if (linha, coluna) == (x1, y1) and tabuleiro[x2][y2] != 0 and numero >= tabuleiro[x2][y2]:
            return False
        # Se a célula atual é (x2, y2) verifica se cumpre a condição de ser maior que (x1, y1)
        if (linha, coluna) == (x2, y2) and tabuleiro[x1][y1] != 0 and numero <= tabuleiro[x1][y1]:
            return False

    return True  #Retorna verdadeiro se todas as verificações forem satisfeitas

def resolve_sudoku(tabuleiro, condicoes):
    
    """
    Resolve o Sudoku utilizando backtracking respeitando as regras padrão e as condições de desigualdade.
    """
    
    n = len(tabuleiro)  # Tamanho do tabuleiro (n x n)

    # Encontra a próxima célula vazia representada pelo valor 0
    for linha in range(n):  # Percorre todas as linhas do tabuleiro
        for coluna in range(n):  # Percorre todas as colunas do tabuleiro
            if tabuleiro[linha][coluna] == 0:  # Verifica se a célula está vazia
                # Testa números de 1 até n
                for numero in range(1, n + 1):
                    # Verifica se o número é válido na posição atual
                    if eh_valido(tabuleiro, linha, coluna, numero, condicoes):
                        tabuleiro[linha][coluna] = numero  # Coloca o número na célula
                        # Continua tentando resolver o restante do tabuleiro recursivamente
                        if resolve_sudoku(tabuleiro, condicoes):
                            return True
                        tabuleiro[linha][coluna] = 0  # Se falhar, desfaz a jogada (backtracking)
                return False  # Se nenhum número for válido, retorna falso (backtracking)
    return True  # Retorna verdade se o tabuleiro for resolvido por completo

def imprime_tabuleiro(tabuleiro):
    
    """
    Imprime o tabuleiro do Sudoku substituindo células vazias (0) por um ponto
    """
    
    for linha in tabuleiro:  # Para cada linha no tabuleiro.
        print(" ".join(str(numero) if numero != 0 else "." for numero in linha))  # Imprime números ou ponto

# Exemplo de uso

n = 4  # Tamanho do tabuleiro é 4x4
tabuleiro = [
    [0, 0, 0, 0],  # Tabuleiro inicial com todas as células vazias
    [0, 0, 0, 0],
    [0, 0, 0, 0],
    [0, 0, 0, 0]
]

# Condições adicionais: (x1, y1) < (x2, y2)
condicoes = [
    ((0, 0), (0, 1)),  # A célula (0, 0) tem que ser menor que a célula (0, 1)
    ((0, 3), (0, 2)),  # A célula (0, 3) tem que ser menor que a célula (0, 2)
    # Assim por diante
    ((0, 0), (1, 0)),
    ((1, 1), (1, 0)),
    ((0, 1), (1, 1)),
    ((1, 2), (0, 2)),
    ((1, 2), (1, 3)),
    ((1, 3), (0, 3)),
    ((2, 0), (2, 1)),
    ((2, 3), (2, 2)),
    ((3, 0), (2, 0)),
    ((3, 1), (2, 1)),
    ((3, 1), (3, 0)),
    ((2, 2), (3, 2)),
    ((2, 3), (3, 3)),
    ((3, 2), (3, 3))
]

print("Tabuleiro inicial:")  # Mostra o tabuleiro inicial

imprime_tabuleiro(tabuleiro)

if resolve_sudoku(tabuleiro, condicoes):  # Tenta resolver o Sudoku com as condições dadas
    print("\nSolução encontrada:")  # Se uma solução for encontrada, a mostra
    imprime_tabuleiro(tabuleiro)
else:
    print("\nNenhuma solução encontrada.")  # Se não houver solução, informa ao jogador
