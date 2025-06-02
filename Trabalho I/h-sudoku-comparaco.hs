import Data.List (nub, (\\))
import Control.Applicative ((<|>))

type Tabuleiro = [[Int]] -- Define o tipo Tabuleiro como uma lista de listas de inteiros
type Condicao = ((Int, Int), (Int, Int)) -- Define o tipo Condicao como um par de pares representando uma restrição entre duas células

-- Verifica se o número pode ser colocado na posição (linha, coluna)
ehValido :: Tabuleiro -> Int -> Int -> Int -> [Condicao] -> Bool
ehValido tabuleiro linha coluna numero condicoes =
  notElem numero (tabuleiro !! linha) &&                        -- Verifica se o número já está na linha
  notElem numero [tabuleiro !! r !! coluna | r <- [0..n-1]] &&   -- Verifica se o número já está na coluna
  notElem numero (elementosDoBloco linha coluna) &&              -- Verifica se o número já está no bloco correspondente
  all condicaoValida condicoes                                   -- Verifica se todas as condições são satisfeitas
  where
    n = length tabuleiro -- Tamanho do tabuleiro
    tamanhoBloco = round . sqrt . fromIntegral $ n -- Calcula o tamanho de um bloco 
    inicioLinha = (linha `div` tamanhoBloco) * tamanhoBloco -- Linha inicial do bloco
    inicioColuna = (coluna `div` tamanhoBloco) * tamanhoBloco -- Coluna inicial do bloco
    elementosDoBloco l c = [tabuleiro !! i !! j | i <- [inicioLinha..inicioLinha+tamanhoBloco-1], 
                                                  j <- [inicioColuna..inicioColuna+tamanhoBloco-1]] -- Retorna os números do bloco
    condicaoValida ((x1, y1), (x2, y2)) -- Verifica se a condição entre duas células é válida
      | (linha, coluna) == (x1, y1) = tabuleiro !! x2 !! y2 == 0 || numero < tabuleiro !! x2 !! y2 -- Se esta célula é x1, verifica se o número é menor que a célula x2
      | (linha, coluna) == (x2, y2) = tabuleiro !! x1 !! y1 == 0 || numero > tabuleiro !! x1 !! y1 -- Se esta célula é x2, verifica se o número é maior que a célula x1
      | otherwise = True -- Caso contrário, a condição não é relevante para esta célula

-- Resolvo o Sudoku usando o backtracking
resolveSudoku :: Tabuleiro -> [Condicao] -> Maybe Tabuleiro
resolveSudoku tabuleiro condicoes
  | null celulasVazias = Just tabuleiro  -- Se não há células vazias, o tabuleiro está resolvido
  | otherwise = tentaValores tabuleiro (head celulasVazias) -- Tenta preencher a próxima célula vazia
  where
    n = length tabuleiro -- Tamanho do tabuleiro
    celulasVazias = [(r, c) | r <- [0..n-1], c <- [0..n-1], tabuleiro !! r !! c == 0] -- Lista de coordenadas de células vazias
    tentaValores t (r, c) = foldr (\num acc -> acc <|> tenta num) Nothing [1..n] -- Tenta cada número possível na célula
      where
        tenta numero
          | ehValido t r c numero condicoes = -- Se o número é válido para a célula
              resolveSudoku (atualizaTabuleiro t r c numero) condicoes -- Avança para a próxima célula
          | otherwise = Nothing -- Caso contrário, tenta outro número
    atualizaTabuleiro t r c numero = take r t ++
                                     [take c (t !! r) ++ [numero] ++ drop (c + 1) (t !! r)] ++
                                     drop (r + 1) t -- Atualiza o tabuleiro com o novo número

-- Mostra o tabuleiro
imprimeTabuleiro :: Tabuleiro -> IO ()
imprimeTabuleiro = mapM_ (putStrLn . unwords . map mostraCelula) -- Converte cada linha do tabuleiro em uma string formatada e exibe
  where
    mostraCelula 0 = "." -- Representa células vazias como ponto
    mostraCelula n = show n -- Exibe o número da célula

-- Tabuleiro inicial e condições
main :: IO ()
main = do
  let n = 4 -- Tamanho do tabuleiro
      tabuleiro = replicate n (replicate n 0) -- Cria um tabuleiro vazio só de zeros
      condicoes = [((0, 0), (0, 1)), -- Ex. cond.: a célula (0, 0) deve ser menor que (0, 1)
                   ((0, 3), (0, 2)),
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
                   ((3, 2), (3, 3))] -- Outras condições, seguindo a mesma lógica
  putStrLn "Tabuleiro inicial:" -- Mostra a mensagem inicial
  imprimeTabuleiro tabuleiro -- Mostra o tabuleiro vazio
  case resolveSudoku tabuleiro condicoes of
    Just solucao -> do -- Se uma solução for encontrada, mostra a mensagem de solução
      putStrLn "\nSolução encontrada:" 
      imprimeTabuleiro solucao -- Então mostra o tabuleiro resolvido
    Nothing -> putStrLn "\nNenhuma solução encontrada." -- Caso contrário, mostra que não há solução

