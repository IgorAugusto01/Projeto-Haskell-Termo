module Logica where

import Tipos
import Grafico
import Data.Char
import Data.Time.Clock (getCurrentTime, utctDayTime)

recolherPalavras :: String -> IO String
recolherPalavras caminhoArquivo = readFile caminhoArquivo

tamanhoPalavra :: [a] -> Int
tamanhoPalavra [] = 0
tamanhoPalavra (x:xs) = 1 + tamanhoPalavra xs

transformaMinusculo :: [Char] -> [Char]
transformaMinusculo xs =  map toLower xs

contaPalavras :: [[Char]] -> Int
contaPalavras []  = 0
contaPalavras (x:xs) = 1 + contaPalavras xs

taNaLista :: Eq a => [a] -> a -> Bool
taNaLista [] _ = False
taNaLista (x:xs) y 
    | x == y = True
    | otherwise = taNaLista xs y

verificarResultado :: Palavra -> Palavra -> Palavra -> Palavra
verificarResultado _ [] _ = []
verificarResultado _ _ [] = []
verificarResultado ts (x:xs) (y:ys) = aplicarCorLetra (compara x y, y) ++ verificarResultado ts xs ys
  where
    compara x y
        | x == y = Corretas
        | taNaLista ts y = Parcial
        | otherwise = Errado
        
verificarPalavra :: Palavra -> Palavra -> [Resultado]
verificarPalavra correta tentativa = zipWith verificar correta tentativa --aplica a função verificar em correta e tentativa, gerando outra lista
    where
        verificar letraCorreta letraTentativa
            | letraCorreta == letraTentativa = Corretas
            | taNaLista correta letraTentativa = Parcial
            | otherwise = Errado

criaLista :: Char -> [Char] ->[Char] ->  [[Char]]
criaLista _ _ [] = []
criaLista c str (x:xs) 
 | x == c = if(str == "") then criaLista c "" xs else str : criaLista c "" xs
 | x == ' ' = criaLista c str xs
 | otherwise = criaLista c (str ++ [x]) xs
 
criarTentativasVazias :: Int -> [Palavra] 
criarTentativasVazias 0 =  []
criarTentativasVazias x =  [tentativaVazia] ++ criarTentativasVazias (x-1)
    where
        tentativaVazia =  "_ _ _ _ _" 
        
atualizarTentativas :: [Palavra] -> Palavra -> Palavra -> [Palavra]
atualizarTentativas [] _ _ = []  
atualizarTentativas (x:xs) palavraCerta tentativa
    | x == "_ _ _ _ _" = verificarResultado palavraCerta palavraCerta tentativa : xs -- Substitui a primeira tentativa vazia
    | otherwise = x : atualizarTentativas xs palavraCerta tentativa


escolherPalavraPeloIndice :: Int -> Int -> [[Char]] -> [Char]
escolherPalavraPeloIndice _ _ [] = error "palavra nao encontrada"
escolherPalavraPeloIndice i n (x : xs)
 | i == n = x
 | otherwise = escolherPalavraPeloIndice (i+1) n xs

gerarAleatorio :: Int -> Int -> IO Int
gerarAleatorio min max = do
    -- Obtém o tempo atual em segundos desde a "época Unix"
    currentTime <- getCurrentTime
    let segundos = floor (utctDayTime currentTime) :: Int  -- Tempo em segundos
    return (min + (segundos `mod` (max - min + 1)))  -- Limita ao intervalo [min, max]