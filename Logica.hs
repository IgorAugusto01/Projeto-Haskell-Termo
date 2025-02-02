module Logica where

import Tipos
import Grafico
import Data.Char
import Data.Time.Clock (getCurrentTime, utctDayTime)

recolherPalavras :: String -> IO String
recolherPalavras caminhoArquivo = readFile caminhoArquivo

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (x:xs) = 1 + tamanho xs

transformaMinusculo :: [Char] -> [Char]
transformaMinusculo xs =  map toLower xs

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

criaLista :: Char -> [Char] ->[Char] ->  [[Char]]
criaLista _ xs [] = [xs]
criaLista c str (x:xs) 
 | x == c = str : criaLista c "" xs
 | otherwise = criaLista c (str ++ [x]) xs
 
criarTentativasVazias :: Int -> [Palavra] 
criarTentativasVazias 0 =  []
criarTentativasVazias x =  [tentativaVazia] ++ criarTentativasVazias (x-1)
    where
        tentativaVazia =  "_ _ _ _ _" 
        
atualizarTentativas :: [Palavra] -> Palavra -> Palavra -> [Palavra]
atualizarTentativas [] _ _ = []  
atualizarTentativas (x:xs) palavraCerta tentativa
    | x == "_ _ _ _ _" = verificarResultado palavraCerta palavraCerta tentativa : xs
    | otherwise = x : atualizarTentativas xs palavraCerta tentativa


escolherPalavraPeloIndice :: Int -> Int -> [[Char]] -> [Char]
escolherPalavraPeloIndice _ _ [] = error "palavra nao encontrada"
escolherPalavraPeloIndice i n (x : xs)
 | i == n = x
 | otherwise = escolherPalavraPeloIndice (i+1) n xs

gerarAleatorio :: Int -> Int -> IO Int
gerarAleatorio min max = do
    
    tempoAtual <- getCurrentTime
    let segundos = floor (utctDayTime tempoAtual) :: Int  
    return (min + (segundos `mod` (max - min + 1)))  