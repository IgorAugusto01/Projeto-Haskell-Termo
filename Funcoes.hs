
module Funcoes  where 
import Data.Time.Clock (getCurrentTime, utctDayTime)
import Data.Char



recolherPalavras :: String -> IO String
recolherPalavras caminhoArquivo = readFile caminhoArquivo 



criaLista :: Char -> [Char] ->[Char] ->  [[Char]]
criaLista _ _ [] = []
criaLista c str (x:xs) 
 | x == c = if(str == "") then criaLista c "" xs else str : criaLista c "" xs
 | x == ' ' = criaLista c str xs
 | otherwise = criaLista c (str ++ [x]) xs

        


escolherPalavraPeloIndice :: Int -> Int -> [[Char]] -> [Char]
escolherPalavraPeloIndice _ _ [] = error "palavra nao encontrada"
escolherPalavraPeloIndice i n (x : xs)
 | i == n = x
 | otherwise = escolherPalavraPeloIndice (i+1) n xs


contaPalavras :: [[Char]] -> Int
contaPalavras []  = 0
contaPalavras (x:xs) = 1 + contaPalavras xs



gerarAleatorio :: Int -> Int -> IO Int
gerarAleatorio min max = do
    -- Obtém o tempo atual em segundos desde a "época Unix"
    currentTime <- getCurrentTime
    let segundos = floor (utctDayTime currentTime) :: Int  -- Tempo em segundos
    return (min + (segundos `mod` (max - min + 1)))  -- Limita ao intervalo [min, max]


transformaMinusculo :: [Char] -> [Char]
transformaMinusculo xs =  map toLower xs