
module Funcoes  where 



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
