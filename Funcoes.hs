
module Funcoes  where





recolherPalavras :: String -> IO String
recolherPalavras caminhoArquivo = readFile caminhoArquivo 



criaLista :: Char -> [Char] ->[Char] ->  [[Char]]
criaLista _ _ [] = []
criaLista c str (x:xs) 
 | x == c = str : criaLista c "" xs 
 | otherwise = criaLista c (str ++ [x]) xs

