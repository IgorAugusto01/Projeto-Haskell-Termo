
module Funcoes  where



recolherPalavras :: String -> IO String
recolherPalavras caminhoArquivo = readFile caminhoArquivo 



criaLista :: Char -> [Char] ->[Char] ->  [[Char]]
criaLista _ _ [] = []
criaLista c str (x:xs) 
 | x == c = if(str == "") then criaLista c "" xs else str : criaLista c "" xs
 | x == ' ' = criaLista c str xs
 | otherwise = criaLista c (str ++ [x]) xs

        


      