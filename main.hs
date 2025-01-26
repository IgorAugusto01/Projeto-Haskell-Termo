import Funcoes

main :: IO ()
main = do
 palavras <- recolherPalavras "BancoDePalavras.txt"
 let lista = criaLista ',' "" palavras
 print lista
 
 
