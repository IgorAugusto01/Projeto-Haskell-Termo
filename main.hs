import Funcoes

main :: IO ()
main = do
  
  -- recolhe as palvras do arquivo colocando
 palavras <- recolherPalavras "BancoDePalavras.txt"

 -- monta uma lista onde cada a partir da vírgula
 let lista = criaLista ',' "" palavras
 let total = contaPalavras lista
 
 indiceAleatorio <- gerarAleatorio 0 (total - 1) 
 let palavraSorteada = escolherPalavraPeloIndice 0 indiceAleatorio  lista
 print palavraSorteada

 
