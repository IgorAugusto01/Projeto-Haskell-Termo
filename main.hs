import Funcoes
import System.IO

 
main :: IO ()
main = do
 hSetEncoding stdout utf8
  -- recolhe as palvras do arquivo colocando
 palavras <- recolherPalavras "BancoDePalavras.txt"

 -- monta uma lista onde cada a partir da vírgula
 let lista = criaLista ',' "" palavras
 let total = contaPalavras lista
 
 indiceAleatorio <- gerarAleatorio 0 (total - 1) 
 let palavraSorteada = escolherPalavraPeloIndice 0 indiceAleatorio  lista

 let p = transformaMinusculo "Ola Mundo"
 putStrLn (p)
