import Jogo
import Logica

jogar :: IO ()
jogar = do
 palavras <- recolherPalavras "BancoDePalavras.txt"
 let lista = criaLista ',' "" palavras
 let numPalavras = tamanho lista
 do
  indiceAleatorio <- gerarAleatorio 0 (numPalavras - 1)
  let palavraEscolhida = escolherPalavraPeloIndice 0 indiceAleatorio lista
  jogo palavraEscolhida (criarTentativasVazias 6)