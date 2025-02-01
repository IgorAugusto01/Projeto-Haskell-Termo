import System.Info (os)
import System.Process (system)
import Data.Char
import Data.Time.Clock (getCurrentTime, utctDayTime)

data Resultado = Corretas | Errado | Parcial deriving (Show, Eq)

tamanhoPalavra :: [a] -> Int
tamanhoPalavra [] = 0
tamanhoPalavra (x:xs) = 1 + tamanhoPalavra xs

--data Tentativa = Tentativa Palavra [Resultado] deriving (Show)--Palavra = palavra digitada pelo usuário

type Jogo = (Palavra,[Palavra]) --Palavra = palavra correta

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

--x = correta
verificarResultado :: Palavra -> Palavra -> Palavra -> Palavra
verificarResultado _ [] _ = []
verificarResultado _ _ [] = []
verificarResultado ts (x:xs) (y:ys) = aplicarCorLetra (compara x y, y) ++ verificarResultado ts xs ys
  where
    compara x y
        | x == y = Corretas
        | taNaLista ts y = Parcial
        | otherwise = Errado

taNaLista :: Eq a => [a] -> a -> Bool
taNaLista [] _ = False
taNaLista (x:xs) y 
    | x == y = True
    | otherwise = taNaLista xs y

type Palavra = String

verificarPalavra :: Palavra -> Palavra -> [Resultado]
verificarPalavra correta tentativa = zipWith verificar correta tentativa --aplica a função verificar em correta e tentativa, gerando outra lista
    where
        verificar letraCorreta letraTentativa
            | letraCorreta == letraTentativa = Corretas
            | taNaLista correta letraTentativa = Parcial
            | otherwise = Errado

aplicarCor :: Resultado -> Char -> String
aplicarCor Corretas letra = "\x1b[32m" ++ [letra] ++ " " ++ "\x1b[0m"  -- verde
aplicarCor Errado letra = [letra] ++ " " 
aplicarCor Parcial letra = "\x1b[33m" ++ [letra] ++ " " ++ "\x1b[0m" -- amarelo

aplicarCorLetra :: (Resultado, Char) -> String
aplicarCorLetra (resultado,letra) = aplicarCor resultado letra

imprimirLista :: [Palavra] -> IO()
imprimirLista [] = return ()       
imprimirLista (x:xs) = do
    putStrLn x                     
    imprimirLista xs 
   
criaLista :: Char -> [Char] ->[Char] ->  [[Char]]
criaLista _ _ [] = []
criaLista c str (x:xs) 
 | x == c = if(str == "") then criaLista c "" xs else str : criaLista c "" xs
 | x == ' ' = criaLista c str xs
 | otherwise = criaLista c (str ++ [x]) xs
   
transformaMinusculo :: [Char] -> [Char]
transformaMinusculo xs =  map toLower xs

recolherPalavras :: String -> IO String
recolherPalavras caminhoArquivo = readFile caminhoArquivo

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

limparTela :: IO ()
limparTela = do
    let comando = if os == "mingw32" then "cls" else "clear"
    _ <- system comando
    return ()

jogar :: Palavra -> [Palavra] -> IO ()
jogar palavraCerta listaTentativas = loop 6 listaTentativas
  where
    loop 0 tentativas = do
	 limparTela
	 imprimirLista tentativas
   	 putStrLn ("Perdeu!!! A palavra correta é: " ++ palavraCerta)
    loop n tentativas = do
        limparTela
        imprimirLista tentativas
        putStr "Digite sua tentativa: "
        tentativaUsuario <- getLine
        if tamanhoPalavra tentativaUsuario /= tamanhoPalavra palavraCerta
            then do
                putStrLn ("A palavra tem " ++ show (tamanhoPalavra palavraCerta))
                loop n tentativas
            else do
                let novasTentativas = atualizarTentativas tentativas palavraCerta (transformaMinusculo tentativaUsuario)
                if (transformaMinusculo tentativaUsuario) == palavraCerta
                    then do
					 limparTela
					 imprimirLista novasTentativas
					 putStrLn "Acertou!"
                    else loop (n-1) novasTentativas

main :: IO ()
main = do
 palavras <- recolherPalavras "BancoDePalavras.txt"
 let lista = criaLista ',' "" palavras
 let numPalavras = contaPalavras lista
 do
  indiceAleatorio <- gerarAleatorio 0 (numPalavras - 1)
  let palavraEscolhida = escolherPalavraPeloIndice 0 indiceAleatorio lista
  jogar palavraEscolhida (criarTentativasVazias 6)