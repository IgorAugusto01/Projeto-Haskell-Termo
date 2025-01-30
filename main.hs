import Funcoes


tamanhoPalavra :: [a] -> Int
tamanhoPalavra [] = 0
tamanhoPalavra (x:xs) = 1 + tamanhoPalavra xs

taNaLista :: Eq a => [a] -> a -> Bool
taNaLista [] _ = False
taNaLista (x:xs) y 
    | x == y = True
    | otherwise = taNaLista xs y

type Palavra = String

verificarPalavra :: Palavra -> Palavra -> [Int]
verificarPalavra correta tentativa = zipWith verificar correta tentativa
    where
        verificar letraCorreta letraTentativa
            | letraCorreta == letraTentativa = 2
            | taNaLista correta letraTentativa = 1 
            | otherwise = 0

jogar :: Palavra -> IO()
jogar palavraCerta = loop 6
    where
        loop 0  = putStrLn ("Perdeu!!! A palavra correta é: " ++ palavraCerta)
        loop n = do
            putStrLn ("Digite sua tentativa:")
            tentativaUsuario <- getLine
            if(tamanhoPalavra tentativaUsuario /= tamanhoPalavra palavraCerta)
                then do
                    putStrLn ("A palavra tem " ++ show (tamanhoPalavra palavraCerta))
                    loop n
                else do
                    --verificação com cor
                    let contLetraCerta = verificarPalavra palavraCerta tentativaUsuario
                    putStrLn ("Voce acertou: " ++ show contLetraCerta)
                    if(tentativaUsuario == palavraCerta)
                        then putStrLn "Acertou"
                        else
                            loop (n-1)        

-- Função principal para rodar o jogo
main :: IO ()
main = do
    let guess = "TEARO"
    let secret = "TERMO"
    putStrLn "Tentativa:"
    putStrLn (verificarResultado guess secret)

main :: IO ()
main = do
 palavras <- recolherPalavras "BancoDePalavras.txt"
 let lista = criaLista ',' "" palavras
 --print lista
 jogar "Termo"
 
 
