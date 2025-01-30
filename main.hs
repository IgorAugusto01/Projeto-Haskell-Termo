import Funcoes

data Resultado = Corretas | Errado | Parcial deriving (Show, Eq)

tamanhoPalavra :: [a] -> Int
tamanhoPalavra [] = 0
tamanhoPalavra (x:xs) = 1 + tamanhoPalavra xs

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
aplicarCor Corretas letra = "\x1b[32m" ++ [letra] ++ "\x1b[0m"  -- verde
aplicarCor Errado letra = [letra]  
aplicarCor Parcial letra = "\x1b[33m" ++ [letra] ++ "\x1b[0m" -- amarelo



aplicarCorLetra :: (Resultado, Char) -> String
aplicarCorLetra (resultado,letra) = aplicarCor resultado letra
--x = correta
verificarResultado :: Palavra -> Palavra -> Palavra
verificarResultado [] [] = []
verificarResultado [] _ = []
verificarResultado _ [] = []
verificarResultado (x:xs) (y:ys) =
    aplicarCorLetra (compara x y, y) ++ verificarResultado xs ys
        where
            compara x y
                | x == y = Corretas
                | taNaLista (x:xs) y = Parcial
                | otherwise = Errado

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
                    let palavraColorida = verificarResultado palavraCerta tentativaUsuario
                    putStrLn ("Tentativa: " ++ show contLetraCerta)
                    putStrLn ("Tentativa: " ++  palavraColorida)
                    if(tentativaUsuario == palavraCerta)
                        then putStrLn "Acertou"
                        else
                            loop (n-1)        

main :: IO ()
main = do
 palavras <- recolherPalavras "BancoDePalavras.txt"
 let lista = criaLista ',' "" palavras
 --print lista
 jogar "Termo"
 
 
