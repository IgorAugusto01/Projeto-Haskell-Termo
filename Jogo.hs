module Jogo where

import Tipos
import Logica
import Grafico

jogo :: Palavra -> [Palavra] -> IO ()
jogo palavraCerta listaTentativas = loop 6 listaTentativas
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
        if tamanho tentativaUsuario /= tamanho palavraCerta
            then do
                putStrLn ("A palavra tem " ++ show (tamanho palavraCerta) ++ " letras!!")
                putStr("Aperte Enter para continuar...")
                getLine
                loop n tentativas
            else do
                let novasTentativas = atualizarTentativas tentativas palavraCerta (transformaMinusculo tentativaUsuario)
                if (transformaMinusculo tentativaUsuario) == palavraCerta
                    then do
                        limparTela
                        imprimirLista novasTentativas
                        putStrLn "Acertou!"
                    else loop (n-1) novasTentativas