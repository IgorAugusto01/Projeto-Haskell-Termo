module Grafico where

import Tipos
import System.Info (os)
import System.Process (system)

aplicarCor :: Resultado -> Char -> String
aplicarCor Corretas letra = "\x1b[32m" ++ [letra] ++ " " ++ "\x1b[0m"  -- verde
aplicarCor Errado letra = [letra] ++ " " 
aplicarCor Parcial letra = "\x1b[33m" ++ [letra] ++ " " ++ "\x1b[0m" -- amarelo

aplicarCorLetra :: (Resultado, Char) -> String
aplicarCorLetra (resultado,letra) = aplicarCor resultado letra

imprimirLista :: [Palavra] -> IO ()
imprimirLista xs = mapM_ putStrLn [x | x <- xs]
    
limparTela :: IO ()
limparTela = do
    let comando = if os == "mingw32" then "cls" else "clear"
    _ <- system comando
    return ()