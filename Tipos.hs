module Tipos where

type Palavra = String

type Jogo = (Palavra,[Palavra]) --Palavra = palavra correta

data Resultado = Corretas | Errado | Parcial deriving (Show, Eq)