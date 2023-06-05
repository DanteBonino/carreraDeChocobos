module Lib () where

type Pista = [Tramo]
type Tramo = (Int, (Chocobo -> Chocobo))

data Chocobo = Chocobo {
    fuerza      :: Int,
    peso        :: Int,
    velocidad   :: Int,
    color       :: String
}

data Jinete = Jinete {
    nombre  :: String,
    chocobo :: Chocobo
}

--Punto 1:
mayorSegun :: (Ord b) => (a -> b) -> a -> a -> Bool
mayorSegun = esDeCiertaFormaSegun (>)

menorSegun :: (Ord b) => (a -> b) -> a -> a -> Bool
menorSegun = esDeCiertaFormaSegun (<)

--No se me ocurre un buen Nombre para la abstracción, pero:
esDeCiertaFormaSegun :: (Ord b) => (b -> b -> Bool) ->(a -> b) ->  a -> a -> Bool
esDeCiertaFormaSegun unaCondicion transformador  unValor = (unaCondicion (transformador unValor) . transformador)
