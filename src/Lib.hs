module Lib () where

type Pista = [Tramo]
type Tramo = (Int, (Chocobo -> Int))

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

--Punto 2:
tiempoQueTardaEnElTramo :: Tramo -> Chocobo -> Int
tiempoQueTardaEnElTramo  unTramo = (div (distancia unTramo) . correccionDeVelocidad unTramo)

--Se podría usar fst y snd de una, pero es más claro qué está haciendo
distancia :: Tramo -> Int
distancia = fst

correccionDeVelocidad :: Tramo -> (Chocobo -> Int)
correccionDeVelocidad = snd

--b
tiempoTotal :: Chocobo -> Pista -> Int
tiempoTotal  unChocobo = sum . map (flip tiempoQueTardaEnElTramo unChocobo)


