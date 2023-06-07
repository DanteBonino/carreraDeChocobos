module Lib () where

type Pista = [Tramo]
type Tramo = (Int, (Chocobo -> Int))

data Chocobo = Chocobo {
    fuerza      :: Int,
    peso        :: Int,
    velocidad   :: Int,
    color       :: String
} deriving (Show, Eq)

data Jinete = Jinete {
    nombre  :: String,
    chocobo :: Chocobo
}deriving (Show, Eq)

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

--Punto 3:
primerosTresJinetes :: Pista -> [Jinete] -> [Jinete]
primerosTresJinetes   = primeroDeAlgo (take 3) menorSegunTiempoTotal

menorSegunTiempoTotal :: Pista -> Jinete -> Jinete -> Bool
menorSegunTiempoTotal pista   = menorSegun (tiempoTotalJinete  pista)

tiempoTotalJinete :: Pista -> Jinete -> Int
tiempoTotalJinete pista jinete = tiempoTotal (chocobo jinete) pista

--Punto 3:
quickSort :: (a -> a -> Bool) -> [a] -> [a]
quickSort _ [] = []
quickSort criterio (x : xs)= (quickSort criterio . filter(not . criterio x)) xs ++ [x] ++ (quickSort criterio . filter(criterio x)) xs

--Punto 4:
primerJinete :: Tramo -> [Jinete] -> Jinete
primerJinete  = primeroDeAlgo head menorSegunTiempoEnElTramo 

menorSegunTiempoEnElTramo :: Tramo -> Jinete -> Jinete -> Bool
menorSegunTiempoEnElTramo unTramo = menorSegun (tiempoDeJineteEnElTramo unTramo)

tiempoDeJineteEnElTramo :: Tramo -> Jinete -> Int
tiempoDeJineteEnElTramo unTramo  = (tiempoQueTardaEnElTramo unTramo . chocobo)

primeroDeAlgo :: ([Jinete]-> a) -> (b -> Jinete -> Jinete -> Bool) -> b -> [Jinete] -> a
primeroDeAlgo ultimaFuncion comparador algo = ultimaFuncion . quickSort (comparador algo)

--
ganoMasTramos :: Pista -> [Jinete] -> Jinete
ganoMasTramos unaPista unosJinetes = (elQueMasVecesAparece . ganadoresDeTramos unosJinetes) unaPista

elQueMasVecesAparece :: [Jinete] -> Jinete
elQueMasVecesAparece unosJinetes =  foldl1 (maximumBy (vecesQueAparece unosJinetes)) unosJinetes

vecesQueAparece :: [Jinete] -> Jinete -> Int
vecesQueAparece unaLista unElemento = (length . filter (== unElemento)) unaLista

maximumBy :: (Ord b)=>(a -> b) -> a -> a -> a
maximumBy trans unValor otroValor
    | trans unValor > trans otroValor = unValor
    | otherwise                       = otroValor

--Punto 5
nombresDeQuienesPuede :: Int ->Tramo -> [Jinete] ->  [String]
nombresDeQuienesPuede unTiempo unTramo = map nombre . filter (loHaceEnMenosDe unTiempo unTramo)

loHaceEnMenosDe :: Int -> Tramo -> Jinete -> Bool
loHaceEnMenosDe unTiempo unTramo = ((<unTiempo) . tiempoDeJineteEnElTramo unTramo)

--Punto 6:
estadisticasCarrera :: Pista -> [Jinete] -> [(String, Int, Int)]
estadisticasCarrera unaPista unosJinetes = map (armarEstadistica unaPista unosJinetes) unosJinetes

armarEstadistica :: Pista -> [Jinete] -> Jinete -> (String, Int, Int)
armarEstadistica unaPista unosJinetes unJinete = (nombre unJinete, vecesQueGanoUnTramo unaPista unosJinetes unJinete, tiempoTotalJinete  unaPista unJinete)

vecesQueGanoUnTramo :: Pista -> [Jinete] -> Jinete -> Int
vecesQueGanoUnTramo unaPista unosJinetes unJinete =  (flip vecesQueAparece unJinete . ganadoresDeTramos unosJinetes) unaPista

ganadoresDeTramos :: [Jinete] -> Pista -> [Jinete]
ganadoresDeTramos  unosJinetes = map (flip primerJinete unosJinetes) 

--Punto 7:
fuePareja :: Pista -> [Jinete] -> Bool
fuePareja unaPista unosJinetes = (and  .  zipWith (esDiezVecesMenor) (listaDeTiemposTotalesOrdenada unaPista unosJinetes) . tail . listaDeTiemposTotalesOrdenada unaPista) unosJinetes

listaDeTiemposTotalesOrdenada ::  Pista -> [Jinete] -> [Int]
listaDeTiemposTotalesOrdenada unaPista = map (tiempoTotalJinete unaPista) . primeroDeAlgo id menorSegunTiempoTotal unaPista

esDiezVecesMenor :: Int -> Int -> Bool
esDiezVecesMenor unValor  otroValor = otroValor <= (porcentajeDe 90 unValor)

porcentajeDe :: Int -> Int -> Int
porcentajeDe unPorcentaje unValor = div (unPorcentaje * unValor) 100

--Punto 8:
plateado :: Chocobo
plateado = Chocobo 500 500 500 "plateado"

--Punto 9
funcionHeavy :: (Ord t, Ord a)=>[(a,b)] -> (t,a) -> ((a,b) -> t) -> [t]
funcionHeavy x y z
    | (fst . head) x < snd y = map z x
    | otherwise = filter (fst y ==) (map z x)
