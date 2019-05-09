module Kars where
import Text.Show.Functions
import Data.Char (toUpper, isUpper)

data Auto = Auto {
nombre :: Nombre,
nafta :: Nafta,
velocidad :: Velocidad,
match :: Match,
truco :: Truco
} deriving Show

data Carrera = Carrera{
    vueltas :: Vueltas,
	longitud :: Longitud,
	publico :: [Nombre],
	trampa :: Trampa,
	participantes :: [Auto]
} deriving Show

type Nombre = String
type Nafta = Int
type Velocidad = Int
type Match = String 
type Truco = Auto -> Auto
type Vueltas = Int
type Longitud = Int
type Trampa = Carrera -> Carrera

-- Autos
rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" deReversaRocha
biankker = Auto "Biankker" 500 20 "Tinch" impresionar
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" nitro
rodra = Auto "Rodra" 0 50 "Taisa" fingirAmor
-- Carreras
potreroFunes = Carrera 3 5 ["Ronco", "Tinch", "Dodain"] sacarAlPistero [rochaMcQueen,biankker,gushtav,rodra]

-- Accessors
mapNafta :: (Int->Int) -> Auto -> Auto
mapNafta f unAuto = unAuto {nafta= f.nafta $ unAuto}
mapVelocidad :: (Int->Int) -> Auto -> Auto
mapVelocidad f unAuto = unAuto {velocidad= f.velocidad $ unAuto} 
mapMatch :: String -> Auto -> Auto
mapMatch nombre unAuto = unAuto {match= nombre}
mapParticipantes :: ( [Auto] -> [Auto]) -> Carrera -> Carrera
mapParticipantes f unaCarrera = unaCarrera {participantes= f.participantes $ unaCarrera}

-- Trucos
deReversaRocha :: Truco
deReversaRocha unAuto = mapNafta  (+ (velocidad unAuto `div` 5)) unAuto
impresionar :: Truco
impresionar  = mapVelocidad (*2)
nitro :: Truco
nitro = mapVelocidad (+15)
fingirAmor :: Truco
fingirAmor unAuto = mapMatch "AlguienNuevo" unAuto
inutilidad :: Truco
inutilidad = id

-- Incrementar Velocidad
esVocal :: Char -> Bool
esVocal unMatch = elem unMatch "AEIOUaeiou"
cantVocales :: Auto -> Int
cantVocales unAuto = length (filter esVocal (match unAuto)) --componer

incrementarVelocidad :: Truco
incrementarVelocidad unAuto
  | cantVocales unAuto <= 2 = mapVelocidad (+15) unAuto
  | cantVocales unAuto <= 4 = mapVelocidad (+20) unAuto
  | otherwise = mapVelocidad (+30) unAuto

-- Puede Realizar Truco
puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco unAuto = nafta unAuto > 0 && velocidad unAuto < 100

-- Nuevos Trucos
comboLoco :: Truco
comboLoco = nitro.deReversaRocha
queTrucazo :: String -> Truco
queTrucazo newMatch unAuto = incrementarVelocidad unAuto {match = newMatch}
turbo :: Truco
turbo unAuto = mapVelocidad (+ nafta unAuto * 10) .mapNafta (\_ -> 0) $ unAuto

-- Trampas
sacarAlPistero :: Trampa
sacarAlPistero unaCarrera = mapParticipantes tail unaCarrera

lluvia:: Trampa
lluvia unaCarrera = unaCarrera {participantes = disminuirVelocidad unaCarrera }
disminuirVelocidad :: Carrera -> [Auto]
disminuirVelocidad unaCarrera =	 map (mapVelocidad (subtract 10)).participantes $ unaCarrera

neutralizarTrucos :: Trampa
neutralizarTrucos unaCarrera = mapParticipantes id unaCarrera

--mapParticipantes :: ( [Auto] -> [Auto]) -> Carrera -> Carrera
pocaReserva :: Trampa
pocaReserva unaCarrera = mapParticipantes (filter muchaNafta) unaCarrera

muchaNafta :: Auto -> Bool
muchaNafta unAuto = (>30).nafta $ unAuto

podio :: Trampa 
podio unaCarrera = mapParticipantes (take 3) unaCarrera


-- A Correr!!

darVuelta :: Carrera -> Carrera
darVuelta unaCarrera = trampaTodos. trucoxMatch . restarNafta $ unaCarrera

restarNafta :: Carrera -> Carrera
restarNafta unaCarrera = mapParticipantes (modificarVelocidad (longitud unaCarrera))  unaCarrera

modificarVelocidad :: Longitud -> [Auto] -> [Auto]
modificarVelocidad _ [] = []
modificarVelocidad kilometros (x:xs) = x {nafta = nafta x - (velocidad x * kilometros)} : modificarVelocidad kilometros xs

trucoxMatch :: Carrera -> Carrera
trucoxMatch unaCarrera = mapParticipantes realizarTruco unaCarrera
realizarTruco :: [Auto] -> [Auto]
realizarTruco [] = []
realizarTruco (x:xs)
 | puedeRealizarTruco x  = (truco x) x : realizarTruco xs
 | otherwise = x : realizarTruco xs


trampaTodos :: Carrera -> Carrera
trampaTodos unaCarrera = mapParticipantes sufreTrampa unaCarrera


sufreTrampa :: [Auto] -> [Auto]
sufreTrampa [] = []
sufreTrampa (x:xs) = (truco x) x : sufreTrampa xs

correrCarrera :: Carrera -> Carrera
correrCarrera unaCarrera = aplicarxVueltas (unaCarrera) (vueltas unaCarrera)

aplicarxVueltas :: Carrera -> Int -> Carrera
aplicarxVueltas unaCarrera 0 = unaCarrera
aplicarxVueltas unaCarrera vueltas = aplicarxVueltas (darVuelta unaCarrera) (vueltas - 1)

-- 3.5	

quienGana :: Carrera -> Auto
quienGana  =  autoGanador.participantes
autoGanador :: [Auto] -> Auto
autoGanador (x:xs) = foldl1 unovsuno (x:xs)
unovsuno :: Auto -> Auto -> Auto
unovsuno unAuto otroAuto
 | velocidad unAuto > velocidad otroAuto = unAuto
 | otherwise = otroAuto

elGranTruco :: [Truco] -> Auto -> Auto
elGranTruco listaTrucos unAuto = foldr ($) unAuto (listaTrucos)

-- 3.6 Una Gran Carrera
-- a) ¿Podemos correrla? -- Si bien la función "correrCarrera" se ejecutaria, no tendriamos resultado por pantalla.
-- b) ¿Podemos conocer el primer participante luego de 2 vueltas? Sí, realizando la función head nos traeria el primer auto de cualquier
-- lista infinita, por ejemplo "head [1..]" nos devuelve 1
-- c) ¿Podemos dar la primera vuelta de la carrera? Sí, aplicando "aplicarxVueltas unaCarreraInfinita 1", pero nuevamente no tendriamos resultado en pantalla.
