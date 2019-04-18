
main :: IO ()    
main = return ()

-- 3.1
data Auto = Auto {
nombre :: String,
nafta :: Int,
velocidad :: Int,
match :: String,
truco :: String
} deriving Show

rochaMcQueen = Auto "RochaMcQueen" 300 0 "Ronco" "deReversaRocha"
biankker = Auto "Biankker" 500 20 "Tinch" "impresionar"
gushtav = Auto "Gushtav" 200 130 "PetiLaLinda" "nitro"
rodra = Auto "Rodra" 0 50 "Taisa" "fingirAmor"

-- Trucos

deReversaRocha :: Auto -> Auto
deReversaRocha unAuto = unAuto { nafta =  (+ nafta unAuto) 200  }
impresionar :: Auto -> Auto
impresionar unAuto = unAuto { velocidad = (* velocidad unAuto) 2  }
nitro :: Auto -> Auto
nitro unAuto = unAuto { velocidad = velocidad unAuto + 15 }
fingirAmor :: Auto -> Auto
fingirAmor unAuto = unAuto { match = "Rocio"}

-- Funcion para ver el truco
verTruco :: Auto -> String
verTruco (Auto _ _ _ _ truco) = truco

-- Aplicando Truco
aplicarTruco :: Auto -> Auto
aplicarTruco unAuto
 | verTruco unAuto == "nitro" = nitro unAuto
 | verTruco unAuto == "deReversaRocha" = deReversaRocha unAuto
 | verTruco unAuto == "impresionar" = impresionar unAuto
 | verTruco unAuto == "fingirAmor" = fingirAmor unAuto
 | otherwise = unAuto

-- 3.2
verMatch :: Auto -> String
verMatch (Auto _ _ _ match _) = match

esVocal :: Char -> Bool
esVocal letra = letra == 'a' || letra == 'e' || letra == 'i' || letra == 'o' || letra == 'u'

cantVoc :: Auto -> Int
cantVoc auto = length (filter esVocal (verMatch auto))


incVelocidad :: Auto -> Auto
incVelocidad auto
  | (cantVoc auto) >=1 && (cantVoc auto) <= 2 = auto {velocidad = velocidad auto + 15}
  | (cantVoc auto) >=3 && (cantVoc auto) <= 4 = auto {velocidad = velocidad auto + 20}
  | (cantVoc auto) >4 = auto {velocidad = velocidad auto + 30}
  | otherwise = auto

-- 3.3
verNafta :: Auto -> Int
verNafta (Auto _ nf _ _ _ ) = nf

verVelocidad :: Auto -> Int
verVelocidad (Auto _ _ vel _ _ ) = vel 

puedeRealizarTruco :: Auto -> Bool
puedeRealizarTruco auto = nafta auto > 0 && velocidad auto <100

-- 3.4
comboLoco :: Auto -> Auto
comboLoco = nitro.deReversaRocha

queTrucaso :: Auto -> String -> Auto
queTrucaso auto newmatch = incVelocidad auto {match = newmatch}

turbo :: Auto -> Auto
turbo auto = auto {nafta = nafta auto * 0 , velocidad = velocidad auto + (nafta auto *10)}

