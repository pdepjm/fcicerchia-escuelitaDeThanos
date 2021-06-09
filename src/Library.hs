module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

--PUNTO 1

data Personaje = UnPersonaje{
    edad :: Number,
    energia :: Number,
    habilidades :: [String],
    nombre :: String,
    planeta :: String
}deriving (Show,Eq)

data Guantelete = UnGuantelete{
    material :: String,
    gemas :: [Gema]
}deriving (Show,Eq)

type Gema = Personaje -> Personaje
type Universo = [Personaje]

chasquido :: Guantelete -> Universo -> Universo
chasquido guantalete universo 
                    | puedeUsarGuantalete guantalete = reduceALaMitad universo
                    | otherwise = universo

puedeUsarGuantalete :: Guantelete -> Bool
puedeUsarGuantalete guantalete = ((==6).length.gemas) guantalete && ((=="uru").material) guantalete

reduceALaMitad :: Universo -> Universo
reduceALaMitad universo = take (div (length universo) 2) universo

--PUNTO 2

--a
aptoParaPendex :: Universo -> Bool
aptoParaPendex universo = any esPersonajePendex universo

esPersonajePendex :: Personaje -> Bool
esPersonajePendex = (<45).edad

--b
energiaTotal :: Universo -> Number
energiaTotal = sum.(map energia).filtrarPersonajesPorHabilidad

filtrarPersonajesPorHabilidad :: Universo -> Universo
filtrarPersonajesPorHabilidad universo = filter ((>1).length.habilidades) universo

--PUNTO 3

mente :: Number -> Gema
mente = quitarEnergia

alma :: String -> Gema
alma habilidad = (quitarEnergia 10).(eliminarHabilidad habilidad)

eliminarHabilidad :: String -> Gema
eliminarHabilidad habilidad personaje = personaje {habilidades = filter (/= habilidad) (habilidades personaje)}

quitarEnergia :: Number -> Gema
quitarEnergia valor personaje = personaje {energia = energia personaje - valor}

type Planeta = String

espacio :: Planeta -> Gema
espacio nuevoPlaneta personaje = quitarEnergia 20 (personaje {planeta = nuevoPlaneta})

poder :: Gema
poder personaje = quitarEnergia (energia personaje) (atacarHabilidad personaje)

atacarHabilidad :: Gema
atacarHabilidad personaje | (length.habilidades) personaje <=2 = quitarHabilidades personaje
                          | otherwise = personaje

quitarHabilidades :: Gema
quitarHabilidades personaje = personaje {habilidades = []}

tiempo :: Gema
tiempo personaje = quitarEnergia 50 (personaje {edad = max 18 (div (edad personaje) 2)})

gemaLoca :: Gema -> Gema
gemaLoca gema = gema.gema

--PUNTO 4

ejGuantelete = UnGuantelete "goma" [tiempo,alma "usar Mjolnir",gemaLoca (alma "programacion en Haskell")]

--PUNTO 5

type Enemigo = Personaje

utilizar :: [Gema] -> Enemigo -> Enemigo
utilizar gemas enemigo = foldl utilizarCadaGema enemigo gemas

utilizarCadaGema :: Enemigo -> Gema -> Enemigo
utilizarCadaGema enemigo gema = gema enemigo

--PUNTO 6

gemaMasPoderosa :: Guantelete -> Personaje -> Gema
gemaMasPoderosa guantelete = obtenerGemaMasPoderosa (gemas guantelete)

obtenerGemaMasPoderosa :: [Gema] -> Personaje -> Gema
obtenerGemaMasPoderosa [gema] _ = gema
obtenerGemaMasPoderosa (gema1 : gema2 : gemas) personaje
                            | (energia.gema1) personaje < (energia.gema2) personaje = obtenerGemaMasPoderosa (gema1 : gemas) personaje
                            | otherwise = obtenerGemaMasPoderosa (gema2 : gemas) personaje

--PUNTO 7