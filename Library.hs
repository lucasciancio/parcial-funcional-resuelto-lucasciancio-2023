module Library where
import PdePreludat

doble :: Number -> Number
doble numero = numero + numero

data Animal = Animal {
    nombre :: String,
    tipo :: String,        
    peso :: Number,
    edad :: Number,
    enfermo :: Bool,
    visitas :: [Visita]
} deriving (Show, Eq)


data Visita = Visita {
    diasRecuperacion :: Number,
    costo :: Number
} deriving (Show, Eq)


-- CASOS DE PRUEBA
-- __________________________________________________________________________________________


visita1 :: Visita
visita1 = Visita {
    diasRecuperacion = 40,
    costo = 100
}

visita2 :: Visita
visita2 = Visita {
    diasRecuperacion = 20,
    costo = 200
}


bobi :: Animal
bobi = Animal {
    nombre = "Bobi",
    tipo = "Perro",        
    peso = 5,
    edad = 8,
    enfermo = False,
    visitas = [visita1, visita2]
}


oscar :: Animal
oscar = Animal {
    nombre = "Oscar",
    tipo = "Oveja",        
    peso = 10,
    edad = 5,
    enfermo = False,
    visitas = [visita2]
}


dorothy :: Animal
dorothy = Animal {
    nombre = "Dorothy",
    tipo = "Vaca",
    peso = 690,
    edad = 15,
    enfermo = True,
    visitas = []
}


-- PUNTO 1
-- _______________________________________________________________________________________________

laPasoMal :: Animal -> Bool                             -- OK
laPasoMal = any((>30) . diasRecuperacion) . visitas

nombreFalopa :: Animal -> Bool                          -- OK
nombreFalopa = (== 'i') . head . reverse . nombre      



-- PUNTO 2
-- _________________________________________________________________________________________________

type Actividad = Animal -> Animal


engorde :: Number -> Actividad          -- OK
engorde kilosAlimento animal = animal {peso = peso animal + min 5 (div  kilosAlimento 2) }


agregarVisita :: Visita -> Animal -> Animal     -- OK
agregarVisita visita animal = animal {visitas = visitas animal ++ [visita]}


revisacion :: Visita -> Actividad       -- OK
revisacion visita animal
    | enfermo animal = (engorde 2 . agregarVisita visita) animal
    | otherwise = animal


festejoCumple :: Actividad           -- OK
festejoCumple animal = animal {
    edad = edad animal + 1,
    peso = peso animal - 1
}

chequeoPeso :: Number -> Actividad           -- OK
chequeoPeso normal animal
    | ((> normal) . peso) animal = animal {enfermo = True}
    | otherwise = animal


-- PUNTO 3
-- _______________________________________________________________________________________________

type Proceso = [Actividad]

aplicarProceso :: Animal -> Proceso -> Animal           -- OK
aplicarProceso animal = foldr ($) animal . reverse

-- Ejemplo:

proceso1 :: Proceso
proceso1 = [engorde 12, revisacion visita1, festejoCumple, chequeoPeso 20]

-- ghci> aplicarProceso bobi proceso1
-- (devuelve un animal)


-- PUNTO 4
-- ____________________________________________________________________________________________


cambioSustentable :: Animal -> Actividad -> Bool       
cambioSustentable animal actividad = 
    (peso (actividad animal) - peso animal) > 0 && (peso (actividad animal) - peso animal) <= 3


-- CAMBIOS NO SUBIDOS AL REPOSITORIO (corrección)

mejora :: Animal -> Proceso -> Bool          -- OK
mejora _ [] = True
mejora animal (actividad : actividades) =
    --cambioSustentable animal actividad && mejora animal actividades
    -- INCORRECTO, ya que compara el peso del animal tras realizar cada actividad con el peso original, que
    -- no es lo que se pide.
    cambioSustentable animal actividad && mejora (actividad animal) actividades -- CORRECTO.
    -- para cada actividad toma como referencia el peso del animal luego de haber aplicado todas las
    -- actividades anteriores.



-- PUNTO 5
-- ____________________________________________________________________________________________


primerosTresFalopa :: [Animal] -> [Animal]              -- OK
primerosTresFalopa = take 3 . filter nombreFalopa


-- Haskell cuenta con un mecanismo llamado evaluación diferida, que le permite evaluar sólo aquellas
-- expresiones que son necesarias para realizar una tarea. Si le pasamos una lista infinita de animales
-- pueden pasar dos cosas, dependiendo de cómo sea esa lista:

-- 1. Si la lista incluye al menos 3 animales con nombre falopa, entonces el programa terminará exitosamente
--    en cuanto la lista generada por filter llegue a tener 3 elementos. Gracias a la evaluación diferida, no
--    no se evalúa el resto de la lista, ya que no es necesario.

-- 2. Si la lista no incluye al menos 3 animales con nombre falopa, entonces, a pesar de la evaluación
--    diferida, el programa nunca terminará, porque la condición de corte (tener una lista 3 animales con
--    nombre falopa) nunca será alcanzada.