-- EJERCICIO 1
type Habilidades = [String]
data Animal = Animal { coeficiente :: Int, especie :: String, habilidades :: Habilidades }
perro = Animal 20 "perro" ["ladrar","comer","jugar"]

-- EJERCICIO 2
type Transformacion = Animal -> Animal

inteligenciaSuperior :: Int -> Transformacion
inteligenciaSuperior numeroDeIncrementacion unAnimal = unAnimal { coeficiente = ((+) numeroDeIncrementacion.coeficiente) unAnimal }

pinkificar :: Transformacion
pinkificar unAnimal = unAnimal { habilidades = [] }

superpoderes:: Transformacion
superpoderes unAnimal 
 | (especie unAnimal == "elefante") = agregarHabilidad "no tenerle miedo a los ratones" unAnimal
 | ((== "raton").especie) unAnimal && ((>100).coeficiente) unAnimal = agregarHabilidad "hablar" unAnimal
 | otherwise = unAnimal

agregarHabilidad :: String -> Transformacion
agregarHabilidad unaHabilidad unAnimal = unAnimal { habilidades = ((:) unaHabilidad.habilidades) unAnimal }

-- EJERCICIO 3
type CriterioDeExito = Animal -> Bool

antropomorfico :: CriterioDeExito
antropomorfico unAnimal = (habilidadDeHablar.habilidades) unAnimal && (coeficienteMayorA60.coeficiente) unAnimal

habilidadDeHablar :: Habilidades -> Bool
habilidadDeHablar = elem "hablar"

coeficienteMayorA60 :: Int -> Bool
coeficienteMayorA60 = (>60)

noTanCuerdo :: CriterioDeExito
noTanCuerdo = esMayorA2.habilidadesPinkiescas.habilidades

habilidadesPinkiescas :: Habilidades -> Habilidades
habilidadesPinkiescas = filter pinkiesco 

esMayorA2 :: Habilidades -> Bool
esMayorA2 = (>2).length

-- EJERCICIO 4
data Experimento = Experimento { transformaciones :: [Transformacion] , criterioDeExito :: CriterioDeExito }

experimentoExitoso :: Experimento -> CriterioDeExito
experimentoExitoso unExperimento unAnimal = ((criterioDeExito unExperimento).(animalTransformado unAnimal.transformaciones)) unExperimento

animalTransformado:: Animal -> [Transformacion] -> Animal
animalTransformado unAnimal listaTransformaciones = (foldl1 (.) listaTransformaciones) unAnimal

{- 
"En un ratón de coeficiente intelectual 17, con habilidades de destruenglonir el mundo y hacer planes desalmados, 
hacer un experimento que consista en pinkificarlo, luego darle inteligencia superior de 10 y por último darle superpoderes. 
Como criterio de éxito, ver si quedó antropomórfico" 

> experimentoExitoso (Experimento [pinkificar, (inteligenciaSuperior 10), superpoderes] antropomorfico) (Animal  17 "raton" [ "destruenglonir el mundo", "hacer planes desalmados"])
> False
-} 

-- EJERCICIO 5
type Funcion = Int -> Bool

reporte1 :: [Habilidades] -> [Transformacion] -> [Animal] -> [Int]
reporte1 listaHabilidades listaTransformaciones = map coeficiente.(nuevosAnimales listaHabilidades listaTransformaciones funcion)
 where funcion = (>0)

nuevosAnimales :: [Habilidades] -> [Transformacion] -> Funcion ->[Animal] -> [Animal]
nuevosAnimales listaHabilidades listaTransformaciones funcion = filtradoDeAnimales listaHabilidades funcion .(animalesTransformados listaTransformaciones)

animalesTransformados :: [Transformacion] -> [Animal] -> [Animal]
animalesTransformados listaTransformaciones = map (\animal -> animalTransformado animal listaTransformaciones)

filtradoDeAnimales :: [Habilidades] -> Funcion -> [Animal] -> [Animal]
filtradoDeAnimales listaHabilidades funcion = filter ((existeInterseccion funcion listaHabilidades).habilidades)

existeInterseccion :: Funcion -> [Habilidades] -> [Habilidades] -> Bool
existeInterseccion funcion listaHabilidades = funcion.length.intersect listaHabilidades

reporte2 :: [Habilidades] -> [Transformacion] -> [Animal] -> [String]
reporte2 listaHabilidades listaTransformaciones = map especie.(nuevosAnimales listaHabilidades listaTransformaciones funcion)
 where funcion = (==length listaHabilidades)

reporte3 :: [Habilidades] -> [Transformacion] -> [Animal] -> [Int]
reporte3 listaHabilidades listaTransformaciones = map (length.habilidades).(nuevosAnimales listaHabilidades listaTransformaciones funcion)
 where funcion = (==0)

-- EJERCICIO 6
{- 
Aparece un nuevo animal que tiene infinitas capacidades. 
Dar ejemplos de experimentos que se puedan realizar y que no, si los hubiera. Justificar conceptualmente. 
	No se puede realizar ningún experimento si el animal tiene habilidades infinitas, ya que todos los criteriosDeExito evaluarían listas infinitas.
-}

-- BONUS
-- EJERCICIO A
pinkiesco :: String -> Bool
pinkiesco unaHabilidad = empiezaConHacer unaHabilidad && palabraPinkiesca unaHabilidad

empiezaConHacer:: String -> Bool
empiezaConHacer = (=="hacer").take 5

palabraPinkiesca :: String -> Bool
palabraPinkiesca unaHabilidad = tiene4Letras elRestoDeHabilidad && tieneUnaVocal elRestoDeHabilidad
 where elRestoDeHabilidad = drop 6 unaHabilidad

tiene4Letras :: String -> Bool
tiene4Letras = (<=4).length

tieneUnaVocal :: String -> Bool
tieneUnaVocal = (>=1).length.filter esVowel 
 where esVowel letra = elem letra "AaEeIiOoUu"

-- EJERCICIO B
generarTodasLasPalabrasPinkiescas = (filtrarPinkiescas.agregarHacer.generateWordsUpTo) 4
 
agregarHacer :: [String] -> [String]
agregarHacer = map ("hacer " ++) 

filtrarPinkiescas :: [String] -> [String]
filtrarPinkiescas = filter pinkiesco 

-- EJERCICIO C
generateWordsUpTo :: Int -> [String]
generateWordsUpTo = (concat.map generateWords) [1,2,3,4]