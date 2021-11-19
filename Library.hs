module Library where
import PdePreludat

-- De cada país se conoce su denominación
--, la cantidad de habitantes y una serie de indicadores que representan 
-- el estado de su situación.

data Pais = UnPais {
    nombre :: String,
    habitantes :: Number,
    indicadores :: [(String, Number)]
} deriving ( Show, Eq, Ord )

-- Por ejemplo, para un país llamado “flandria" con 4000 habitantes,
-- sus indicadores son: 2.4% de desocupación, la deuda externa estimada en 8000,
-- un IVA del 21%, las reservas valuadas en 3000 y un índice educativo de 100.
-- Otro país podría tener otros indicadores, pero siempre con un valor numérico asociado a una
-- descripción que indica de qué se trata.


flandria = UnPais "Flandria" 4000 [("Desocupacion", 0.024),("Deuda Externa", 8000),("IVA", 0.21),("Reservas", 3000),("Indice Educativo", 100)]

-- 1 ) Crecimiento vegetativo: la población del país aumenta cada período un 5%. Hacer la función que permita que crezca la población del país durante un período.
crecimientoVegetativo :: Pais -> Pais
crecimientoVegetativo pais = pais { habitantes = pasarPeriodo ( habitantes pais )}

pasarPeriodo :: Number -> Number
pasarPeriodo numero = numero + ( numero * 5/100 )

-- 2 ) A ) Hacer una función para analizar si un país está bien en un determinado momento, 
-- que consiste en ver si la deuda externa por habitante es mayor al iva vigente.
descripcion = fst
valorNumerico = snd

estaBien :: Pais -> Bool
estaBien pais = deudaExternaPerCapita pais > valores pais "IVA"

deudaExternaPerCapita :: Pais -> Number
deudaExternaPerCapita pais = ( valores pais "Deuda Externa") / ( habitantes pais )

valores :: Pais -> String -> Number
valores pais s = valorNumerico( head ( filter (\(x,y) -> x == s) ( indicadores pais ) ) )

-- B ) Hacer una función que permita saber si un país tiene futuro, que es cuando el índice educativo es superior a un valor dado. 
tieneFuturo :: Pais -> Number -> Bool
tieneFuturo pais valor = valores pais "Indice Educativo" > valor

-- C ) Inventar una nueva función booleana que nos diga algo sobre un país, en la que se contemplen todos los indicadores.
indices = ["Desocupacion","Deuda Externa","IVA","Reservas","Indice Educativo"]

despegaPais :: Pais -> Bool
despegaPais pais = sum ( map (valores pais) indices ) > ( valores pais "Deuda Externa") + (valores pais "Desocupacion")*10000

-- D ) Definir una función, que articulandose con cualquiera de las funciones anteriores, pueda analizar cómo está un conjunto de países y obtener los nombres de los países que satisfagan la condición indicada.
uganda = UnPais "uganda" 4000 [("Desocupacion", 11),("Deuda Externa", 8000),("IVA", 0.21),("Reservas", 3000),("Indice Educativo", 100)]
varsovia = UnPais "varsovia" 4000 [("Desocupacion", 0.85),("Deuda Externa", 800011111),("IVA", 0.89),("Reservas", 10),("Indice Educativo", 100)]
listaPaises = [flandria, uganda, varsovia]
 
funcion condicion paises = map nombre ( filter (\ x -> condicion x ) paises )


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
data Politico = UnPolitico {
    nombreP :: String,
    transformacion :: Pais -> Pais
} deriving (Show, Eq, Ord)

menosTupla :: Pais -> String -> [(String,Number)]
menosTupla pais s = filter (\(x,y)-> x /= s) ( indicadores pais )

quitarDiezPorCiento :: Number -> Number
quitarDiezPorCiento numero = numero - (numero) *10/100

restarUno :: Number -> Number
restarUno numero = numero - 1

tupla :: String -> Pais -> (String,Number)
tupla indicador pais = head ( filter (\(x,y) -> x == indicador) ( indicadores pais ) )

deteriorarEducacion :: Pais -> Pais
deteriorarEducacion pais = pais { indicadores = f  ( quitarDiezPorCiento ) ( tupla "Indice Educativo" pais): (menosTupla pais "Indice Educativo" ) }

f g (x,y) = (x, g y )

bajarDesocupacion :: Pais -> Pais
bajarDesocupacion pais = pais { indicadores = f  (restarUno) ( tupla "Desocupacion" pais): (menosTupla pais "Desocupacion" ) }

descocupacionPorEncimaDeDiez :: Pais -> Bool
descocupacionPorEncimaDeDiez pais = valores pais "Desocupacion" > 10 

hormigaIgnorante = UnPolitico "Hormiga Ignorante" transformacionHormiga

transformacionHormiga :: Pais -> Pais
transformacionHormiga pais | valores pais "Desocupacion" > 10 = (bajarDesocupacion.deteriorarEducacion) pais
                           | otherwise = deteriorarEducacion pais

----------------------------------------------------------------------------------------------------------------------------------------

ivaEnVeintiCuatro :: Number -> Number
ivaEnVeintiCuatro  numero = 24

cuarentaPorciento :: Number -> Number
cuarentaPorciento numero = numero + numero * 0.4

poneElIvaEnVeinticuatro :: Pais -> Pais
poneElIvaEnVeinticuatro pais = pais { indicadores = f  (ivaEnVeintiCuatro) ( tupla "IVA" pais): (menosTupla pais "IVA" ) }

mejoraEducacion :: Pais -> Pais
mejoraEducacion pais = pais { indicadores = f  (cuarentaPorciento) ( tupla "Indice Educativo" pais): (menosTupla pais "Indice Educativo" ) }

eduqueitor = UnPolitico "Eduqueitor" transformacionEduqueitor

transformacionEduqueitor :: Pais -> Pais
transformacionEduqueitor = (poneElIvaEnVeinticuatro.mejoraEducacion)


duplicar :: Number -> Number
duplicar numero = 2 * numero

empiezaConD pais = head( filter(\(x,y) -> head x == 'D') (indicadores pais) )

transformacionDuplicador :: Pais -> Pais
transformacionDuplicador pais = pais { indicadores = f (duplicar) (empiezaConD pais):indicadores pais}

duplicador = UnPolitico "Duplicador" transformacionDuplicador



cazaBuitre = UnPolitico "Caza Buitre" transformacionCazaBuitre

transformacionCazaBuitre :: Pais -> Pais
transformacionCazaBuitre pais = pais { indicadores = f (0*) (tupla "Deuda Externa" pais):(menosTupla pais "Deuda Externa")}


-- Personal: Agregar una nueva forma de gobernar
-- Gobierna el empresario qatarí,las reservas aumentan x500 y el desempleo desciende a 0

qatari = UnPolitico "Qatari" transformacionQatari

transformacionPetrolera pais = pais { indicadores = f (500*) (tupla "Reservas" pais):(menosTupla pais "Reservas")}
chauDesempleo pais = pais{ indicadores = f (0*) (tupla "Desocupacion" pais):(menosTupla pais "Desocupacion")}

transformacionQatari :: Pais -> Pais
transformacionQatari = transformacionPetrolera.chauDesempleo


-----------------------------------------------------------------------------------------------------------------------------

transformar :: Pais -> Politico -> Pais
transformar pais politico = ((crecimientoVegetativo).(transformacion politico)) pais


fuerzasPoliticas = [hormigaIgnorante, eduqueitor,duplicador, cazaBuitre]

-- Mostrar cómo se usa la función anterior con las 6 fuerzas políticas explicadas previamente.
transformarPais :: Pais -> [Pais]
transformarPais pais = map (transformar pais) fuerzasPoliticas

{- *Spec Library Spec> transformarPais uganda
[UnPais {nombre = "uganda", habitantes = 4200, indicadores = [("Desocupacion",10),("Indice Educativo",90),("Deuda Externa",8000),("IVA",0.21),("Reservas",3000)]},
UnPais {nombre = "uganda", habitantes = 4200, indicadores = [("IVA",24),("Indice Educativo",140),("Desocupacion",11),("Deuda Externa",8000),("Reservas",3000)]},
UnPais {nombre = "uganda", habitantes = 4200, indicadores = [("Desocupacion",22),("Desocupacion",11),("Deuda Externa",8000),("IVA",0.21),("Reservas",3000),("Indice Educativo",100)]},
UnPais {nombre = "uganda", habitantes = 4200, indicadores = [("Deuda Externa",0),("Desocupacion",11),("IVA",0.21),("Reservas",3000),("Indice Educativo",100)]}] 
-}


-- Se permiten sucesivas reelecciones, aunque con límites. Esto implica que si luego de gobernar un período el 
-- país está bien y verifica una cierta condición, la fuerza política es reelegida y gobierna por un período más.
-- A su vez, hay un máximo de reelecciones permitida, independientemente de cómo gobierne. Determinar cómo va a quedar 
-- el país cuando se retire del gobierno una determinada fuerza política luego de sus periodos de gobierno, 
-- ya sea porque llegó al máximo de reelecciones permitidas o porque luego de algún período no verificó la 
-- condición dada.  
-- Por ejemplo, se desa saber cómo quedará frandria luego que la gobierne la hormiga ignorante, con un máximo de 
-- 3 reelecciones y usando como criterio de reelección que tiene futuro con un valor de referencia 85.
