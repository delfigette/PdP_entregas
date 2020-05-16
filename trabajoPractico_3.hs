import Text.Show.Functions()
import Data.List (genericLength)

type Propiedad = (String, Int)


data Persona = Persona   {
                                nombrePersona :: String, 
                                tacticaDeJuego :: String, 
                                cantidadDeDinero :: Int, 
                                propiedadesCompradas :: [Propiedad], 
                                acciones :: [Accion]
                            } deriving (Show)

type Accion = Persona -> Persona

-- cargar Jugadores

carolina = Persona "Carolina" "Accionista" 500 [] [pagarAAccionistas, pasarPorElBanco]

manuel = Persona "Manuel" "Oferente singular" 500 [] [enojarse, pasarPorElBanco]

--Alias útiles por si en un futuro decide cambiarse la cantidad de dinero que se da
dineroComienzoJuego :: Int
dineroComienzoJuego = 500
dineroPorPasarPorBanco :: Int
dineroPorPasarPorBanco = 40

-- FUNCIONES CAMBIAR

cambiarNombrePersona :: (String -> String) -> Persona -> Persona
cambiarNombrePersona unaFuncion unaPersona = unaPersona {nombrePersona = unaFuncion (nombrePersona unaPersona)} 

cambiarCantidadDeDinero :: (Int -> Int) -> Persona -> Persona
cambiarCantidadDeDinero unaFuncion unaPersona = unaPersona {cantidadDeDinero = unaFuncion (cantidadDeDinero unaPersona)}

cambiarTacticaDeJuego :: (String -> String) -> Persona -> Persona
cambiarTacticaDeJuego unaFuncion unaPersona = unaPersona {tacticaDeJuego = unaFuncion (tacticaDeJuego unaPersona) }


-- OTRAS FUNCIONES

agregarAcciones :: Accion -> Persona -> Persona
agregarAcciones accion unaPersona = unaPersona {acciones = accion : acciones unaPersona}

agregarPropiedades :: Propiedad -> Persona ->  Persona
agregarPropiedades unaPropiedad unaPersona = unaPersona {propiedadesCompradas = unaPropiedad : propiedadesCompradas unaPersona}

tieneLaTactica :: String -> Persona -> Bool
tieneLaTactica tactica unaPersona = tacticaDeJuego unaPersona == tactica


-- Acciones

pasarPorElBanco :: Persona -> Persona
pasarPorElBanco unaPersona = (cambiarTacticaDeJuego (const "Comprador compulsivo") . cambiarCantidadDeDinero ( + dineroPorPasarPorBanco)) unaPersona

enojarse :: Persona -> Persona
enojarse unaPersona = (cambiarCantidadDeDinero (+50) . agregarAcciones (gritar)) unaPersona

gritar :: Persona -> Persona
gritar unaPersona = cambiarNombrePersona ("AHHHH"++) unaPersona

puedeSubastar :: String -> Bool
puedeSubastar "Oferente singular" = True
puedeSubastar "Accionista" = True
puedeSubastar _ = False

subastar :: Persona -> Propiedad -> Persona
subastar unaPersona (nombrePropiedad, precio)
    | puedeSubastar (tacticaDeJuego unaPersona) = (cambiarCantidadDeDinero (subtract precio) . agregarPropiedades ((nombrePropiedad, precio))) unaPersona
    | otherwise = unaPersona

--cobrarAlquileres

esBarata :: Propiedad -> Bool
esBarata (_, precio) = precio < 150

dineroACobrarPorAlquileres :: [Propiedad] -> Int
dineroACobrarPorAlquileres propiedades = length (filter esBarata propiedades) * 10 + length (filter (not.esBarata) propiedades) * 20 

cobrarAlquileres :: Persona -> Persona
cobrarAlquileres unaPersona = cambiarCantidadDeDinero ( + (dineroACobrarPorAlquileres (propiedadesCompradas unaPersona))) unaPersona

--pagarAAccionistas

pagarAAccionistas :: Persona -> Persona
pagarAAccionistas unaPersona 
                                | tacticaDeJuego unaPersona == "Accionista" = cambiarCantidadDeDinero ( + 200) unaPersona
                                | otherwise = cambiarCantidadDeDinero (subtract 100) unaPersona

--hacerBerrinchePor

hacerBerrinchePor :: Persona -> Propiedad -> Persona
hacerBerrinchePor unaPersona unaPropiedad = (agregarPropiedades (unaPropiedad) . gritar . cambiarCantidadDeDinero (+10)) unaPersona 

--ultimaRonda 
ultimaRonda :: Persona -> Persona
ultimaRonda unaPersona
  = foldl (\persona accion -> accion persona) unaPersona (acciones unaPersona)

-- Me había propuesto hacerlo utilizando max, pero me fue difícil hacer la comparación entre dos personas sólo  por su cantidad de dinero
-- la idea incluso era después hacer contarDinero como una funcion lambda
-- pero no me funcionaba, así que preferí hacerlo de otra forma

-- juegoFinal :: Persona -> Persona -> Persona
-- juegoFinal persona1 persona2 = max (contarDinero (persona1)) (contarDinero (persona2))

-- contarDinero :: Persona -> Int
-- contarDinero unaPersona = cantidadDeDinero (ultimaRonda unaPersona)

juegoFinal :: Persona -> Persona -> Persona
juegoFinal unaPersona otraPersona
  | cantidadDeDinero (ultimaRonda unaPersona) < cantidadDeDinero (ultimaRonda otraPersona) = otraPersona
  | otherwise = otraPersona