import Text.Show.Functions()
import Data.List (genericLength)

data Persona = Persona   {
                                nombrePersona :: String, 
                                tacticaDeJuego :: String, 
                                cantidadDeDinero :: Int, 
                                propiedadesCompradas :: [(String, Int)], 
                                acciones :: [String]
                            } deriving (Show, Eq)
                            
-- cargar Jugadores

carolina = Persona "Carolina" "Accionista" 0 [] ["pagarAAccionista"]

manuel = Persona "Manuel" "Oferente singular" 0 [] ["enoajrse"]

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

agregarAcciones :: String -> Persona-> Persona
agregarAcciones accion unaPersona = unaPersona {acciones = accion : acciones unaPersona}

agregarPropiedades :: (String, Int) -> Persona ->  Persona
agregarPropiedades unaPropiedad unaPersona = unaPersona {propiedadesCompradas = unaPropiedad : propiedadesCompradas unaPersona}

tieneLaTactica :: String -> Persona -> Bool
tieneLaTactica tactica unaPersona = tacticaDeJuego unaPersona == tactica

--Accion agregada para comenzar juego

comenzarJuego :: Persona -> Persona
comenzarJuego unaPersona = (cambiarCantidadDeDinero ( + dineroComienzoJuego) . (agregarAcciones "pasarPorElBanco")) unaPersona


-- Acciones

pasarPorElBanco :: Persona -> Persona
pasarPorElBanco unaPersona = (cambiarTacticaDeJuego (const "Comprador compulsivo") . cambiarCantidadDeDinero ( + dineroPorPasarPorBanco)) unaPersona

enojarse :: Persona -> Persona
enojarse unaPersona = (cambiarCantidadDeDinero (+50) . agregarAcciones ("gritar")) unaPersona

gritar :: Persona -> Persona
gritar unaPersona = cambiarNombrePersona ("AHHHH"++) unaPersona

puedeSubastar :: String -> Bool
puedeSubastar "Oferente singular" = True
puedeSubastar "Accionista" = True
puedeSubastar _ = False

subastar :: Persona -> (String, Int) -> Persona
subastar unaPersona (nombrePropiedad, precio)
    | puedeSubastar (tacticaDeJuego unaPersona) = (cambiarCantidadDeDinero (precio-) . agregarPropiedades ((nombrePropiedad, precio))) unaPersona
    | otherwise = unaPersona

--cobrarAlquileres

esBarata :: (String, Int) -> Bool
esBarata (_, precio) = precio < 150

dineroACobrarPorAlquileres :: [(String, Int)] -> Int
dineroACobrarPorAlquileres propiedades = length (filter esBarata propiedades) * 10 + length (filter (not.esBarata) propiedades) * 20 

cobrarAlquileres :: Persona -> Persona
cobrarAlquileres unaPersona = cambiarCantidadDeDinero ( + (dineroACobrarPorAlquileres (propiedadesCompradas unaPersona))) unaPersona

--pagarAAccionistas

pagarAAccionistas :: Persona -> Persona
pagarAAccionistas unaPersona 
                                | tacticaDeJuego unaPersona == "Accionista" = cambiarCantidadDeDinero ( + 200) unaPersona
                                | otherwise = cambiarCantidadDeDinero (100 - ) unaPersona
