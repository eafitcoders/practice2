Claro, aquí está el código con comentarios en español explicando cada línea:

```haskell
-- Importar las bibliotecas necesarias
import System.Directory
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Data.List
import System.IO
import Control.Exception (bracket)
import Control.Monad (when)
import Text.Read (readMaybe)
import Data.Maybe (mapMaybe)

-- Definir un nuevo tipo de datos `Vehiculo` con tres campos
data Vehiculo = Vehiculo {
    placa :: String,  -- una cadena que representa la placa del vehículo
    entrada :: UTCTime,  -- un `UTCTime` que representa la hora de entrada
    salida :: Maybe UTCTime  -- un `Maybe UTCTime` que representa la hora de salida, que puede no estar establecida si el vehículo aún está en el estacionamiento
} deriving (Show, Read)

-- Función para registrar la entrada de un vehículo al estacionamiento
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero  -- añadir el nuevo vehículo al frente de la lista

-- Función para registrar la salida de un vehículo del estacionamiento
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero  -- actualizar el campo `salida` del vehículo con el número de placa dado

-- Función para encontrar un vehículo por su número de placa en el estacionamiento
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v) parqueadero  -- devolver el vehículo con el número de placa dado, si existe en la lista

-- Función para calcular el tiempo que un vehículo permaneció en el estacionamiento
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    diffUTCTime tiempoActual (entrada vehiculo)  -- devolver la diferencia entre la hora actual y la hora en que el vehículo entró al estacionamiento

-- Función para guardar la información del estacionamiento en un archivo
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = bracket
    (openFile "parqueadero.txt" WriteMode)
    hClose
    (\h -> do
        hPutStr h (unlines $ map mostrarVehiculo parqueadero)  -- escribir los vehículos en el archivo
        putStrLn "Parqueadero guardado en el archivo parqueadero.txt.")  -- imprimir un mensaje de confirmación

-- Función para leer el contenido de un archivo si existe
readFileIfExists :: FilePath -> IO String
readFileIfExists path = do
    fileExists <- doesFileExist path
    if fileExists
        then do
            contenido <- readFile path  -- leer el archivo si existe
            return contenido
        else return ""  -- devolver una cadena vacía si el archivo no existe

-- Función para cargar los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    contenido <- readFileIfExists "parqueadero.txt"
    let lineas = lines contenido
    return (mapMaybe leerVehiculo lineas)  -- analizar cada línea del archivo en un `Vehiculo`
    where
        leerVehiculo linea = case words linea of
            [placaVehiculo, entradaVehiculo, "Nothing"] -> Just $ Vehiculo placaVehiculo (read entradaVehiculo) Nothing
            [placaVehiculo, entradaVehiculo, salidaVehiculo] -> Just $ Vehiculo placaVehiculo (read entradaVehiculo) (Just $ read salidaVehiculo)
            _ -> Nothing

-- Función para mostrar la información de un vehículo como una cadena
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    let entradaLocal = utcToBogotaTime (entrada vehiculo)
        salidaLocal = case salida vehiculo of
                        Nothing -> "Nothing"
                        Just s -> show $ utcToBogotaTime s
    in placa vehiculo ++ "," ++ show entradaLocal ++ "," ++ salidaLocal

-- Función para listar los vehículos
enlistarVehiculos :: [Vehiculo] -> IO ()
enlistarVehiculos parqueadero = do
    putStrLn "Vehículos en el parqueadero:"
    mapM_ (\v -> putStrLn $ mostrarVehiculo v) parqueadero  -- imprimir cada vehículo en la consola

-- Función principal del programa
main :: IO ()
main = do
    parqueadero <- cargarParqueadero  -- cargar los vehículos desde el archivo
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"
    ciclo parqueadero  -- entrar en el ciclo principal del programa

-- Función para convertir la hora UTC a la hora local en Bogotá
utcToBogotaTime :: UTCTime -> LocalTime
utcToBogotaTime utcTime = utcToLocalTime (TimeZone (-300) False "COT") utcTime

-- Ciclo principal del programa
ciclo :: [Vehiculo] -> IO ()
ciclo parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Enlistar vehiculos"
    putStrLn "5. Salir"
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero  -- registrar la entrada del vehículo
            guardarParqueadero parqueaderoActualizado  -- guardar la lista actualizada de vehículos
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
            ciclo parqueaderoActualizado  -- recursión con la lista actualizada de vehículos
        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarSalida placaVehiculo tiempoActual parqueadero  -- registrar la salida del vehículo
            guardarParqueadero parqueaderoActualizado  -- guardar la lista actualizada de vehículos
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
            ciclo parqueaderoActualizado  -- recursión con la lista actualizada de vehículos
        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of  -- encontrar el vehículo con el número de placa dado
                Just vehiculo -> do
                    let tiempoTotal = tiempoEnParqueadero vehiculo (entrada vehiculo)
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            ciclo parqueadero  -- recursión con la misma lista de vehículos
        "4" -> do
            enlistarVehiculos parqueadero  -- listar los vehículos
            ciclo parqueadero  -- recursión con la misma lista de vehículos
        "5" -> putStrLn "¡Hasta luego!"  -- imprimir un mensaje de despedida y terminar
        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            ciclo parqueadero  -- recursión con la misma lista de vehículos
```