import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (mapMaybe)

-- Definición del tipo de datos para representar la información de un vehículo
data Vehiculo = Vehiculo {
    placa :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime  -- Usamos Maybe para representar que el vehículo aún está en el parqueadero o ya salió
} deriving (Show, Read)

-- Función para registrar la entrada de un vehículo al parqueadero
registrarEntrada :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarEntrada placaVehiculo tiempo parqueadero =
    Vehiculo placaVehiculo tiempo Nothing : parqueadero

-- Función para registrar la salida de un vehículo del parqueadero
registrarSalida :: String -> UTCTime -> [Vehiculo] -> [Vehiculo]
registrarSalida placaVehiculo tiempo parqueadero =
    map (\v -> if placaVehiculo == placa v then v { salida = Just tiempo } else v) parqueadero

-- Función para buscar un vehículo por su placa en el parqueadero
buscarVehiculo :: String -> [Vehiculo] -> Maybe Vehiculo
buscarVehiculo placaVehiculo parqueadero =
    find (\v -> placaVehiculo == placa v && isNothing (salida v)) parqueadero
    where
        isNothing Nothing = True
        isNothing _       = False

-- Función para calcular el tiempo que un vehículo permaneció en el parqueadero
tiempoEnParqueadero :: Vehiculo -> UTCTime -> NominalDiffTime
tiempoEnParqueadero vehiculo tiempoActual =
    case salida vehiculo of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada vehiculo)
        Nothing           -> diffUTCTime tiempoActual (entrada vehiculo)

-- Función para guardar la información de los vehículos en un archivo de texto
guardarParqueadero :: [Vehiculo] -> IO ()
guardarParqueadero parqueadero = do
    resultado <- reintentar 5 (writeFile "parqueadero.txt" (unlines (map mostrarVehiculo parqueadero)))
    case resultado of
        Left ex -> putStrLn $ "Error guardando el parqueadero: " ++ show ex
        Right _ -> putStrLn "Parqueadero guardado en el archivo parqueadero.txt."

-- Función para reintentar una operación en caso de error
reintentar :: Int -> IO a -> IO (Either IOException a)
reintentar 0 accion = catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
reintentar n accion = do
    resultado <- catch (accion >>= return . Right) (\(ex :: IOException) -> return (Left ex))
    case resultado of
        Left ex -> do
            threadDelay 1000000  -- Esperar 1 segundo antes de reintentar
            reintentar (n - 1) accion
        Right val -> return (Right val)

-- Función para cargar la información de los vehículos desde un archivo de texto
cargarParqueadero :: IO [Vehiculo]
cargarParqueadero = do
    resultado <- try (readFile "parqueadero.txt") :: IO (Either IOException String)
    case resultado of
        Left ex -> do
            putStrLn $ "Error cargando el parqueadero: " ++ show ex
            return []
        Right contenido -> do
            let lineas = lines contenido
            return (map leerVehiculo lineas)
    where
        leerVehiculo linea = read linea :: Vehiculo

-- Función para mostrar la información de un vehículo como cadena de texto
mostrarVehiculo :: Vehiculo -> String
mostrarVehiculo vehiculo =
    placa vehiculo ++ "," ++ show (entrada vehiculo) ++ "," ++ show (salida vehiculo)

-- Función para cargar la información de los vehículos desde un archivo de texto
leerParqueadero :: IO [Vehiculo]
leerParqueadero = do
    contenido <- readFile "parqueadero.txt"
    let lineas = lines contenido
    return (mapMaybe parsearVehiculo lineas)
    where
        parsearVehiculo :: String -> Maybe Vehiculo
        parsearVehiculo linea = case words linea of
            [placa, entrada, salida] -> Just $ Vehiculo placa (read entrada) (readMaybeSalida salida)
            _ -> Nothing

        readMaybeSalida :: String -> Maybe UTCTime
        readMaybeSalida "Nothing" = Nothing
        readMaybeSalida salidaStr = Just (read salidaStr)




-- Función para el ciclo principal del programa
cicloPrincipal :: [Vehiculo] -> IO ()
cicloPrincipal parqueadero = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de vehículo"
    putStrLn "2. Registrar salida de vehículo"
    putStrLn "3. Buscar vehículo por placa"
    putStrLn "4. Listar los vehiculos del parqueadero"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese la placa del vehículo:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarEntrada placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " ingresado al parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "2" -> do
            putStrLn "Ingrese la placa del vehículo a salir:"
            placaVehiculo <- getLine
            tiempoActual <- getCurrentTime
            let parqueaderoActualizado = registrarSalida placaVehiculo tiempoActual parqueadero
            putStrLn $ "Vehículo con placa " ++ placaVehiculo ++ " salido del parqueadero."
            guardarParqueadero parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado

        "3" -> do
            putStrLn "Ingrese la placa del vehículo a buscar:"
            placaVehiculo <- getLine
            case buscarVehiculo placaVehiculo parqueadero of
                Just vehiculo -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnParqueadero vehiculo tiempoActual
                    putStrLn $ "El vehículo con placa " ++ placaVehiculo ++ " se encuentra en el parqueadero."
                    putStrLn $ "Tiempo en parqueadero: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Vehículo no encontrado en el parqueadero."
            cicloPrincipal parqueadero
        "4" -> do
            putStrLn "Mostrando Lista de carros dentro del parqueadero"
            -- Leer el parqueadero actualizado
            parqueaderoActualizado <- leerParqueadero
            mapM_ (\v -> putStrLn $ "Placa: " ++ placa v ++ ", Entrada: " ++ show (entrada v) ++ ", Salida: " ++ show (salida v)) parqueaderoActualizado
            cicloPrincipal parqueaderoActualizado  -- Mantenemos el parqueadero actualizado




        "5" -> putStrLn "¡Hasta luego!"

        _ -> do
            putStrLn "Opción no válida. Por favor, seleccione una opción válida."
            cicloPrincipal parqueadero

-- Función principal del programa
main :: IO ()
main = do
    -- Cargar el parqueadero desde el archivo de texto
    parqueadero <- cargarParqueadero
    putStrLn "¡Bienvenido al Sistema de Gestión de Parqueadero!"

    -- Ciclo principal del programa
    cicloPrincipal parqueadero

