import Data.Time.Clock
import Data.List
import System.IO
import Control.Exception
import Control.Concurrent (threadDelay)
import Data.Maybe (isNothing, fromMaybe)
import Data.Time.Format (defaultTimeLocale, parseTimeM, formatTime)
import Data.Maybe (isNothing, fromMaybe, mapMaybe)


-- Definición del tipo de datos para representar la información de un estudiante
data Estudiante = Estudiante {
    idEstudiante :: String,
    entrada :: UTCTime,
    salida :: Maybe UTCTime
} deriving (Show, Read)

-- Función para registrar la entrada de un estudiante
registrarEntrada :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarEntrada id tiempo estudiantes =
    Estudiante id tiempo Nothing : estudiantes

-- Función para registrar la salida de un estudiante
registrarSalida :: String -> UTCTime -> [Estudiante] -> [Estudiante]
registrarSalida id tiempo estudiantes =
    map (\e -> if id == idEstudiante e then e { salida = Just tiempo } else e) estudiantes

-- Función para buscar un estudiante por su ID
buscarEstudiante :: String -> [Estudiante] -> Maybe Estudiante
buscarEstudiante id estudiantes =
    find (\e -> id == idEstudiante e && isNothing (salida e)) estudiantes

-- Función para calcular el tiempo que un estudiante permaneció en la universidad
tiempoEnUniversidad :: Estudiante -> UTCTime -> NominalDiffTime
tiempoEnUniversidad estudiante tiempoActual =
    case salida estudiante of
        Just tiempoSalida -> diffUTCTime tiempoSalida (entrada estudiante)
        Nothing           -> diffUTCTime tiempoActual (entrada estudiante)

-- Función para guardar la información en un archivo sin sobrescribir los datos anteriores
guardarRegistro :: [Estudiante] -> IO ()
guardarRegistro estudiantes = do
    resultado <- try (writeFile "universidad.txt" (unlines (map mostrarEstudiante estudiantes))) :: IO (Either IOException ())
    case resultado of
        Left ex -> putStrLn $ "Error guardando los registros: " ++ show ex
        Right _ -> putStrLn "Registros guardados en universidad.txt."

-- Función para cargar los datos desde un archivo
cargarRegistro :: IO [Estudiante]
cargarRegistro = do
    resultado <- try (readFile "universidad.txt") :: IO (Either IOException String)
    case resultado of
        Left _ -> return []
        Right contenido -> return (mapMaybe leerEstudiante (lines contenido))
  where
    leerEstudiante :: String -> Maybe Estudiante
    leerEstudiante linea =
        case split ',' linea of
            [idStr, entradaStr, salidaStr] ->
                case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" entradaStr of
                    Just entradaTime ->
                        let salidaTime = if salidaStr == "N/A" then Nothing else parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S" salidaStr
                        in Just (Estudiante idStr entradaTime salidaTime)
                    _ -> Nothing
            _ -> Nothing

-- Función auxiliar para dividir un string por un delimitador
split :: Char -> String -> [String]
split _ "" = []
split delim str =
    let (primera, resto) = break (== delim) str
    in primera : if null resto then [] else split delim (tail resto)

-- Función para mostrar información de un estudiante en formato CSV
mostrarEstudiante :: Estudiante -> String
mostrarEstudiante e =
    idEstudiante e ++ "," ++ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" (entrada e) ++ "," ++ fromMaybe "N/A" (fmap (formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S") (salida e))

-- Ciclo principal del programa
cicloPrincipal :: [Estudiante] -> IO ()
cicloPrincipal estudiantes = do
    putStrLn "\nSeleccione una opción:"
    putStrLn "1. Registrar entrada de estudiante"
    putStrLn "2. Registrar salida de estudiante"
    putStrLn "3. Buscar estudiante por ID"
    putStrLn "4. Listar estudiantes registrados"
    putStrLn "5. Salir"

    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "Ingrese el ID del estudiante:"
            id <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarEntrada id tiempoActual estudiantes
            putStrLn $ "Estudiante con ID " ++ id ++ " registrado."
            guardarRegistro estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "2" -> do
            putStrLn "Ingrese el ID del estudiante a registrar salida:"
            id <- getLine
            tiempoActual <- getCurrentTime
            let estudiantesActualizados = registrarSalida id tiempoActual estudiantes
            putStrLn $ "Salida registrada para estudiante con ID " ++ id ++ "."
            guardarRegistro estudiantesActualizados
            cicloPrincipal estudiantesActualizados

        "3" -> do
            putStrLn "Ingrese el ID del estudiante a buscar:"
            id <- getLine
            case buscarEstudiante id estudiantes of
                Just estudiante -> do
                    tiempoActual <- getCurrentTime
                    let tiempoTotal = tiempoEnUniversidad estudiante tiempoActual
                    putStrLn $ "El estudiante con ID " ++ id ++ " está en la universidad."
                    putStrLn $ "Tiempo en la universidad: " ++ show tiempoTotal ++ " segundos."
                Nothing -> putStrLn "Estudiante no encontrado."
            cicloPrincipal estudiantes

        "4" -> do
            putStrLn "Listado de estudiantes registrados:"
            mapM_ (putStrLn . mostrarEstudiante) estudiantes
            cicloPrincipal estudiantes

        "5" -> putStrLn "¡Hasta luego!"
        _   -> do
            putStrLn "Opción no válida. Por favor, intente nuevamente."
            cicloPrincipal estudiantes

-- Función principal del programa
main :: IO ()
main = do
    estudiantes <- cargarRegistro
    putStrLn "¡Bienvenido al Sistema de Gestión de Estudiantes!"
    cicloPrincipal estudiantes
