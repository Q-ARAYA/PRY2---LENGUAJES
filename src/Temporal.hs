{-# LANGUAGE OverloadedStrings #-}

module Temporal where

import System.IO (hFlush, stdout)
import qualified Data.Map.Strict as Map
import Data.Time (defaultTimeLocale, parseTimeM, dayOfWeek, DayOfWeek(..))
import Data.Time.Calendar (Day)
import Data.List (sortOn)
import Data.Ord (Down(..))
import Importacion (Venta(..))
import Analisis (redondear2dec)

pausa :: IO ()
pausa = do
    putStrLn "\nPresione Enter para continuar..."
    _ <- getLine
    return ()

limpiarTerminal :: IO ()
limpiarTerminal = putStr "\ESC[2J\ESC[H"

menuTemporal :: [Venta] -> IO ()
menuTemporal ventas = do
    limpiarTerminal
    putStrLn "\n--- ANALISIS TEMPORAL ---"
    putStrLn "1. Mes con mayor venta y dia de la semana más activo"
    putStrLn "2. Calcular tasa de crecimiento o decrecimiento trimestral"
    putStrLn "3. Resumen de ventas por trimestre"
    putStrLn "7. Volver"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "\n--- Mes con mayor venta y dia de la semana más activo ---"
            mesMayorVenta ventas
            diaMasActivo ventas
            pausa
            menuTemporal ventas


        "2" -> do
            putStrLn "\n--- Calcular tasa de crecimiento o decrecimiento trimestral ---"
            tasaCrecimientoTrimestral ventas
            pausa
            menuTemporal ventas

        "3" -> do
            putStrLn "\n--- Resumen de ventas por trimestre ---"
            resumenVentasPorTrimestre ventas
            pausa
            menuTemporal ventas

        "7" -> do
            putStrLn "\n.."
            

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
            pausa
            menuTemporal ventas

-- Entrdas: lista de ventas
-- Salida: Imprime el mes con mayor venta
mesMayorVenta :: [Venta] -> IO ()
mesMayorVenta ventas = do
    -- Agrupar todas las ventas por MES (sin importar el año)
    let ventasPorMes = Map.toList $
            Map.fromListWith (+)
                [ (take 2 $ drop 5 (fecha v), total v) | v <- ventas ] 

    if null ventasPorMes
        then putStrLn "\nNo hay ventas registradas."
        else do
            let (mesMax, montoMax) = segundoMayor ventasPorMes
            putStrLn "\n--- Mes con mayor venta (entre todos los años) ---"
            putStrLn $ "    - Mes: " ++ nombreMes mesMax ++ " | Total: $" ++ show (redondear2dec montoMax)

nombreMes :: String -> String
nombreMes "01" = "Enero"
nombreMes "02" = "Febrero"
nombreMes "03" = "Marzo"
nombreMes "04" = "Abril"
nombreMes "05" = "Mayo"
nombreMes "06" = "Junio"
nombreMes "07" = "Julio"
nombreMes "08" = "Agosto"
nombreMes "09" = "Septiembre"
nombreMes "10" = "Octubre"
nombreMes "11" = "Noviembre"
nombreMes "12" = "Diciembre"
nombreMes _    = "Mes desconocido"


-- Entradas: lista de ventas
-- Salida: Imprime el dia de la semana más activo
diaMasActivo :: [Venta] -> IO ()
diaMasActivo ventas = do
    let dias = [ obtenerDiaSemana (fecha v) | v <- ventas ]
        conteo = Map.toList $ Map.fromListWith (+) [(d, 1 :: Int) | d <- dias]
    if null conteo
        then putStrLn "\nNo hay ventas registradas."
        else do
            let (diaMax, cantMax) = segundoMayor conteo
            putStrLn "\n--- Día de la semana más activo ---"
            putStrLn $ "    - Día: " ++ diaMax ++ " | Transacciones: " ++ show cantMax


-- Función auxiliar para obtener el máximo segundo valor de una tupla
-- Entradas: lista de tuplas
-- Salida: tupla con el segundo valor mas grande
segundoMayor :: (Ord b) => [(a, b)] -> (a, b)
segundoMayor = foldr1 (\x y -> if snd x >= snd y then x else y)

-- Convierte una fecha (YYYY-MM-DD) en el nombre del día de la semana
obtenerDiaSemana :: String -> String
obtenerDiaSemana f =
    case parseTimeM True defaultTimeLocale "%Y-%m-%d" f :: Maybe Day of
        Just day ->
            case dayOfWeek day of
                Monday    -> "Lunes"
                Tuesday   -> "Martes"
                Wednesday -> "Miércoles"
                Thursday  -> "Jueves"
                Friday    -> "Viernes"
                Saturday  -> "Sábado"
                Sunday    -> "Domingo"
        Nothing -> "Fecha inválida"

-- Función para calcular la tasa de crecimiento o decrecimiento trimestral
-- Entradas: lista de ventas
-- Salida: Imprime la tasa de crecimiento o decrecimiento trimestral
tasaCrecimientoTrimestral :: [Venta] -> IO ()
tasaCrecimientoTrimestral ventas = do
    putStrLn "\nIngrese el año (YYYY): "
    hFlush stdout
    anio <- getLine

    putStrLn "Ingrese el trimestre (1-4): "
    hFlush stdout
    trimestreStr <- getLine

    let trimestre = read trimestreStr :: Int
    if trimestre < 1 || trimestre > 4
        then putStrLn "Trimestre inválido."
        else do
            -- Filtrar ventas del año y trimestre seleccionado
            let ventasAnio = filter (\v -> take 4 (fecha v) == anio) ventas
                mesesTrimestre = case trimestre of
                    1 -> ["01","02","03"]
                    2 -> ["04","05","06"]
                    3 -> ["07","08","09"]
                    4 -> ["10","11","12"]
                    _ -> []

                totalTrimestre = sum [ total v | v <- ventasAnio, take 2 (drop 5 (fecha v)) `elem` mesesTrimestre ]

            -- Calcular ventas del trimestre anterior
            let (anioAnt, trimestreAnt) = if trimestre == 1
                                            then (show ((read anio)-1), 4)
                                            else (anio, trimestre-1)

                mesesTrimestreAnt = case trimestreAnt of
                    1 -> ["01","02","03"]
                    2 -> ["04","05","06"]
                    3 -> ["07","08","09"]
                    4 -> ["10","11","12"]
                    _ -> []

                ventasAnioAnt = filter (\v -> take 4 (fecha v) == anioAnt) ventas
                totalTrimestreAnt = sum [ total v | v <- ventasAnioAnt, take 2 (drop 5 (fecha v)) `elem` mesesTrimestreAnt ]

            -- Calcular tasa
            if totalTrimestreAnt == 0
                then putStrLn "No hay ventas en el trimestre anterior para calcular la tasa."
                else do
                    let tasa = ((totalTrimestre - totalTrimestreAnt) / totalTrimestreAnt) * 100
                    putStrLn $ "\nVentas Trimestre " ++ show trimestre ++ " de " ++ anio ++ ": $" ++ show (redondear2dec totalTrimestre)
                    putStrLn $ "Ventas Trimestre anterior: $" ++ show (redondear2dec totalTrimestreAnt)
                    putStrLn $ "Tasa de crecimiento/decrecimiento: " ++ show (redondear2dec tasa) ++ "%"

-- Generar un resumen de ventas por trimestre
-- Entradas: lista de ventas
-- Salida: imprime total de ventas por trimestre y año
resumenVentasPorTrimestre :: [Venta] -> IO ()
resumenVentasPorTrimestre ventas = do
    -- Crear lista de tuplas ((año, trimestre), total venta)
    let ventasTrimestre = [ ((take 4 (fecha v), trimestreDeMes (read (take 2 (drop 5 (fecha v))) :: Int)), total v)
                          | v <- ventas ]
        totales = Map.toList $ Map.fromListWith (+) ventasTrimestre
        ordenados = sortOn fst totales

    if null ordenados
        then putStrLn "\nNo hay ventas registradas."
        else do
            putStrLn "\n--- Resumen de ventas por trimestre ---"
            mapM_ (\((anio, tri), monto) ->
                putStrLn $ "    - Año: " ++ anio ++ " | Trimestre: " ++ show tri ++ " | Total ventas: $" ++ show (redondear2dec monto)
                ) ordenados

-- Función auxiliar devuelve el trimestre correspondiente al mes
trimestreDeMes :: Int -> Int
trimestreDeMes m
    | m >= 1 && m <= 3   = 1
    | m >= 4 && m <= 6   = 2
    | m >= 7 && m <= 9   = 3
    | m >= 10 && m <= 12 = 4
    | otherwise          = error "Mes inválido"
