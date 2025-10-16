{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO (hFlush, stdout)
import qualified Data.ByteString.Lazy as B
import Data.Aeson (encode, decode)
import System.Directory (doesFileExist, renameFile)
import Importacion (Venta(..), importarVentas)
import Procesamiento (menuProcesamiento)
import Analisis (menuAnalisis)

main :: IO ()
main = do
    putStrLn "=========================================="
    putStrLn "           ANALISIS DE VENTAS              "
    putStrLn "=========================================="

    ventasIniciales <- cargarVentas

    menuLoop ventasIniciales


menuLoop :: [Venta] -> IO ()
menuLoop ventas = do
    putStrLn "\n--- MENÚ PRINCIPAL ---"
    putStrLn "1. Importación de datos"
    putStrLn "2. Procesamiento de datos"
    putStrLn "3. Análisis de datos"
    putStrLn "4. Análisis temporal"
    putStrLn "5. Búsqueda de ventas"
    putStrLn "6. Estadísticas"
    putStrLn "7. Salir"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "\n--- Importación de datos ---"
            putStr "Ingrese el nombre del archivo JSON: "
            hFlush stdout
            ruta <- getLine

            -- Llamada a importarVentas
            nuevasVentas <- importarVentas ruta ventas
            imprimirVentas nuevasVentas
            menuLoop nuevasVentas

        "2" -> do
            putStrLn "\n--- Procesamiento de datos ---"
            nuevasVentas <- menuProcesamiento ventas
            imprimirVentas nuevasVentas
            menuLoop nuevasVentas

        "3" -> do
            putStrLn "\n--- Análisis de datos ---"
            menuAnalisis ventas
            menuLoop ventas

        "4" -> do
            putStrLn "\n--- Análisis temporal ---"
            -- analisisTemporal ventas
            menuLoop ventas

        "5" -> do
            putStrLn "\n--- Búsqueda de ventas ---"
            -- busquedaVentas ventas
            menuLoop ventas

        "6" -> do
            putStrLn "\n--- Estadísticas ---"
            -- estadisticasMenu ventas
            menuLoop ventas

        "7" -> do
            putStrLn "\nSaliendo del sistema..."
            putStrLn "Gracias por usar el sistema de análisis de ventas."
            guardarVentas ventas
            putStrLn "--- Datos guardados correctamente. ---"

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
            menuLoop ventas


-- imprime las ventas existenetes en memoria
imprimirVentas :: [Venta] -> IO ()
imprimirVentas ventas = do
  putStrLn "\n--- Ventas en memoria ---"
  if null ventas
    then putStrLn "No hay ventas cargadas."
    else mapM_ (\v -> putStrLn $
         "ID: " ++ show (venta_id v)
      ++ " | Fecha: " ++ fecha v
      ++ " | Producto ID: " ++ show (producto_id v)
      ++ " | Nombre: " ++ producto_nombre v
      ++ " | Categoría: " ++ categoria v
      ++ " | Cantidad: " ++ show (cantidad v)
      ++ " | Precio Unitario: " ++ show (precio_unitario v)
      ++ " | Total: " ++ show (total v)
      ) ventas
  putStrLn "------------------------\n"

-- guarda las ventas en un json para manejar persistencia
guardarVentas :: [Venta] -> IO ()
guardarVentas ventas = do
  let archivoTemp = "ventas_temp.json"
  B.writeFile archivoTemp (encode ventas)
  renameFile archivoTemp "ventas.json"
  putStrLn "Ventas guardadas en ventas.json"

-- carga las ventas desde el json
cargarVentas :: IO [Venta]
cargarVentas = do
  existe <- doesFileExist "ventas.json"
  if not existe
    then return []
    else do
      contenido <- B.readFile "ventas.json"
      let decoded = decode contenido :: Maybe [Venta]
      case decoded of
        Nothing -> do
          putStrLn "Error al leer ventas.json. Archivo corrupto o mal formado."
          return []
        Just ventas -> return ventas