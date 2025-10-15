{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Procesamiento where

import System.IO (hFlush, stdout)
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.Set as Set
import Data.List (foldl')
import System.Directory (renameFile, removeFile)
import qualified Data.Map.Strict as Map
import Data.List (maximumBy)
import Data.Ord (comparing)


data Venta = Venta
  { venta_id        :: Int
  , fecha           :: String
  , producto_id     :: Int
  , producto_nombre :: String
  , categoria       :: String
  , cantidad        :: Int
  , precio_unitario :: Double
  , total           :: Double
  } deriving (Show, Generic)

instance FromJSON Venta
instance ToJSON Venta where


menuProcesamiento :: IO ()
menuProcesamiento = do
    putStrLn "\n--- PROCESAMIENTO DE DATOS ---"
    putStrLn "1. Completar datos faltantes"
    putStrLn "2. Eliminar duplicados"
    putStrLn "6. Salir"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\nCompletar datos faltantes seleccionado"
            menuCompletarDatos
            menuProcesamiento 

        "2" -> do
            putStrLn "\nEliminar duplicados seleccionado"
            eliminarDuplicados "ventas.json"
            menuProcesamiento

        "6" -> do
            putStrLn "\n.."
            

        _   -> do
            putStrLn "\nOpción inválida. Intente de nuevo."
            menuProcesamiento 

menuCompletarDatos :: IO ()
menuCompletarDatos = do
    putStrLn "\n--- PROCESAMIENTO DE DATOS ---"
    putStrLn "1. Moda"
    putStrLn "2. Media"
    putStrLn "3. Mendiana"
    putStrLn "6. Salir"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            putStrLn "\nCompletar por la moda seleccionado"
 
            contenido <- B.readFile "ventas.json"
            let ventas = case decode contenido :: Maybe [Venta] of
                            Just v -> v
                            Nothing -> []

            let (modaCant, modaPrecio) = calcularModaCantidadPrecio ventas

            let (ventasMod, idsModificados) = modificarVentas modaCant modaPrecio ventas

            let archivoTemp = "temp.json"
            B.writeFile archivoTemp (encode ventasMod)
            removeFile "ventas.json"
            renameFile archivoTemp "ventas.json"

            if null idsModificados
            then putStrLn "No se modificaron registros."
            else putStrLn $ "Ventas modificadas. IDs: " ++ show idsModificados

            menuCompletarDatos
        "2" -> do
            putStrLn "\nCompletar por la media seleccionado"
            menuCompletarDatos

        "3" -> do
            putStrLn "\nCompletar por el mediana seleccionado"
            menuCompletarDatos

        "6" -> do
            putStrLn "\n.."

        _   -> do
            putStrLn "\nOpción inválida. Intente de nuevo."
            menuCompletarDatos 


-- Eliminr Duplicados
eliminarDuplicados :: FilePath -> IO [Int]
eliminarDuplicados ruta = do
    contenido <- B.readFile ruta
    let ventas = case decode contenido :: Maybe [Venta] of
                    Just v  -> v
                    Nothing -> []
    let (ventasUnicas, idsEliminados, _) = foldl' procesar ([], Set.empty, Set.empty) ventas
    let archivoTemp = "temp.json"
    B.writeFile archivoTemp (encode ventasUnicas)
    removeFile ruta
    renameFile archivoTemp ruta
    putStrLn "\nDuplicados eliminados con éxito."
    if null (Set.toList idsEliminados)
        then putStrLn "No se encontraron registros duplicados."
        else putStrLn $ "Registros eliminados: " ++ show (Set.toList idsEliminados)
    return (Set.toList idsEliminados)
  where
    procesar (acumVentas, elimIds, idsVistos) venta
      | venta_id venta `Set.member` idsVistos =
          (acumVentas, Set.insert (venta_id venta) elimIds, idsVistos)
      | otherwise =
          (acumVentas ++ [venta], elimIds, Set.insert (venta_id venta) idsVistos)


modificarVentas :: Int -> Double -> [Venta] -> ([Venta], [Int])
modificarVentas valorCantidad valorPrecio ventas =
    foldl modificar ([], []) ventas
  where
    modificar (acumVentas, ids) v
      | cantidad v == 0 || precio_unitario v == 0 =
          let nuevaVenta = v { cantidad = valorCantidad
                             , precio_unitario = valorPrecio
                             , total = fromIntegral valorCantidad * valorPrecio }
          in (acumVentas ++ [nuevaVenta], ids ++ [venta_id v])
      | otherwise = (acumVentas ++ [v], ids)


moda :: (Ord a) => [a] -> a
moda xs =
  let freqMap = Map.fromListWith (+) [(x,1) | x <- xs]
  in fst $ maximumBy (comparing snd) (Map.toList freqMap)

-- Calcula la moda
calcularModaCantidadPrecio :: [Venta] -> (Int, Double)
calcularModaCantidadPrecio ventas =
    let cantidadesValidas = [cantidad v | v <- ventas, cantidad v /= 0]
        preciosValidos    = [precio_unitario v | v <- ventas, precio_unitario v /= 0]
        modaCantidad = moda cantidadesValidas
        modaPrecio   = moda preciosValidos
    in (modaCantidad, modaPrecio)