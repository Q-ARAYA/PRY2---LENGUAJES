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
import Data.List (maximumBy, sort)
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
            putStrLn "\n--- Completar datos faltantes seleccionado ---"
            menuCompletarDatos
            menuProcesamiento 

        "2" -> do
            putStrLn "\n--- Eliminar duplicados seleccionado ---"
            eliminarDuplicados "ventas.json"
            menuProcesamiento

        "6" -> do
            putStrLn "\n.."
            

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
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
            putStrLn "\n--- Completar por la moda seleccionado ---"
 
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
            putStrLn "\n--- Completar por la media seleccionado ---"
            
            contenido <- B.readFile "ventas.json"
            let ventas = case decode contenido :: Maybe [Venta] of
                            Just v -> v
                            Nothing -> []

            let (mediaCant, mediaPrecio) = calcularMediaCantidadPrecio ventas

            let (ventasMod, idsModificados) = modificarVentas mediaCant mediaPrecio ventas

            let archivoTemp = "temp.json"
            B.writeFile archivoTemp (encode ventasMod)
            removeFile "ventas.json"
            renameFile archivoTemp "ventas.json"

            if null idsModificados
            then putStrLn "No se modificaron registros."
            else putStrLn $ "Ventas modificadas. IDs: " ++ show idsModificados


        "3" -> do
            putStrLn "\n--- Completar por el mediana seleccionado ---"

            contenido <- B.readFile "ventas.json"
            let ventas = case decode contenido :: Maybe [Venta] of
                            Just v -> v
                            Nothing -> []

            let (medianaCant, medianaPrecio) = calcularMedianaCantidadPrecio ventas

            let (ventasMod, idsModificados) = modificarVentas medianaCant medianaPrecio ventas

            let archivoTemp = "temp.json"
            B.writeFile archivoTemp (encode ventasMod)
            removeFile "ventas.json"
            renameFile archivoTemp "ventas.json"

            if null idsModificados
            then putStrLn "No se modificaron registros."
            else putStrLn $ "Ventas modificadas. IDs: " ++ show idsModificados
            menuCompletarDatos

        "6" -> do
            putStrLn "\n.."

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
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

calcularMediaCantidadPrecio :: [Venta] -> (Int, Double)
calcularMediaCantidadPrecio ventas =
    let cantidadesValidas = [cantidad v | v <- ventas, cantidad v /= 0]
        preciosValidos    = [precio_unitario v | v <- ventas, precio_unitario v /= 0]

        mediaCantidad = if null cantidadesValidas
                        then 0
                        else round $ fromIntegral (sum cantidadesValidas) / fromIntegral (length cantidadesValidas)

        mediaPrecio   = if null preciosValidos
                        then 0
                        else sum preciosValidos / fromIntegral (length preciosValidos)
    in (mediaCantidad, mediaPrecio)


calcularMedianaCantidadPrecio :: [Venta] -> (Int, Double)
calcularMedianaCantidadPrecio ventas =
    let cantidadesValidas = sort [cantidad v | v <- ventas, cantidad v /= 0]
        preciosValidos    = sort [precio_unitario v | v <- ventas, precio_unitario v /= 0]

        mediana xs
          | null xs   = 0
          | odd n     = xs !! (n `div` 2)
          | otherwise = let mid = n `div` 2
                        in round $ (fromIntegral (xs !! (mid - 1) + xs !! mid)) / 2
          where n = length xs

        medianaCantidad = mediana cantidadesValidas
        medianaPrecio   = if null preciosValidos
                          then 0
                          else let n = length preciosValidos
                               in if odd n
                                  then preciosValidos !! (n `div` 2)
                                  else (preciosValidos !! (n `div` 2 - 1) + preciosValidos !! (n `div` 2)) / 2
    in (medianaCantidad, medianaPrecio)