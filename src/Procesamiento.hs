module Procesamiento where

import System.IO (hFlush, stdout)
import Data.List (foldl', sort, maximumBy)
import Data.Ord (comparing)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Importacion (Venta(..))


menuProcesamiento :: [Venta] -> IO [Venta]
menuProcesamiento ventas = do
    putStrLn "\n--- PROCESAMIENTO DE DATOS ---"
    putStrLn "1. Completar datos faltantes"
    putStrLn "2. Eliminar duplicados"
    putStrLn "7. Volver"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            ventasMod <- menuCompletarDatos ventas
            menuProcesamiento ventasMod

        "2" -> do
            ventasSinDup <- eliminarDuplicados ventas
            menuProcesamiento ventasSinDup

        "7" -> do
            putStrLn "\n.."
            return ventas

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
            menuProcesamiento ventas


menuCompletarDatos :: [Venta] -> IO [Venta]
menuCompletarDatos ventas = do
    putStrLn "\n--- Completar datos faltantes ---"
    putStrLn "1. Moda"
    putStrLn "2. Media"
    putStrLn "3. Mediana"
    putStrLn "7. Volver"
    putStr   "Seleccione una opción: "
    hFlush stdout
    opcion <- getLine
    case opcion of
        "1" -> do
            let (modaCant, modaPrecio) = calcularModaCantidadPrecio ventas
            let (ventasMod, idsMod) = modificarVentas modaCant modaPrecio ventas
            reportarModificaciones idsMod
            return ventasMod

        "2" -> do
            let (mediaCant, mediaPrecio) = calcularMediaCantidadPrecio ventas
            let (ventasMod, idsMod) = modificarVentas mediaCant mediaPrecio ventas
            reportarModificaciones idsMod
            return ventasMod

        "3" -> do
            let (medianaCant, medianaPrecio) = calcularMedianaCantidadPrecio ventas
            let (ventasMod, idsMod) = modificarVentas medianaCant medianaPrecio ventas
            reportarModificaciones idsMod
            return ventasMod

        "7" -> do
            putStrLn "\n.."
            return ventas

        _   -> do
            putStrLn "\nXXX Opción inválida. Intente de nuevo. XXX"
            menuCompletarDatos ventas

-- Funcion para eliminar duplicados
-- Entradas: lista de ventas
-- Salida: nueva lista de ventas sin duplicados
-- Restricciones: lo duplicados se identifican por venta_id
eliminarDuplicados :: [Venta] -> IO [Venta]
eliminarDuplicados ventas = do
    let (ventasUnicas, idsEliminados, _) = foldl' procesar ([], Set.empty, Set.empty) ventas
    putStrLn "\nDuplicados eliminados con éxito."
    if null (Set.toList idsEliminados)
        then putStrLn "No se encontraron registros duplicados."
        else putStrLn $ "Registros eliminados: " ++ show (Set.toList idsEliminados)
    return ventasUnicas
  where
    procesar (acumVentas, elimIds, idsVistos) venta
      | venta_id venta `Set.member` idsVistos =
          (acumVentas, Set.insert (venta_id venta) elimIds, idsVistos)
      | otherwise =
          (acumVentas ++ [venta], elimIds, Set.insert (venta_id venta) idsVistos)

-- Funcion modificar ventas con datos faltantes
-- Entradas: valor para cantidad, valor para precio_unitario, lista de ventas
-- Salida: nueva lista de ventas con datos completados, lista de ids modificados
modificarVentas :: Int -> Double -> [Venta] -> ([Venta], [Int])
modificarVentas valorCantidad valorPrecio ventas =
    foldl modificar ([], []) ventas
  where
    modificar (acum, ids) v
      | cantidad v == 0 || precio_unitario v == 0 =
          let nueva = v { cantidad = valorCantidad
                        , precio_unitario = valorPrecio
                        , total = fromIntegral valorCantidad * valorPrecio }
          in (acum ++ [nueva], ids ++ [venta_id v])
      | otherwise = (acum ++ [v], ids)

-- reporta cuales fueron las ventas modificadas
reportarModificaciones :: [Int] -> IO ()
reportarModificaciones ids
  | null ids = putStrLn "No se modificaron registros."
  | otherwise = putStrLn $ "Ventas modificadas. IDs: " ++ show ids

-- Funcion calcular de moda
-- Entradas: lista de ventas
-- Salida: tupla con (moda cantidad, moda precio_unitario)
-- Restricciones: se ignoran ceros (es decir las ventas a las cuales se va a cambiar el valor después)
calcularModaCantidadPrecio :: [Venta] -> (Int, Double)
calcularModaCantidadPrecio ventas = 
    let moda xs = fst $ maximumBy (comparing snd) $ Map.toList $ Map.fromListWith (+) [(x,1) | x <- xs]
        cantidades = [cantidad v | v <- ventas, cantidad v /= 0]
        precios    = [precio_unitario v | v <- ventas, precio_unitario v /= 0]
    in (moda cantidades, moda precios)


-- Funcion calcular de media (promedio)
-- Entradas: lista de ventas
-- Salida: tupla con (promedio cantidad, promedio precio_unitario)
-- Restricciones: se ignoran ceros (es decir las ventas a las cuales se va a cambiar el valor después)
calcularMediaCantidadPrecio :: [Venta] -> (Int, Double)
calcularMediaCantidadPrecio ventas =
    let cant = [cantidad v | v <- ventas, cantidad v /= 0]
        prec = [precio_unitario v | v <- ventas, precio_unitario v /= 0]
        mediaC = if null cant then 0 else round $ fromIntegral (sum cant) / fromIntegral (length cant)
        mediaP = if null prec then 0 else sum prec / fromIntegral (length prec)
    in (mediaC, mediaP)

-- Funcion calcular de mediana
-- Entradas: lista de ventas
-- Salida: tupla con (mediana cantidad, mediana precio_unitario)
-- Restricciones: se ignoran ceros (es decir las ventas a las cuales se va a cambiar el valor después)
calcularMedianaCantidadPrecio :: [Venta] -> (Int, Double)
calcularMedianaCantidadPrecio ventas =
    let cant = sort [cantidad v | v <- ventas, cantidad v /= 0]
        prec = sort [precio_unitario v | v <- ventas, precio_unitario v /= 0]
        mediana xs
          | null xs   = 0
          | odd n     = xs !! (n `div` 2)
          | otherwise = let mid = n `div` 2
                        in round $ (fromIntegral (xs !! (mid-1) + xs !! mid)) / 2
          where n = length xs
        medC = mediana cant
        medP = if null prec
               then 0
               else let n = length prec
                    in if odd n then prec !! (n `div` 2)
                       else (prec !! (n `div` 2 -1) + prec !! (n `div` 2)) / 2
    in (medC, medP)
