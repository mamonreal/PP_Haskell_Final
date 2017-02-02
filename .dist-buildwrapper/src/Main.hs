module Main where

import Postfija
import OperarPostfija
import Pila
import System.IO

main::IO()
main = do
    exp <- Prelude.readFile "entrada.txt"
    let expresiones = lines exp
    if head expresiones == [] then
        return ()
    else
        escribirArchivo (tail expresiones) (head expresiones)
        
escribirArchivo:: [String]-> String-> IO()
escribirArchivo xs s 
    |xs == [] = appendFile "salida.txt" (s ++ " = "++ show(operar(infijaToPostfija (words s) [] PV)PV)++"\n")
    |otherwise = do
        appendFile "salida.txt" (s ++ " = "++ show (operar(infijaToPostfija (words s) [] PV)PV)++"\n")
        escribirArchivo (tail xs) (head xs)