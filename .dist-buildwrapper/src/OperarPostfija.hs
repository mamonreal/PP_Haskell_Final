module OperarPostfija where

import Pila
import Postfija (desdeValorAbsoluto, hastaValorAbsoluto)

operar:: [String]-> Pila Int-> Int
operar [] (P c []) = c
operar (x:xs) PV
    |x=="|" = operar (desdeValorAbsoluto xs) (apilar PV (valorAbsoluto(operar(hastaValorAbsoluto xs) PV)))
    |otherwise = operar xs (P (read x:: Int) [])
operar (x:xs) (P c []) = operar xs (P (read x:: Int) [c])
operar (x:xs) (P c r)
    |x=="^"||x=="/"||x=="*"||x=="+"||x=="-" =
        operar xs (apilar (desapilarOperandos(P c r)) (operacion x (devolverOperandos(P c r))))
    |x=="|" = operar (desdeValorAbsoluto xs) (apilar (P c r) (valorAbsoluto(operar(hastaValorAbsoluto xs) PV)))
    |otherwise = operar xs (apilar (P c r) (read x:: Int))
    
operacion:: String-> (Int, Int)-> Int
operacion x (a, b)
    |x=="^" = b^a
    |x=="*" = b*a
    |x=="/" = b`div`a
    |x=="+" = b+a
    |x=="-" = b-a
    
valorAbsoluto:: Int-> Int
valorAbsoluto x = if x<0 then x*(-1) else x