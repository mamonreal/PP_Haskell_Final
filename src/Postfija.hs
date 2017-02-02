module Postfija where

import Pila

--Primer parametro es la expresion infija
--El segundo es la expresio en forma postfija
--El tercero, la pila con los signos de operacion
infijaToPostfija:: [String]-> [String]-> Pila String-> [String]
infijaToPostfija [] num PV = num
infijaToPostfija [] num (P c r) = desap num (P c r)
infijaToPostfija (e:exp) num PV
    |e=="^"||e=="*"||e=="/"||e=="+"||e=="-" = infijaToPostfija exp num (P e [])
-------------------------------------DEFENSE------------------------------------------   
--Separa la expresion de dentro del valor asoluto de las de fuera y se pasa a notacion
--postfija por separado
    | e=="|" = infijaToPostfija (desdeValorAbsoluto exp) (num++["|"]++(infijaToPostfija(hastaValorAbsoluto exp) [] PV)++["|"]) PV
----------------------------------------------------------------------------------------
    |otherwise = infijaToPostfija exp (num++[e]) PV
    
infijaToPostfija (e:exp) num (P c r)
------------------------------------DEFENSE------------------------------------------   
--Separa la expresion de dentro del valor asoluto de las de fuera y se pasa a notacion
--postfija por separado
    | e=="|" = infijaToPostfija (desdeValorAbsoluto exp) (num++["|"]++(infijaToPostfija(hastaValorAbsoluto exp) [] PV)++["|"]) (P c r)
----------------------------------------------------------------------------------------
    |e=="(" = infijaToPostfija exp num (apilar (P c r) e)
    |e==")" = desapilarHastaParentesis exp num (P c r)
    |e=="^" = 
        if c=="^" then
            apilarInferior (e:exp) num (P c r)
        else
            infijaToPostfija exp num (apilar (P c r) e)
    |(e=="*" || e=="/")&&(c=="+"||c=="-"||c=="(") =
        infijaToPostfija exp num (apilar (P c r) e)
    |(e=="*" || e=="/")&&(c=="*" || c=="/" || c=="^") =
        apilarInferior (e:exp) num (P c r)
    |e=="+" || e=="-" = apilarInferior (e:exp) num (P c r)
    |otherwise = infijaToPostfija exp (num++[e]) (P c r)
    
--Cuando se a analizado toda la expresión de entrada, pero siguen quedadndo
--signos en la pila de signos estos se vacían en la expresión de salida
desap:: [String]-> Pila String-> [String]
desap exp PV = exp
desap exp (P c r) = desap (exp++[devolverCabeza (P c r)]) (desapilar (P c r))

--Cuando en la expresión de entrada se encuentra un cerrar parentesis se 
--desapilan los signos de la pila hasta encontrar un signo de abrir parentesis
desapilarHastaParentesis:: [String]-> [String]-> Pila String-> [String]
desapilarHastaParentesis exp num (P c r)
    |c == "(" = infijaToPostfija exp num (desapilar (P c r))
    |otherwise = desapilarHastaParentesis exp (num++[devolverCabeza (P c r)]) (desapilar (P c r))
    
--Cuando se quiere apilar un signo de operacion, pero en la cabeza de la cola hay 
--uno que tiene prioridad igual o menor, se desapilan los signos hasta que
--se pueda apilar
apilarInferior:: [String]-> [String]-> Pila String-> [String]
apilarInferior (e:exp) num PV = infijaToPostfija exp num (P e [])
apilarInferior (e:exp) num (P c r)
    |e=="^" && c=="^" = apilarInferior (e:exp) (num++[devolverCabeza(P c r)]) (desapilar (P c r))
    |(e=="*" || e=="/")&&(c=="*" || c=="/" || c=="^") =
        apilarInferior (e:exp) (num++[devolverCabeza(P c r)]) (desapilar (P c r))
    |(e=="+" || e=="-")&&(c=="+" || c=="-" || c=="*" || c=="/" || c=="^") =
        apilarInferior (e:exp) (num++[devolverCabeza(P c r)]) (desapilar (P c r))
    |otherwise = infijaToPostfija exp num (apilar (P c r) e)
    
    
    
-----------------------------------DEFENSA DE PRACTICA------------------------------------
hastaValorAbsoluto:: [String]-> [String]
hastaValorAbsoluto (s:ss)
    | s=="|" = []
    | otherwise = [s]++(hastaValorAbsoluto ss)

desdeValorAbsoluto:: [String]-> [String]
desdeValorAbsoluto (s:ss)
    | s=="|" = ss
    | otherwise = desdeValorAbsoluto ss
    