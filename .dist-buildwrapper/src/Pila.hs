module Pila where

data Pila a = PV | P a [a]

apilar:: Pila a-> a-> Pila a
apilar PV a = P a []
apilar (P cabeza l) x = P x (cabeza:l)

desapilar:: Pila a-> Pila a
desapilar (P c []) = PV
desapilar (P _ (x:xs)) = P x xs

desapilarOperandos:: Pila a-> Pila a
desapilarOperandos (P c r) = desapilar(desapilar (P c r))

devolverCabeza:: Pila a-> a
devolverCabeza (P c _) = c

devolverOperandos:: Pila a-> (a, a)
devolverOperandos (P c r) = (c, devolverCabeza(desapilar(P c r)))