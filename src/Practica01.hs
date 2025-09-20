module Practica01 where

--FUNCIONES
--función auxiliar usadas:
-- 'mod' calcula el residuo de una division entera
-- 'mod' se expresa de la forma: a `mod` b donde b divide a


--Valor absoluto de un entero
--Devuelve siempre el numero en positivo, con el cero devolviendo cero
valorAbs :: Int -> Int
valorAbs 0 = 0
valorAbs n = if 0 < n  then n else n* (-1)

--Es divisor
--Comprueba que el n ingresado divida a la m tal que su residuo sea cero
-- se usa 'mod' usar el residuo de m entre n
-- se iguala el residuo ('mod') a cero para comprobar que es divisor
esDivisor :: Int -> Int -> Bool
esDivisor n m = m `mod` n == 0

--Ecuacion cuadratica
--Resuelve la ecuacion cuadratica de la forma ax^2 + bx + c
cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c v = a*(v)^2 + b*(v) + c

--Suma de fracciones
--Suma dos fracciones escritas como (a,b) y (c,d), tal que a y c son los numeradores y b y d los denominadores
--Si los denominadores son iguales se suman los numeradores directamente
--Si los denominadores son distintos se encuentra el minimo comun multiplo y se suman los numeradores usando la multiplicacion cruzada
sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (a,b) (c,d) = if b==d then ((a+c,b)) else (((a*d)+(c*b),b*d)) 

--Comparador
--Comparamos n y m tal que, si son iguales se devuelve 0, si n es mayor que m se devuelve 1 y si m es mayor que n se devuelve -1
comparador :: Float -> Float -> Int
comparador n m = if n == m then 0 else (if n>m then 1 else -1) 

--Punto medio
--Se calcula el punto medio de dos puntos (x,y) y (w,z)
puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x,y) (w,z) = ((x+w)/2,(y+z)/2)


--RELACIONES
-- FUNCIONES AUXILIARES PARA LA SECCION 3 DE LA PRACTICA

-- Determina si el primer número es menor que el segundo
menorQue :: Int -> Int -> Bool
menorQue x y = x < y

-- Suma dos números de cualquier tipo numérico
sumaI :: (Num a) => a -> a -> a
sumaI x y = x + y

-- Determina si un número es múltiplo de otro (resto de división es 0)
esMultiplo :: Int -> Int -> Bool
esMultiplo n m = n `mod` m == 0

-- Determina si un número es divisor de otro
esDivisorI :: Int -> Int -> Bool
esDivisorI d n = esMultiplo n d

-- Determina si un número es par
esParI :: Int -> Bool
esParI n = n `mod` 2 == 0

-- Determina si un número es impar
esImpar :: Int -> Bool
esImpar n = n `mod` 2 == 1

-- Determina si dos números tienen la misma paridad (ambos pares o ambos impares)
mismaParidad :: Int -> Int -> Bool
mismaParidad a b = (esParI a && esParI b) || (esImpar a && esImpar b)

-- Determina si dos números tienen el mismo residuo al dividirlos por un módulo
mismoResiduo :: Int -> Int -> Int -> Bool
mismoResiduo a b modulo = (a `mod` modulo) == (b `mod` modulo)

-- Determina si dos números son congruentes módulo un valor dado
sonCongruentes :: Int -> Int -> Int -> Bool  
sonCongruentes a b modulo = mismoResiduo a b modulo

-- Determina si dos números son distintos
distintoDe :: Int -> Int -> Bool
distintoDe x y = x /= y

-- RELACIONES PRACTICA SECCION 3

-- Tipo para representar relaciones binarias como listas de pares ordenados
type Rel a b = [(a, b)]

-- Ejercicio 1.- Relación que combina divisibilidad y misma paridad, hace uso de las funciones esDivisor y mismaParidad
relacionIntegrada1 :: Rel Int Int
relacionIntegrada1 = [(x,y) | x <- [1..30], y <- [1..30], esDivisorI x y && mismaParidad x y]

-- Ejercicio 2.- Relación donde la suma es múltiplo de 5 y el primer elemento es menor, hace uso de las funciones esMultiplo, suma y menorQue
relacionIntegrada2 :: Rel Int Int
relacionIntegrada2 = [(x,y) | x <- [1..30], y <- [1..30], esMultiplo (sumaI x y) 5 && menorQue x y]

-- Ejercicio 3.- Relación de congruencia módulo 5 entre números distintos, hace uso de las funciones sonCongruentes y distintoDe
relacionIntegrada3 :: Rel Int Int
relacionIntegrada3 = [(x,y) | x <- [1..30], y <- [1..30], sonCongruentes x y 5, distintoDe x y]


--NATURALES
-- Cero es natural, Suc Cero es natural, Suc Suc Cero es natural, etc.
data Natural = Cero | Suc Natural deriving (Show,Eq) --Esto es para que se muestre y que se puedan comparar

esPar :: Natural -> Bool
esPar Cero = True
esPar (Suc n) = False
esPar (Suc (Suc n)) = esPar n


iguales :: Natural -> Natural -> Bool
iguales Cero Cero = True
iguales _ Cero = False
iguales Cero _ = False
iguales (Suc n) (Suc m) = iguales n m


maximo :: Natural -> Natural -> Natural 
maximo n Cero = n
maximo Cero n = n
maximo (Suc n) (Suc m) = Suc (maximo n m)


potencia :: Natural -> Natural -> Natural
potencia n Cero = Suc Cero
potencia n (Suc m) = multiplicacion n (potencia n m)

multiplicacion :: Natural -> Natural -> Natural
multiplicacion n Cero = Cero
multiplicacion n (Suc m) = suma n (multiplicacion n m)

suma :: Natural -> Natural -> Natural
suma n Cero = n
suma n (Suc m) = Suc (suma n m)
