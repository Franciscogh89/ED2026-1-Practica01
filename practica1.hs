module LabDis where

-- FUNCIONES AUXILIARES PARA LA SECCION 3 DE LA PRACTICA

-- Determina si el primer número es menor que el segundo
menorQue :: Int -> Int -> Bool
menorQue x y = x < y

-- Suma dos números de cualquier tipo numérico
suma :: (Num a) => a -> a -> a
suma x y = x + y

-- Determina si un número es múltiplo de otro (resto de división es 0)
esMultiplo :: Int -> Int -> Bool
esMultiplo n m = n `mod` m == 0

-- Determina si un número es divisor de otro
esDivisor :: Int -> Int -> Bool
esDivisor d n = esMultiplo n d

-- Determina si un número es par
esPar :: Int -> Bool
esPar n = n `mod` 2 == 0

-- Determina si un número es impar
esImpar :: Int -> Bool
esImpar n = n `mod` 2 == 1

-- Determina si dos números tienen la misma paridad (ambos pares o ambos impares)
mismaParidad :: Int -> Int -> Bool
mismaParidad a b = (esPar a && esPar b) || (esImpar a && esImpar b)

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
relacionIntegrada1 = [(x,y) | x <- [1..30], y <- [1..30], esDivisor x y && mismaParidad x y]

-- Ejercicio 2.- Relación donde la suma es múltiplo de 5 y el primer elemento es menor, hace uso de las funciones esMultiplo, suma y menorQue
relacionIntegrada2 :: Rel Int Int
relacionIntegrada2 = [(x,y) | x <- [1..30], y <- [1..30], esMultiplo (suma x y) 5 && menorQue x y]

-- Ejercicio 3.- Relación de congruencia módulo 5 entre números distintos, hace uso de las funciones sonCongruentes y distintoDe
relacionIntegrada3 :: Rel Int Int
relacionIntegrada3 = [(x,y) | x <- [1..30], y <- [1..30], sonCongruentes x y 5, distintoDe x y]

