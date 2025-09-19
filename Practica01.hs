--función auxiliar usadas:
-- 'mod' calcula el residuo de una division entera
-- 'mod' se expresa de la forma: a `mod` b donde b divide a


--Valor absoluto de un entero
--Devuleve siempre el numero en positivo, con el cero devolveindo cero
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