valorAbs :: Int -> Int
valorAbs 0 = 0
valorAbs n = if 0 < n  then n else n* (-1)

esDivisor :: Int -> Int -> Bool
esDivisor n m = m `mod` n == 0

cuadratica :: Float -> Float -> Float -> Float -> Float
cuadratica a b c v = a*(v)^2 + b*(v) + c

sumaFracciones :: (Int, Int) -> (Int, Int) -> (Int, Int)
sumaFracciones (a,b) (c,d) = if b==d then ((a+c,b)) else (((a*d)+(c*b),b*d)) 

comparador :: Float -> Float -> Int
comparador n m = if n == m then 0 else (if n>m then 1 else -1) 

puntoMedio :: (Float,Float) -> (Float,Float) -> (Float,Float)
puntoMedio (x,y) (w,z) = ((x+w)/2,(y+z)/2)