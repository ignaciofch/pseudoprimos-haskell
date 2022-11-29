--Chavez Ignacio

-- EJERCICIO 1. sonCoprimos

mayor :: Integer -> Integer -> Integer
mayor a b
 | a >= b = a
 | a < b = b

menor :: Integer -> Integer -> Integer
menor a b
 | a <= b = a
 | a > b = b

--a|b
divide :: Integer -> Integer -> Bool
divide a b = mod b a == 0

mcd :: Integer -> Integer -> Integer
mcd  a b
 | menor a b `divide` mayor a b = menor a b
 | otherwise = mcd (mod (mayor a b) (menor a b)) (menor a b)

sonCoprimos :: Integer -> Integer -> Bool
sonCoprimos a b = (mcd a b) == 1

-- EJE3RCICO 2: es2Pseudoprimo

menorDivisor :: Integer -> Integer
menorDivisor x = auxiliarDivisor x 2

auxiliarDivisor :: Integer -> Integer -> Integer
auxiliarDivisor x y
 | mod x y == 0 = y
 | otherwise = auxiliarDivisor x (y+1)

esPrimo :: Integer -> Bool
esPrimo x
  | x == 1 = False
  | otherwise = menorDivisor x == x

esCongruente :: Integer -> Integer -> Integer -> Bool
esCongruente a b c = (mod a c) == (mod b c)


es2Pseudoprimo :: Integer -> Bool
es2Pseudoprimo 1 = False
es2Pseudoprimo n = ( esPrimo n == False ) && ( sonCoprimos n 2 == True ) && ( (esCongruente (2^(n-1)) 1 n) == True )

-- EJERCICIO 3: cantidad3Pseudoprimos

es3Pseudoprimo :: Integer -> Bool
es3Pseudoprimo 1 = False
es3Pseudoprimo n = ( esPrimo n == False ) && ( sonCoprimos n 3 == True ) && ( (esCongruente (3^(n-1)) 1 n) == True )

--n fijo -> n contador de 3-pseudoprimnos -> n que varia
contadorPseudoprimos :: Integer -> Integer -> Integer -> Integer
contadorPseudoprimos a b c
  | a < c = b
  | es3Pseudoprimo c = contadorPseudoprimos a (b+1) (c+1)
  | otherwise = contadorPseudoprimos a b (c+1)

cantidad3Pseudoprimos :: Integer -> Integer
cantidad3Pseudoprimos n = contadorPseudoprimos n 0 1

-- EJERCICIO 4: kesimo2y3Pseudoprimo

es2y3Pseudoprimo :: Integer -> Bool
es2y3Pseudoprimo n = (es2Pseudoprimo n) && (es3Pseudoprimo n)

--posicion fija 2y3pseudoprimo -> n variante -> poscion que varia contando pseudoprimos
ordenador2y3Pseudoprimos :: Integer -> Integer -> Integer -> Integer
ordenador2y3Pseudoprimos a b c
 | a == c = b
 | es2y3Pseudoprimo (b+1) == True = ordenador2y3Pseudoprimos a (b+1) (c+1)
 | es2y3Pseudoprimo (b+1) == False = ordenador2y3Pseudoprimos a (b+1) c

kesimo2y3Pseudoprimo :: Integer -> Integer
kesimo2y3Pseudoprimo n = ordenador2y3Pseudoprimos n 1100 0

{-

-- EJERCICIO 5: esCarmichael
esCarmichael :: Integer -> Bool

-}
