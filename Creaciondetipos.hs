{-La estructura de lista es la siguiente-}
data List a = Void | Node a (List a) deriving Show

{-Ejercicio 1-}

longitud :: List a -> Int
longitud Void = 0
longitud (Node _ xs) = 1 + longitud xs

{-Ejercicio 2-}

estaContenido :: Eq a => List a -> a -> Bool
estaContenido Void elem = False
estaContenido (Node x xs) a = x == a || estaContenido xs a

{-Ejercicio 3-}

convertirAEstructura :: [a] -> List a
convertirAEstructura [] = Void
convertirAEstructura (x:xs) = Node x (convertirAEstructura xs)

{-Ejercicio 4-}

convertirALista :: List a -> [a]
convertirALista Void = []
convertirALista (Node x xs) = x : convertirALista xs

{-Ejercicio 5-}

conjunto :: Eq a => List a -> List a 
conjunto Void = Void
conjunto (Node x xs) = 
    if estaContenido xs x
        then conjunto xs
    else Node x (conjunto xs)

{-Ejercicio 6-}

eliminarIndice :: List a -> Int -> List a
eliminarIndice Void _ = Void
eliminarIndice (Node _ xs) 0 = xs
eliminarIndice (Node x xs) n = if n < 0 || n >= longitud (Node x xs)
    then error "Indice fuera del rango permitido"
    else Node x (eliminarIndice xs (n -1))
