type Num1 = Nume
data Nume = ListaS Char | ListaInt Int 
        deriving(Show)

myButLast :: [String] -> String
myButLast w = (last (init w))

isPalindrome :: String -> String
isPalindrome m = let w = foldl (\x y -> [x]++[y]) [] m
                     strw = concat (reverse (words w))
                    in
                    if w == strw
                        then True
                        else False
                
pack :: [Char] -> [String]
pack w = foldl (\x y -> if x == y 
                            then x++y
                            else [x]++[y]) [] w

-- #10
insArb :: Ord a => a -> Arbol a -> Arbol a 
insArb n [] -> Bifur n [] []
insArb n (Bifur m izq der) = if n == m
                                then (Bifur m izq der)
                                else if n < m 
                                    then Bifur m (insArb n izq) der
                                    else Bifur m izq (insArb n der)

-- #11
-- Evaluacion peresoza. No hay problemas con las listas infinitas.


-- Quiz
data Arbol = Hoja Int | Nodo Arbol Arbol
                deriving (Show, Eq)

sumar :: Arbol -> Int -> Arbol
sumar n (Hoja m) = Hoja (m+n)
sumar n (Nodo izq der) = Nodo (sumar n izq) (sumar n der)

data Arbol2 = Hoja [Int] | Nodo1 (Int,Int) Arbol Arbol | Nodo2 Arbol Arbol Arbol 
    deriving(Eq,Show)

map' :: Arbol2 -> (Int-> Int) -> Arbol2
map' arb (\x->x-1)




------------------------------------------





separarYalinear2 :: HypMap -> Int -> String -> String -> String -> [String]
separarYalinear2 hm num noseparar noajustar w = if noseparar == "SEPARAR" && noajustar == "NOAJUSTAR"
                                                then getResultFinal hm num (getLinePrimeraCorrida2 (breakLine (num) (string2line w)) num)
                                                else []

getResultFinal :: HypMap -> Int -> [String] -> [String]
getResultFinal h num w  = foldl (\x y -> if x == [] 
    then [y]
    else let ultima = last x
             palabra1 = (line2string [(head (head (getCadena num ultima (tail (lineBreaks2 h num ([Word (head (words y))]))))))])
             palabra2 = (line2string [(last (head (getCadena num ultima (tail (lineBreaks2 h num ([Word (head (words y))]))))))])
             largototal = (length ultima)+(length palabra1)
            in
            if largototal > num
                then x++[y]
                else ((init (x))++[ultima++" "++palabra1++"-"])++[palabra2++" "++(foldl (\o p -> if o /= [] then  o++" "++p else p) [] (tail (words (head y))))]) [] w



foldl (\x y -> if x == [] then [y] else let ultima=(last x) palabra1=(getWord1 (getFirtsWord h num y ultima)) palabra2=(getWord2 (getFirtsWord h num y ultima)) largototal=((length ultima)+(length palabra1)) in if largototal > num   then x++[y] else init (x)++[ultima++" "++palabra1]++[palabra2++" "++getWordUnion y] ) [] a








