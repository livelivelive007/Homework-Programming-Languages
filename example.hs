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
             palabra1 = printWord y h num
             palabra2 = printWord2 y h num
             largototal = (length ultima)+(length palabra1)
            in
            if largototal > num
                then x++[y]
                else (init (x))++[ultima++" "++palabra1]++[palabra2++" "++getWordUnion y] ) [] w

foldl (\x y -> if x == [] then [y] else let ultima=(last x) palabra1=(getWord1 (getFirtsWord h num y ultima)) palabra2=(getWord2 (getFirtsWord h num y ultima)) largototal=((length ultima)+(length palabra1)) in if largototal > num   then x++[y] else init (x)++[ultima++" "++palabra1]++[palabra2++" "++getWordUnion y] ) [] a




-----------------------------------------------------

type Estado = Map Token [[Char]]

main :: IO ()
main = do
        mainloop (fromList[])

mainloop :: Estado -> IO ()
mainloop estado = do
    putStr ">> "
    inpStr <- getLine
    let tokens = words inpStr
    let comando = tokens!!0

    case comando of
        "load" -> do
                    nombreArchivo <- getLine
                    dict <- openFile nombreArchivo ReadMode
                    nuevoestado <- loadWords dict estado
                    hClose dict
                    let totalpalabras = length nuevoestado
                    putStrLn $ "Diccionario cargado ("++[intToDigit totalpalabras]++" palabras)"
                    mainloop nuevoestado

        "show" -> do
                    let (nuevoestado, salida) = showPalabras estado
                    putStrLn salida
                    mainloop nuevoestado

        "ins" -> do
                    let (nuevoestado, salida) = setPalabra (tail tokens) estado
                    printStrLn $ "Palabra "++(init (tail tokens))++" agregada"
                    mainloop nuevoestado
        "save" -> do
                    let diccionario = tail tokens

                    let totalpalabras = length estado
                    printStrLn $ "Diccionario guardado ("++totalpalabras++" palabras)"
                    mainloop nuevoestado
        
        "split" -> do  
                    let longitud = head (tail tokens)
                    let separar = head (tail (tail tokens))
                    let ajustar = last (init tokens)
                    let texto = last tokens
                    
                    let (nuevoestado, salida) = splitWords longitud separar ajustar texto
                    printStrLn salida
                    mainloop nuevoestado
        "splitf" -> do  
                    let longitud = head (tail tokens)
                    let separar = head (tail (tail tokens))
                    let ajustar = last (init tokens)
                    let archivo1 = last (init tokens)
                  
                    let (nuevoestado, salida) = splitWords longitud separar ajustar archivo1
                    printStrLn salida
                    mainloop nuevoestado

        "exit" -> do
                    putStrLn "Saliento..."
        _      -> do
                    putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                    mainloop estado

loadWords :: Handle -> Estado -> IO Estado
loadWords inh estado = do
      ineof <- hIsEOF inh
      if ineof then return estado
               else do inpStr <- hGetLine inh
                       let nuevoestado = foldl createData estado ([words (map toLower inpStr)])
                       loadWords inh nuevoestado

createData :: Estado -> [[Char]] -> Estado
createData estado tok = insert (Word (head tok)) (getHypMap (last tok)) estado

getHypMap :: [Char] -> [[Char]]
getHypMap w = words [if c == '-' then ' ' else c|c <- w]



convertirListaFull :: String -> Line
convertirListaFull w = head (map (\x -> string2line x) [w])

convertirListaFull2 :: [String] -> Line
convertirListaFull2 w = head (map (\x -> string2line x) w)


getWord1 :: [Token] -> String 
getWord1 w = line2string [head w]

getWord2 :: [Token] -> String 
getWord2 w = line2string [last w]

obtenerDatoFinal :: [String] -> String -> [String]
obtenerDatoFinal w1 w2 = w1++[w2]

getCadena :: Int -> String -> [(Line, Line)] -> [Line]
getCadena num w cadena = convertCadenaArreglo (foldl (\x y -> let (a,b) = y 
                                                                  ph = init a 
                                                                  pw = tail a
                                                                  largototal = (length w)+(length ph)+1
                                                                  (m,n,k) = x
                                                        in
                                                        if largototal < num && k == 0
                                                            then (ph,pw,k+1)
                                                            else x ) ([],[],0) cadena)

convertCadenaArreglo :: (Line,Line,Int) -> [Line]
convertCadenaArreglo w = map (\(x,y,z) -> x++y) [w]

--(init (x))++[ultima++" "++palabra1]++[palabra2++" "++getWordUnion y]

-- foldl (\x y -> if x == [] then [y] else x++[(printWord y h num)] ) [] a


-- [([HypWord "con",Word "trola"],[]),([HypWord "contro",Word "la"],[])]
--getFirtsWord h num w ultima = head (getCadena num ultima (tail (lineBreaks2 h num ([Word (head (words w))]))))


-- lista = line2string (convertirListaFull lastone)
--  lista2 = line2string (convertirListaFull2 (map (\a -> spaces2cadena a) (init lista) ))


-- [([HypWord "con"], [Word "trola"]), ([HypWord "contro"], [Word "la"])]

--y = ["controla el presente"]
--x = ["Quien controla el","pasado controla el","futuro. Quien"]
--h = [(Word "controla",["con","tro","la"]),(Word "futuro",["fu","tu","ro"])]
--head (getCadena 20 (last x) (tail (lineBreaks2 h 20 ([Word (head (words (head y)))]))))

--[HypWord "con", Word "trola"]

--ultima = "futuro. Quien"
--palabra1 = con
--palabra2 = trola

--a = [ "Quien controla el","pasado controla el","futuro. Quien","controla el presente","controla el pasado."]

--y="controla el presente"

--head $ getFirtsWord h num y
--HypWord "con"
--line2string [head $ getFirtsWord h num y]
--"con-"

-- init (x)++[ultima++" "++palabra1++"-"]++ [palabra2++" "++(foldl (\o p -> if o /= [] then  o++" "++p else p) [] (tail (words y)))]

--foldl (\x y -> if x == [] then y else x++[y]) [] a

--((init (x))++[ultima++" "++palabra1++"-"])++[palabra2++" "++(foldl (\o p -> if o /= [] then  o++" "++p else p) [] (tail (words (head y))))]

--(["Quien controla el","pasado controla el", "futuro. Quien con-"])++["trola el presente"]

--x= [ "Quien controla el",
-- "pasado controla el",
-- "futuro. Quien"]
--y= "controla el presente"

-- ["el","presente"]
-- ... ++ ["futuro. Quien con-"] ++ ["trola el presente"]

--y=["controla el presente"]
--num=20
--h=[(Word "controla",["con","tro","la"]),(Word "futuro",["fu","tu","ro"])] 
--ultima = "futuro. Quien"



--(line2string [(head (head (getCadena num ultima (tail (lineBreaks2 h num ([Word (head (words (head y)))]))))))])
--(line2string [(head (head (getCadena num ultima (tail (lineBreaks2 h num [Word "controla"])))))])
--(line2string [(head (head (getCadena num ultima [([HypWord "con",Word "trola"],[]),([HypWord "contro",Word "la"],[])]))))])
--(line2string [(head [[HypWord "con",Word "trola"]])])
--"con-"

--(line2string [(last (head [[HypWord "con",Word "trola"]]))])
--"trola"

--[(foldl (\x y -> x++y) [] w1)++w2]

--(init x)++[(ultima++" "++palabra1)]++[(head (tail (words (head y))))]
--ultima++".."++palabra1++".."++palabra2++".."++largototal

--foldl (\x y -> x++".."++y) [] ["Quien controla el pasado","controla el futuro. Quien","controla el presente controla","el pasado."]

--combinarCadenas ((init x)++ultima++palabra1):combinarCadenas(palabra2++(tail y)):[]
--combinarCadenas :: [Char] -> [String]
--combinarCadenas w = map (\x xs -> x:xs ) w

--w = "futuro. Quien"
--cadena = [([HypWord "con",Word "trola"],[]),([HypWord "contro",Word "la"],[])]
--foldl (\x y -> let (a,b)=(y) ph=(init a) pw=(tail a) largototal=((length w)+(length ph)+1) (m,n,k)=(x) in if largototal<=num && k==0 then (ph,pw,k+1) else x ) ([],[],0) cadena



------------------------------------------------------------------------------------





