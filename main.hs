import qualified Data.Text as T
--import Prelude hiding (null, lookup, filter)
--import Data.Map hiding (map, foldl, drop)
--import System.IO

type Line = [Token]
data Token = Word String | Blank | HypWord String
            deriving (Eq, Show)

string2line :: String -> Line
string2line w = map (\x->Word x) $ words w
--
line2string :: Line -> String
line2string w = T.unpack $ T.dropAround (==' ') $ T.pack $ concat $ map (spaces2cadena) w

spaces2cadena :: Token -> String
spaces2cadena (Word w) = w++" "
spaces2cadena (HypWord w) = w++"- "
spaces2cadena (Blank) = " "
--
tokenLength :: Token -> Int
tokenLength (Word w) = length w
tokenLength (HypWord w) = 1 + length w
tokenLength (Blank) = 1

--lineLength [Word "Aquel",Word "que",HypWord "contro",Word "la"]⇒ 20
--lineLength [Word "Aquel",Word "que",HypWord "con"] ⇒ 14
lineLength :: Line -> Int
lineLength [] = 0
lineLength w = (-1) + lineLength2 w
               
lineLength2 :: Line -> Int
lineLength2 w = sum $ map (tokenLength2) w 
--foldl possibility

tokenLength2 :: Token -> Int
tokenLength2 (Word w) = 1 + length w
tokenLength2 (HypWord w) = 2 + length w
tokenLength2 (Blank) = 1
--
--breakLine 9 [Word "Aquel",Word "que",Word "controla"]
--([Word "Aquel",Word "que"],[Word "controla"])
breakLine :: Int -> Line -> (Line, Line)
breakLine num [] = ([],[])
breakLine num linea = foldl (\x y ->let (a,b) = x
                                        w = a++[y]
                                    in
                                    if lineLength (w) <= num && b == []
                                        then (w, [])
                                        else (a,b++[y])) ([],[]) linea
--
--mergers ["co","nt","ro","la"]
mergers :: [String] -> [(Token,Token)]
mergers w = if length w == 1 then [] else 
             if length w == 2 then [convertTypeCharToToken (mer1 w)] else 
              if length w == 3 then [convertTypeCharToToken (mer1 w)] ++ [convertTypeCharToToken (mer2 w)] 
              else [convertTypeCharToToken (mer1 w)] ++ [convertTypeCharToToken (mer2 w)] ++ [convertTypeCharToToken (mer3 w)]

mer1 :: [String] -> ([Char],[Char])
mer1 w = foldl (\x y -> let (a,b) = x in if a == [] then (y,b) else 
                                                      if a /= [] then (a,b++y) else (a,b) ) ([],[]) w

mer2 :: [String] -> ([Char],[Char])
mer2 w = foldl (\x y -> let (a,b) = x in if a == [] then (y,b) else 
                                    if a /= [] && b == [] then (a++y,"f") else 
                                       if b == "f" then (a,y) else 
                                           if b /= [] && b /= "f" then (a,b++y) else (a,b)) ([],[]) w

mer3 :: [String] -> ([Char],[Char])
mer3 w = foldl (\x y -> let (a,b) = x in if a == [] then (y,b) else 
                                     if a /= [] && b == [] then (a++y,"f") else 
                                      if b == "f" then (a++y,"t") else 
                                        if b == "t" then (a,y) else (a,b) ) ([],[]) w
--
--hyphenate [(Word "controla",["con","tro","la"]),(Word "futuro",["fu","tu","ro"]),(Word "presente",["pre","sen","te"])] (Word "controla")
--[(HypWord "con",Word "trola"),(HypWord "contro",Word "la")]

--hyphenate [(Word "controla",["con","tro","la"]),(Word "futuro",["fu","tu","ro"]),(Word "presente",["pre","sen","te"])] (Word "futuro...")
--[(HypWord "fu",Word "turo..."),(HypWord "futu",Word "ro...")]

type HypMap = [(Token,[String])]
hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate q w = let ft = T.unpack (T.takeWhileEnd (=='.') (T.pack $ convertTokenString w)) 
                in if (last (convertTokenString w)) == '.' then case lookup (Word $ T.unpack $ T.dropWhileEnd (=='.') $ T.pack $ convertTokenString w) q of
                     Nothing -> [] --Porque entra aqui? Si realmente le quita los puntos al final
                     Just valor -> addPoints ft (mergers valor)
                    else case lookup w q of
                     Nothing -> []
                     Just valor -> mergers valor

convertTokenString :: Token -> String
convertTokenString (Word w) = w++""
convertTokenString (HypWord w) = w++""
convertTokenString (Blank) = ""

addPoints :: String -> [(Token,Token)] -> [(Token,Token)]
addPoints ft w = map (\(x,y) -> (x,string22line $ (convertTokenString (y))++ft)) w

convertTypeCharToToken :: ([Char],[Char]) -> (Token, Token)
convertTypeCharToToken w = head $ map (\(x,y) -> (HypWord x, Word y)) [w]

string22line :: String -> Token
string22line w = Word w

--transf :: Token -> Token -> Token
--transf w e = w++e

--h
--lineBreaks [(Word "controla",["con","tro","la"]),(Word "futuro",["fu","tu","ro"])] 17 [Word "Aquel",Word "que",Word "controla"]
--[([Word "Aquel",Word "que"], [Word "controla"]),
-- ([Word "Aquel",Word "que",HypWord "con"], [Word "trola"]),
-- ([Word "Aquel",Word "que",HypWord "contro"], [Word "la"])]

--lineBreaks enHyp 12 [Word "Aquel"] ⇒ [([Word "Aquel"],[])]

lineBreaks :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks q num w = ([breakLine num w])++(let combi = hyphenate q (last w)
                        in
                        map (\(HypWord x,Word y) -> let tuff = [HypWord x,Word y]
                                                        lista = ((reverse $ drop 1 $ reverse w)++tuff) 
                                                        in breakLine num lista) combi)

--i
--insertBlanks 3 [Word "hola", Word "mundo", Word "cruel"]
-- ⇒[Word "hola", Blank,Blank, Word "mundo", Blank, Word "cruel"]
--insertBlanks 5 [Word "hola", Word "mundo", Word "cruel"]
-- ⇒ [Word "hola", Blank,Blank,Blank, Word "mundo", Blank,Blank, Word "cruel"]
--insertBlanks 5 [Word "hola", Word "mundo", Word "cruel", Word "adios"]
-- ⇒ [Word "hola",Blank,Blank, Word "mundo", Blank,Blank,Word "cruel", Blank, Word "adios"]

insertBlanks :: Int -> Line -> Line
insertBlanks num [] = []
insertBlanks num w = let largo = length w
                        in
                        getLinea (getLineaReturn num w 1 largo)

getIlera :: Int -> Line -> Int -> Int -> (Line, Int, Int)
getIlera num w z largoriginal = let largo = length w 
                        in 
                        foldl (\x y -> let (a,b,c) = x 
                                        in 
                                        if c < largo && b /= 0
                                            then if y /= Blank && largoriginal /= c
                                                    then ((convertStringToken a y),(-1)+b,c+1)
                                                    else ((convertStringToken0 a y),b,c)
                                            else ((convertStringToken0 a y),b,1) ) ([],num,z) w
                                        
getLineaReturn :: Int -> Line -> Int -> Int -> (Line, Int, Int)
getLineaReturn num w cont largoriginal = let (x,y,z) = getIlera num w cont largoriginal
                            in
                            if num == 0 
                                then (w,0,0)
                                else getLineaReturn y x cont largoriginal
--getIlera num w cont
getLinea  :: (Line, Int, Int) -> Line
getLinea w = head $ map (\(x,y,z) -> x ) [w]

getLinea2  :: (Line, Int, Int) -> Line
getLinea2 w = head $ map (\(x,y,z) -> x ) [w]

convertStringToken0 :: [Token] -> Token -> Line
convertStringToken0 lista w = lista++[w]

convertStringToken :: [Token] -> Token -> Line
convertStringToken lista w = lista++[w]++[Blank]

--j
--separarYalinear 20 "NOSEPARAR" "NOAJUSTAR" "Quien controla el pasado controla el futuro. Quien controla el presente controla el pasado."
 -- 12345678901234567890
-- [ "Quien controla el",
-- "pasado controla el",
-- "futuro. Quien",
-- "controla el presente",
-- "controla el pasado."]

separarYalinear :: Int -> String -> String -> String -> [String]
separarYalinear num noseparar noajustar w = if noseparar == "NOSEPARAR" && noajustar == "NOAJUSTAR"
                                                then getLinePrimeraCorrida(breakLine num (string2line w)) num
                                                else []

getLinePrimeraCorrida :: (Line,Line) -> Int-> [String]
getLinePrimeraCorrida w n = let (a,b) = w in if b /= []
                                                then [line2string a]++getLinePrimeraCorrida(breakLine n b) n
                                                else [line2string a]

----------------------------------------------------------
--separarYalinear3 20 "NOSEPARAR" "AJUSTAR" "Quien controla el pasado controla el futuro. Quien controla el presente controla el pasado."
 -- 12345678901234567890
-- ["Quien   controla  el",
-- "pasado controla el",
-- "futuro. Quien",
-- "controla el presente",
-- "controla el pasado."]

--["Quien   controla  el",
-- "pasado  controla  el",
-- "futuro.        Quien",
-- "controla el presente",
-- "controla  el pasado."]
separarYalinear3 :: Int -> String -> String -> String -> [String]
separarYalinear3 num noseparar noajustar w = if noseparar == "NOSEPARAR" && noajustar == "AJUSTAR"
                                                then getLinePrimeraCorrida3(breakLine num (string2line w)) num
                                                else []

getLinePrimeraCorrida3 :: (Line,Line) -> Int-> [String]
getLinePrimeraCorrida3 w n = let (a,b) = w 
                                 largo = length (line2string a)
                                 dato = n - largo
                                in if b /= []
                                    then if dato /= 0
                                        then [line2string (insertBlanks dato a)]++getLinePrimeraCorrida3(breakLine n b) n
                                        else [line2string a]++getLinePrimeraCorrida3(breakLine n b) n
                                    else if dato /= 0
                                        then [line2string (insertBlanks dato a)]
                                        else [line2string a]

-------------------------------------------------
--separarYalinear2 [(Word "controla",["con","tro","la"]),(Word "futuro",["fu","tu","ro"])] 20 "SEPARAR" "NOAJUSTAR" "Quien controla el pasado controla el futuro. Quien controla el presente controla el pasado."
-- ["Quien controla el","pasado controla el","futuro. Quien con-","trola el presente","controla el pasado."]

 -- 12345678901234567890
-- [ "Quien controla el",
-- "pasado controla el",
-- "futuro. Quien",
-- "controla el presente",
-- "controla el pasado."]

--["Quien controla el",
--"pasado controla el",
--"futuro. Quien con-",
--"trola el presente",
--"controla el pasado."]

separarYalinear2 :: HypMap -> Int -> String -> String -> String -> [String]
separarYalinear2 hm num noseparar noajustar w = if noseparar == "SEPARAR" && noajustar == "NOAJUSTAR"
                                                then getResultFinal hm num (getLinePrimeraCorrida (breakLine (num) (string2line w)) num)
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
                else if palabra1 /= ""
                    then (init (x))++[ultima++" "++palabra1]++[palabra2++" "++getWordUnion y] 
                    else x++[y] ) [] w

getWordUnion :: String -> [Char]
getWordUnion w = foldl (\o p -> if o /= [] then  o++" "++p else p) [] (tail (words w))

printWord :: String -> HypMap -> Int -> String 
printWord w h num  = let fword = getFirtsWord h num w 
                    in 
                    if fword /= []
                        then line2string [head (head (fword))]
                        else ""

printWord2 :: String -> HypMap -> Int -> String 
printWord2 w h num  = let fword = getFirtsWord h num w 
                    in 
                    if fword /= []
                        then line2string [last (head (fword))]
                        else ""

getFirtsWord :: HypMap -> Int -> String -> [Line]
getFirtsWord h num w = let palabra = tail (lineBreaks h num ([Word (head (words w))]))
                        in  
                        if palabra /= []
                            then map (\(x,y) -> x) [(head (palabra))]
                            else []

-----------------------------------------------------------------------------
--separarYalinear4 [(Word "controla",["con","tro","la"]),(Word "futuro",["fu","tu","ro"])] 20 "SEPARAR" "AJUSTAR" "Quien controla el pasado controla el futuro. Quien controla el presente controla el pasado."
--["Quien   controla  el",
-- "pasado  controla  el",
-- "futuro.  Quien  con-",
-- "trola   el  presente",
-- "controla  el pasado."]

separarYalinear4 :: HypMap -> Int -> String -> String -> String -> [String]
separarYalinear4 hm num noseparar noajustar w = let texto = foldl (\x y -> if x == "" 
                                                                                then y
                                                                                else x++" "++y) "" (getResultFinal hm num (getLinePrimeraCorrida (breakLine (num) (string2line w)) num))
                                                in
                                                if noseparar == "SEPARAR" && noajustar == "AJUSTAR"
                                                    then getLinePrimeraCorrida3(breakLine num (string2line texto)) num
                                                    else []

getResultFinal2 :: HypMap -> Int -> [String] -> [String]
getResultFinal2 h num w  = foldl (\x y -> if x == [] 
    then [y]
    else let ultima = last x
             palabra1 = printWord y h num
             palabra2 = printWord2 y h num
             largototal = (length ultima)+(length palabra1)
            in
            if largototal > num
                then x++[y]
                else if palabra1 /= ""
                    then (init (x))++[ultima++" "++palabra1]++[palabra2++" "++getWordUnion y] 
                    else x++[y] ) [] w

getLinePrimeraCorrida4 :: (Line,Line) -> Int-> [String]
getLinePrimeraCorrida4 w n = let (a,b) = w 
                                 largo = length (line2string a)
                                 dato = n - largo
                                in if b /= []
                                    then if dato /= 0
                                        then [line2string (insertBlanks dato a)]++getLinePrimeraCorrida4(breakLine n b) n
                                        else [line2string a]++getLinePrimeraCorrida4(breakLine n b) n
                                    else if dato /= 0
                                        then [line2string (insertBlanks dato a)]
                                        else [line2string a]



-----------------------------------------------------------------------------



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

