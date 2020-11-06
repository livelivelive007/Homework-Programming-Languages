import qualified Data.Text as T

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
insertBlanks num w = let largo = length w in if largo == 1 || (largo == 2 && (last w) == Blank) || (largo == 3 && (last w) == Blank)
                            then w
                            else getLineaReturn (getIlera num w largo 1) largo 

getIlera :: Int -> Line -> Int -> Int -> (Line, Int, Int)
getIlera num w largo z = foldl (\x y -> let (a,b,c) = x 
                    in 
                    if c < largo && b /= 0
                        then if y /= Blank 
                                then ((convertStringToken a y),(-1)+b,c+1)
                                else ((convertStringToken0 a y),b,c)
                        else ((convertStringToken2 a y),b,1) ) ([],num,z) w

getLineaReturn :: (Line, Int, Int) -> Int -> Line
getLineaReturn w largo = head $ map (\(x,y,z) -> getLinea (getIlera y x largo z)) [w]

getLinea  :: (Line, Int, Int) -> Line
getLinea w = head $ map (\(x,y,z) -> x) [w]

convertStringToken0 :: [Token] -> Token -> Line
convertStringToken0 lista w = lista++[w]

convertStringToken :: [Token] -> Token -> Line
convertStringToken lista w = lista++[w]++[Blank]

convertStringToken2 :: [Token] -> Token -> Line
convertStringToken2 lista w = lista++[w]

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

--separarYalinear2 [(Word "controla",["con","tro","la"]),(Word "futuro",["fu","tu","ro"])] 20 "SEPARAR" "NOAJUSTAR" "Quien controla el pasado controla el futuro. Quien controla el presente controla el pasado."
 -- 12345678901234567890
-- ["Quien controla el","pasado controla el","futuro. Quien con-","trola el presente","controla el pasado."]

--head $ getFirtsWord h num y
--HypWord "con"
--line2string [head $ getFirtsWord h num y]
--"con-"


separarYalinear2 :: HypMap -> Int -> String -> String -> String -> [String]
separarYalinear2 hm num noseparar noajustar w = if noseparar == "SEPARAR" && noajustar == "NOAJUSTAR"
                                                then getResultFinal hm num (getLinePrimeraCorrida2 (breakLine (num) (string2line w)) num)
                                                else []

getResultFinal :: HypMap -> Int -> [String] -> [String]
getResultFinal h num w  = foldl (\x y -> if x == [] 
    then [y]
    else let lastone = last y
            in x++[y]) [] w

-- lista = line2string (convertirListaFull lastone)
--              lista2 = line2string (convertirListaFull2 (map (\a -> spaces2cadena a) (init lista) ))





convertirListaFull :: String -> Line
convertirListaFull w = head (map (\x -> string2line x) [w])

convertirListaFull2 :: [String] -> Line
convertirListaFull2 w = head (map (\x -> string2line x) w)







-- foldl (\x y -> if x == [] then [y] else x++[(printWord y h num)] ) [] a

getWordUnion :: String -> [Char]
getWordUnion w = foldl (\o p -> if o /= [] then  o++" "++p else p) [] (tail (words w))

printWord :: String -> HypMap -> Int -> String 
printWord w h num  = line2string [head (getFirtsWord h num w)]

printWord2 :: String -> HypMap -> Int -> String 
printWord2 w h num  = line2string [last (getFirtsWord h num w)]

getFirtsWord :: HypMap -> Int -> String -> Line
getFirtsWord h num w = head (map (\(x,y) -> x) [(head (tail (lineBreaks2 h num ([Word (head (words w))]))))])

-- [([HypWord "con",Word "trola"],[]),([HypWord "contro",Word "la"],[])]
--getFirtsWord h num w ultima = head (getCadena num ultima (tail (lineBreaks2 h num ([Word (head (words w))]))))

getLinePrimeraCorrida2 :: (Line,Line) -> Int-> [String]
getLinePrimeraCorrida2 w n = let (a,b) = w in if b /= []
                                                then [line2string a]++getLinePrimeraCorrida2 (breakLine n b) n
                                                else [line2string a]

lineBreaks2 :: HypMap -> Int -> Line -> [(Line, Line)]
lineBreaks2 q num w = ([breakLine num w])++(let combi = hyphenate q (last w)
                        in
                        map (\(HypWord x,Word y) -> let tuff = [HypWord x,Word y]
                                                        lista = ((reverse $ drop 1 $ reverse w)++tuff) 
                                                        in breakLine num lista) combi)
          
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

 -- 12345678901234567890
-- [ "Quien controla el",
-- "pasado controla el",
-- "futuro. Quien",
-- "controla el presente",
-- "controla el pasado."]

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



main = do
    
    print "My first Haskell program"

    --name <- getLine
    --print ("Hello, " ++ name)

