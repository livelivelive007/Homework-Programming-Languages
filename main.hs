import qualified Data.Text as T
import Prelude hiding (null,filter)
import Data.Map hiding (map, foldl, drop)
import Data.List (sort,map)
import System.IO
import Data.Char

type HypMap = [(Token,[String])]
type Estado = Map String [String]

type Line = [Token]
data Token = Word String | Blank | HypWord String
            deriving (Eq, Show)

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
                    putStrLn ">>> Nombre archivo entrada: "
                    nombreArchivo <- getLine
                    inh <- openFile nombreArchivo ReadMode
                    hSetEncoding inh utf8
                    nuevoestado <- cargar inh estado
                    hClose inh
                    putStrLn $ "Archivo " ++ nombreArchivo ++ " fue cargado"
                    mainloop nuevoestado

        "show" -> do
                    let (nuevoestado, salida) = showPalabras estado
                    putStrLn salida
                    mainloop nuevoestado


        "ins" -> do
                    let nuevoestado = setPalabra (tail tokens) estado
                    putStrLn $ "Palabra "++head (init (tail tokens))++" agregada"
                    mainloop nuevoestado

        "save" -> do
                    putStrLn ">>> Nombre archivo salida: "
                    nombreArchivo <- getLine
                    outh <- openFile nombreArchivo WriteMode
                    hSetEncoding outh utf8
                    descargar outh (sort (toList estado))
                    hClose outh
                    let totalpalabras = length estado
                    putStrLn $ "Diccionario guardado ( "++show totalpalabras++" palabras)"
                    mainloop estado
        
        "split" -> do 
                    let longitud = (read (tokens!!1)::Int)
                    let separar = (tokens!!2)
                    let ajustar = (tokens!!3)
                    let texto = (foldl (\x y -> if x /= "" then x++" "++y else y) "" (tail (tail (tail (tail tokens)))))
                    let salida = splitWords longitud separar ajustar texto
                    putStrLn salida
                    mainloop estado

        "splitf" -> do 
                    let longitud = (read (tokens!!1)::Int)
                    let separar = (tokens!!2)
                    let ajustar = (tokens!!3)
                    let texto = (tokens!!4)
                
                    inh <- openFile texto ReadMode
                    cadenatexto <- hGetLine inh
                    hClose inh

                    let salida = splitWords longitud separar ajustar cadenatexto
                    putStrLn salida

                    let tamano = length tokens
                    if tamano == 6 
                        then do 
                            outh <- openFile (tokens!!5) WriteMode
                            descargar2 outh salida
                            hClose outh 
                            mainloop estado
                        else mainloop estado

        "split2" -> do 
                    let longitud = (read (tokens!!1)::Int)
                    let separar = (tokens!!2)
                    let ajustar = (tokens!!3)
                    let texto = (foldl (\x y -> if x /= "" then x++" "++y else y) "" (tail (tail (tail (tail tokens)))))
                    
                    let hp = convertEstado estado

                    let salida = splitWords2 hp longitud separar ajustar texto
                    putStrLn salida
                    mainloop estado

        "splitf2" -> do 
                    let longitud = (read (tokens!!1)::Int)
                    let separar = (tokens!!2)
                    let ajustar = (tokens!!3)
                    let texto = (tokens!!4)
                
                    inh <- openFile texto ReadMode
                    cadenatexto <- hGetLine inh
                    hClose inh

                    let hp = convertEstado estado

                    let salida = splitWords2 hp longitud separar ajustar cadenatexto
                    putStrLn salida

                    let tamano = length tokens
                    if tamano == 6 
                        then do 
                            outh <- openFile (tokens!!5) WriteMode
                            descargar2 outh salida
                            hClose outh 
                            mainloop estado
                        else mainloop estado

        "exit" -> do
                    putStrLn "Saliento..."
        _      -> do
                    putStrLn $ "Comando desconocido ("++ comando ++"): '" ++ inpStr ++ "'" 
                    mainloop estado

---------------------------------------------------

cargar :: Handle -> Estado -> IO Estado
cargar inh estado = do
      ineof <- hIsEOF inh
      if ineof then return estado
               else do inpStr <- hGetLine inh
                       let nuevoestado = contar_token estado (words (map toLower inpStr))
                       cargar inh nuevoestado

contar_token :: Estado -> [String] -> Estado
contar_token estado tok = insert (head tok) (getHypMap (last tok)) estado

getHypMap :: String -> [String]
getHypMap w = words [if c == '-' then ' ' else c|c <- w]


getLinea11 :: (HypMap,Int) -> HypMap
getLinea11 w = head $ map (\(x,y) -> x ) [w]

convertEstado :: Estado -> HypMap
convertEstado estado = let keyss = keys estado 
                           result = foldl (\x y ->let (a,b) = x in ((getLinea11 x)++[(Word (keyss!!b), y)],b+1) ) ([],0) estado
                        in getLinea11 result 

splitWords2 :: HypMap -> Int -> String -> String -> String -> String
splitWords2 hp longitud separar ajustar texto = let result = separarYalinear2 hp longitud separar ajustar texto
                                            in
                                            foldl (\x y -> if x /= "" then x++"\n"++y else y) "" result 

descargar2 outh [] = return ()
descargar2 outh (kvs) = do hPutStrLn outh kvs

splitWords :: Int -> String -> String -> String -> String
splitWords longitud separar ajustar texto = let result = separarYalinear longitud separar ajustar texto
                                            in
                                            foldl (\x y -> if x /= "" then x++"\n"++y else y) "" result 

showPalabras :: Estado -> (Estado, String)
showPalabras estado = (estado, show estado)

setPalabra :: [String] -> Estado -> Estado
setPalabra tokens estado = contar_token estado tokens

descargar outh [] = return ()
descargar outh ((k,v):kvs) = do hPutStrLn outh $ k ++ " " ++ (show v)
                                descargar outh kvs

---------------------------------------------------

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

hyphenate :: HypMap -> Token -> [(Token,Token)]
hyphenate q w = let ft = T.unpack (T.takeWhileEnd (=='.') (T.pack $ convertTokenString w)) 
                in if (last (convertTokenString w)) == '.' then case Prelude.lookup (Word $ T.unpack $ T.dropWhileEnd (=='.') $ T.pack $ convertTokenString w) q of
                     Nothing -> [] --Porque entra aqui? Si realmente le quita los puntos al final
                     Just valor -> addPoints ft (mergers valor)
                    else case Prelude.lookup w q of
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
separarYalinear num noseparar noajustar w = if noseparar /= "" && noajustar /= "" 
                                                then if noseparar == "NOSEPARAR" && noajustar == "NOAJUSTAR"
                                                    then getLinePrimeraCorrida(breakLine num (string2line w)) num
                                                    else if noseparar == "NOSEPARAR" && noajustar == "AJUSTAR"
                                                        then getLinePrimeraCorrida3(breakLine num (string2line w)) num
                                                        else []
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
                                                else if noseparar == "SEPARAR" && noajustar == "AJUSTAR"
                                                    then let texto = foldl (\x y -> if x == "" then y else x++" "++y) "" (getResultFinal hm num (getLinePrimeraCorrida (breakLine (num) (string2line w)) num))
                                                        in
                                                        getLinePrimeraCorrida3(breakLine num (string2line texto)) num
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
