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
--insertBlanks

--j
--separarYalinear





main = do
    
    print "My first Haskell program"

    --name <- getLine
    --print ("Hello, " ++ name)

