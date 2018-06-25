import qualified Data.List.Split as S
import qualified Data.Map as M
import qualified Data.List as L

unMaybe :: (Maybe a) -> a -> a
unMaybe (Just x) _ = x
unMaybe (Nothing) deflt = deflt

data Object = Object {text::String, values::(M.Map String Object)}
getVal :: Object -> String -> Object
runFunc :: Object -> String -> IO ()
getVal obj val = unMaybe (M.lookup val (values obj)) (Object "" M.empty) where --TODO: should empty thing be actually empty?
runFunc x y = putStrLn "" --TODO

lineEnders = ";,"
inlineSymbols = "`~!@#$%^&*-+=|:<>/?"
priorityInlineSymbols = "._"
areaOpeners = "{[(\"'"
areaClosers = "}])\"'"

splitLine :: String -> [String]
splitLine line = splitSymbols $ separateEnclosedAreas "" line where
  splitSymbols :: [String] -> [String]
  splitSymbols [line]
    | (line == "") || (elem (head line) areaOpeners) = [line]
    | otherwise = S.split (S.dropBlanks $ S.oneOf inlineSymbols) line
  splitSymbols (x:y) = splitSymbols [x] ++ splitSymbols y

  separateEnclosedAreas :: [Char] -> String -> [String] --separate areas enclosed by {} [] () "" or ''
  separateEnclosedAreas closers area
    | area == "" = []
    | head area == '\\' = qualifiedJoinOnto closers (doubleHead area) (separateEnclosedAreas closers (doubleTail area))
    | (elem (head area) areaOpeners) && ((closers == "") || head closers /= respectiveCloser) =
      joinOnto (head area) (separateEnclosedAreas (respectiveCloser:closers) (tail area))
    | (closers == "") || (head area /= head closers) = qualifiedJoinOnto closers [head area] (separateEnclosedAreas closers (tail area))
    | otherwise = joinAreaEnd closers (head area) (separateEnclosedAreas (tail closers) (tail area))
      where
        respectiveCloser = areaClosers !! unMaybe (L.elemIndex (head area) areaOpeners) 0

        doubleHead :: [a] -> [a]
        doubleHead (x:y:_) = [x,y]
        doubleHead [x] = [x]
        doubleHead [] = []

        doubleTail :: [a] -> [a]
        doubleTail (_:_:x) = x
        doubleTail _ = []

        joinOnto :: Char -> [String] -> [String]
        joinOnto c (x:y) = (c:x):y
        joinOnto c []  =  [[c]]

        joinAreaEnd :: String -> Char -> [String] -> [String]
        joinAreaEnd closers x y
          | length closers == 1 = [x]:y
          | otherwise = joinOnto x y

        qualifiedJoinOnto :: String -> String -> [String] -> [String]
        qualifiedJoinOnto closers before [] = [before]
        qualifiedJoinOnto closers before after
          | (elem (head $ head after) areaOpeners) && closers == "" = before:after
          | otherwise = [before++(head after)]++(tail after)

main = putStrLn $ show $ splitLine "!a_b.c!d:e-\"[{]a}]\"a.c+b"
