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
runFunc x y = putStrLn "" --TODO: get function and run it

lineEnders = ";,"
inlineSymbols = "`~!@#$%^&*-+=|:<>/?"
priorityInlineSymbols = "._"
areaOpeners = "{[(\"'"
areaClosers = "}])\"'"

hasOpener :: String -> Bool
hasOpener "" = False
hasOpener str = elem (head str) areaOpeners

doubleHead :: [a] -> [a]
doubleHead (x:y:_) = [x,y]
doubleHead [x] = [x]
doubleHead [] = []

doubleTail :: [a] -> [a]
doubleTail (_:_:x) = x
doubleTail _ = []

splitSymbols :: [String] -> [String] --split away symbols: ["a+b"] -> ["a","+","b"]; strings starting with areaOpeners are ignored; priorityInlineSymbols are ignored
splitSymbols line = foldr (\x t -> f x ++ t) [] line where
  f :: String -> [String]
  f part
    | (part == "") || (hasOpener part) = [part]
    | otherwise = S.split (S.dropBlanks $ S.oneOf inlineSymbols) part

splitIntoLines :: [String] -> [[String]]
splitIntoLines strings = foldr (\x t -> f x t) [] strings where
  addToLines :: [String] -> [[String]] -> [[String]]
  addToLines [] acc = acc
  addToLines [""] acc = acc
  addToLines [str] acc@(h:t)
    | elem (last str) lineEnders = [str]:acc
    | otherwise = (str:h):t
  addToLines str@(h1:t1) acc = addToLines [h1] $ addToLines t1 acc
  f :: String -> [[String]] -> [[String]]
  f str [] = foldr (\x t -> [x]:t) []
    (S.split (S.dropInitBlank $ S.dropFinalBlank $ S.dropInnerBlanks $ S.keepDelimsR $ S.oneOf lineEnders) str)
  f str acc@(h:t)
    | (str == "") || (hasOpener str) = (str:h):t
    | otherwise = addToLines (S.split (S.keepDelimsR $ S.oneOf lineEnders) str) acc

separateEnclosedAreas :: [Char] -> String -> [String] --separate areas enclosed by {} [] () "" or ''
separateEnclosedAreas closers area --closers are the symbols that are needed to close the areas: '{'->'}' etc; they stack, with newer ones at the start
  | area == "" = []
  | head area == '\\' = qualifiedJoinOnto closers (doubleHead area) (separateEnclosedAreas closers (doubleTail area))
  | (hasOpener area) && ((closers == "") || head closers /= respectiveCloser) =
    joinOnto (head area) (separateEnclosedAreas (respectiveCloser:closers) (tail area))
  | (closers == "") || (head area /= head closers) = qualifiedJoinOnto closers [head area] (separateEnclosedAreas closers (tail area))
  | otherwise = joinAreaEnd closers (head area) (separateEnclosedAreas (tail closers) (tail area))
    where
      respectiveCloser = areaClosers !! unMaybe (L.elemIndex (head area) areaOpeners) 0

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
