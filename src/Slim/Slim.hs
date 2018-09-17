{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE IncoherentInstances  #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Slim.Slim where

import           Data.Functor (void)
import           Data.List
import           Debug.Trace
import           Text.Parsec
import           Text.Printf

data Instruction ident = Import ident PackName
                       | Make  ident Instance Class  [ Argument ]
                       | Call  ident Instance Method [ Argument ]
                       | CallAndAssign  ident Instance Symbol Method [ Argument ]
                       deriving (Eq,Show, Read)

type PackName     = String
type Instance = String
type Class    = String
type Method   = String
type Symbol   = String
type Argument = String
data Answer = A NestedList
            | KO String
           deriving (Eq, Show)
data NestedList = S String
                | L [ NestedList ]
                        deriving (Eq,Show)


class SlimEncodable a where
  encode :: a -> String


aLength :: [a] -> String
aLength l = printf "%06d" (length l)


instance SlimEncodable String where
  encode s = aLength s ++ ":" ++  s


instance SlimEncodable NestedList where
   encode (S s) = encode s
   encode (L l) = encode s
        where
          s = concat [
            "["
            , aLength l
            , ":"
            , (concatMap (( ++ ":") . encode) l)
            , "]"
            ]

instance SlimEncodable Answer where
  encode (A l)  = encode l
  encode (KO s) = encode ("__EXCEPTION__:" ++ s)

class SlimDecodable a where
  decode :: String -> Maybe a


instance SlimDecodable NestedList where
  decode s = case (runParser slimparser () "" s) of
               Left e   -> trace (show e) Nothing
               Right ts -> Just ts


readInt :: String -> Int
readInt s = case reads s of
              (x,_):_ -> x
              e       -> error $ "cannot read int from " ++ show e


slimparser :: Parsec String () NestedList
slimparser  =   do len <- lengthOfData ; char' ':'
                   (listParser <|> stringParser len)
            where
              contentOfLength n = count n anyChar
              lengthOfData      = fmap readInt (count 6 digit)
              manyStrings n     = count n (do nest <- slimparser; char' ':'
                                              return nest)
              stringParser len  = fmap S (contentOfLength len)

              char' = void . char

              listParser        = do char' '['
                                     size    <- lengthOfData     ; char' ':'
                                     strings <- manyStrings size ; char' ']'
                                     return $ L strings

instance (SlimEncodable a) => SlimEncodable [ a ] where
  encode l  = encode' l ""
    where
      encode' []     li@(_:_) = encode (li ++ "]")
      encode' (x:xs) l'@(_:_) = encode' xs (l' ++ encode x ++ ":")
      encode' l'        []    = encode' l' ("["
                                  ++ aLength l'
                                  ++ ":")


instance SlimEncodable (Instruction String) where
  encode (Import ident path)
       = encode [ ident , "import", path ]
  encode (Make ident inst cls args)
       = encode ( [ ident , "make", inst, cls ] ++ args)
  encode (Call ident inst meth args)
       = encode ( [ ident , "call", inst, meth ] ++ args)
  encode (CallAndAssign ident symbol inst meth args)
       = encode ( [ ident , "callAndAssign", symbol, inst, meth ] ++  args)

instance SlimDecodable [Instruction String] where
  decode s = let
               nest = decode s  :: Maybe NestedList

               fromNestedList :: [NestedList] -> [ String ]
               fromNestedList ((S s' : rest)) = s' : (fromNestedList rest)
               fromNestedList (_:rest)        = fromNestedList rest
               fromNestedList []              = []

               decode' x@(L [S ident, S "import", S pack]) = trace (show x) (Import ident pack)
               decode' x@(L (S ident : S "make" : S inst:  S cls : args)) = trace (show x) (Make ident inst cls (fromNestedList args))
               decode' x@(L (S ident : S "call" : S inst:  S cls : args)) = trace (show x) (Call ident inst cls (fromNestedList args))
               decode' x@(L (S ident : S "callAndAssign" : S inst: S var :  S cls : args)) = trace (show x) (CallAndAssign ident inst var cls (fromNestedList args))
               decode' x    = trace (show x) (Import "" "")

             in case nest of
               Just (L insts) -> Just (map decode' insts)
               _              -> Nothing

instance SlimDecodable Answer where
  decode s = let
               nest = decode s  :: Maybe NestedList
             in case nest of
                 Just (S ('_':'_':'E':'X':'C':'E':'P':'T':'I':'O':'N':'_':'_':':': message ))
                    -> Just $ KO message
                 Just r -> Just $ A r
                 Nothing -> Nothing

idOf :: Instruction String -> String
idOf (Import         ident _)       = ident
idOf (Make           ident _ _ _)   = ident
idOf (Call           ident _ _ _)   = ident
idOf (CallAndAssign  ident _ _ _ _) = ident

matchQandA :: [Instruction String] -> Maybe Answer -> [ (Instruction String, Answer)]
matchQandA insts  Nothing          = map (\ i -> (i , KO "No answer from client")) insts
matchQandA insts (Just e@(KO _))   = map (\ i -> (i ,e)) insts
matchQandA insts (Just (A (L as))) = [(q, a) | q <- insts,
                                                  a <- matchOneAnswer as q]
matchQandA _     _                 = []

matchOneAnswer :: [ NestedList ] -> Instruction String -> [Answer]
matchOneAnswer ((L [S num, rep]): as) q
  | idOf q == num = [A rep]
  | otherwise     = matchOneAnswer as q
matchOneAnswer _                      _ = [KO "No answer from client"]

renumber ::
  [ Instruction String ] -> [ Instruction String]
renumber insts = renumberFrom 1 insts
    where
      renumberFrom n (x:xs) = setId x n : renumberFrom (n+1) xs
      renumberFrom _ []     = []
      setId :: Instruction a -> Int -> Instruction String
      setId  (Make _ ident cls pars)               n = Make (show n) ident cls pars
      setId (Call _ ident ope pars)                n = Call (show n) ident ope pars
      setId (CallAndAssign _ ident lbl ope pars) n = CallAndAssign (show n) ident lbl ope pars
      setId (Import _ scope)                    n = Import (show n) scope

isException ::
  String -> Answer -> Bool
isException exc (A (S msg)) = isPrefixOf ("__EXCEPTION__:" ++ exc) msg
isException _   _           = False

maybeAnInt :: String -> Maybe Int
maybeAnInt s = case (reads :: String -> [(Int,String)]) s of
                 (x,_):_ -> Just x
                 _       -> Nothing
