{-# LANGUAGE FlexibleInstances #-}

module Slim.SlimServer where

import           Control.Arrow
import           Control.Concurrent.Async
import           Control.Monad.State      hiding (void)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Char8    as BS8
import           Data.ByteString.UTF8     (fromString)
import qualified Data.Map                 as M
import           Data.Monoid              ((<>))
import           Network.Socket
import           Slim.Slim
import           Slim.SlimClientIO
import           System.IO

-- |Starts a slim server on given port and returns this port
startSlimServer :: Int -> IO (Async ())
startSlimServer = async . doServe

doServe :: Int -> IO ()
doServe port =
  do address <- inet_addr "127.0.0.1"
     sock <- socket AF_INET Stream defaultProtocol
     let addr = SockAddrInet (fromIntegral port :: PortNumber) address
     bind sock addr
     listen sock 5
     putStrLn $ "started slim server on port "++ show port
     loopOn sock

loopOn :: Socket -> IO ()
loopOn sock = withFile ".hslimserver.log" AppendMode $
              \ logFile -> do hSetBuffering logFile LineBuffering
                              (client, clientAddress) <- accept sock
                              hPutStrLn logFile $ "connection from " ++ (show clientAddress)
                              answerRequest logFile client
                              close client
                              return ()  -- should be a loop

answerRequest :: Handle -> Socket -> IO ()
answerRequest logFile sock = do hdl <- socketToHandle sock ReadWriteMode
                                hSetBuffering hdl (BlockBuffering Nothing)
                                hPutStrLn logFile "sending version"
                                _ <- BS.hPutStr hdl (fromString "Slim -- V0.3\n")
                                hFlush hdl
                                count <- countAnswerCharsLog logFile hdl
                                calls <- readAnswer hdl count
                                let Just calls' = (decode calls) :: Maybe [Instruction String]
                                hPutStrLn logFile $ "Received: " <> show calls'
                                -- state is not generic
                                let st = M.empty :: M.Map String Div
                                let answer =  invoke calls' st
                                hPutStrLn logFile $ "Sending: " <> show answer
                                BS8.hPutStrLn hdl ((fromString . encode) answer)
                                hFlush hdl
                                return ()

class Invokable i where
  call :: Instruction String -> State i Answer
  make :: [ String ] -> i

instance (Invokable i) => Invokable (M.Map String i) where
  call c@(Call _x instanc _t _a) = do m <- get
                                      let (result, st) = tryCall c m
                                      case st of
                                        Nothing -> put m
                                        Just s' -> put $ M.insert instanc s' m
                                      return $ result
  call (Make x  n _ p) = do m <- get
                            put $ M.insert n (make p) m
                            return $ ok x
  call _ = pure $ A (S "OK")

  make _ = M.empty

tryCall :: Invokable s => Instruction String -> M.Map Instance s -> (Answer, Maybe s)
tryCall c@(Call x instanc _t _a) objects = case M.lookup instanc objects  of
  Nothing  -> (exception x ***  id) ( "NO_INSTANCE " ++ instanc, Nothing)
  Just obj -> (id *** Just) $ runState (call c) obj
tryCall c _ = error $ "Cannot invoke " <> show c

invoke :: (Invokable s) => [ Instruction String ] -> s -> Answer
invoke calls = answers . evalState (sequence $ map call calls)

answers :: [ Answer ] -> Answer
answers as = A $ L (answers' as)
  where
    answers' []           = []
    answers' (A a : rest) = a : answers' rest
    answers' (e:_)        = error $ "Cannot build answer with error " <> show e

void :: String -> Answer
void x = A (L [ S x, S "/__VOID__/"])

exception :: String -> [Char] -> Answer
exception x m = A (L [ S x, S ("__EXCEPTION__: " ++ m)])

ok :: String -> Answer
ok   x = A ( L [S x , S "OK"] )

val :: (Show v) => String -> v -> Answer
val  x v = A ( L [S x , S $ show v] )

-- |Division data-type, used for testing purpose much like the classical eg.Division
-- from standard fitnesse documentation.
-- It should be possible to generate a lot of stuff using Template Haskell...

data Div = Div { numerator   :: Double,
                 denominator :: Double }
         deriving (Eq,Show)

setNumerator :: Double -> Div -> Div
setNumerator   d division = division { numerator   = d }

setDenominator :: Double -> Div -> Div
setDenominator d division = division { denominator = d }

getNumerator :: Div -> Double
getNumerator         = numerator

getDenominator :: Div -> Double
getDenominator       = denominator

quotient :: Div -> Double
quotient  d          = numerator d / denominator d

readDouble :: String -> Double
readDouble s = x where (x,_):_ = (reads :: String -> [(Double,String)]) s

setter :: MonadState s m => String -> (Double -> s -> s) -> String -> m Answer
setter x f s = get >>= (put . f (readDouble s)) >> (return $ void x)

getter :: (MonadState a m, Show v) => String -> (a -> v) -> m Answer
getter x f = get >>= return . val x . f

instance Invokable Div where
  call (Call x _n "setNumerator" [ s ])   = setter x setNumerator s
  call (Call x _n "setDenominator" [ s ]) = setter x setDenominator s
  call (Call x _n "getNumerator" [])   = getter x getNumerator
  call (Call x _n "getDenominator" []) = getter x getDenominator
  call (Call x  _n "quotient" [])         = get >>= return . val x . quotient
  -- The next 5 are standard decision tables API calls we can ignore
  call (Call _ _ "endTable" _)                   = return $ ok ""
  call (Call _ _ "beginTable" _)                   = return $ ok ""
  call (Call _ _ "table" _)                   = return $ ok ""
  call (Call _ _ "execute" _)                   = return $ ok ""
  call (Call _ _ "reset" _)                   = return $ ok ""
  call (Call x n m _)                   = return $ exception x ("NO_METHOD_IN_CLASS " ++ m ++ " " ++ n)
  call (Make x  _n "eg.Division" [])      = do put $ Div 0 0
                                               return $ ok x
  call c                                 = error $ "Cannot call " <> show c <> " on Div invokable"

  make _                                 = Div 0 0
