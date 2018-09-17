{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Slim.SlimClientIO where

import           Control.Concurrent
import           Control.Monad.State
import           Data.ByteString      (hGet, hPutStr)
import           Data.ByteString.UTF8 (fromString, toString)
import           Data.Maybe
import           Network.BSD
import           Network.Socket
import           Slim.Slim
import           Slim.SlimClient
import           System.Info
import           System.IO            (BufferMode (..), Handle, IOMode (..),
                                       hClose, hFlush, hGetLine, hSetBuffering,
                                       stdout)
import qualified System.IO            as IO
import           System.Process
import           Text.Printf

data IOSlimState = IOSlimState { slimHandle :: Maybe ProcessHandle
                               , config     :: SlimConfig }

instance SlimState IOSlimState where
  slimConfig = config

-- | Connect to the given host/port
connectTo :: String -> Integer -> IO Handle
connectTo host port = do address <- inet_addr host
                         sock <- socket AF_INET Stream defaultProtocol
                         setSocketOption sock KeepAlive 1
                         connect sock (SockAddrInet ((fromIntegral port) :: PortNumber) address)
                         socketToHandle sock ReadWriteMode

countAnswerChars :: Handle -> IO Int
countAnswerChars = countAnswerCharsLog stdout

countAnswerCharsLog :: Handle ->  Handle -> IO Int
countAnswerCharsLog loggerHandle h = do
  c <- hGet h 6
  IO.hPutStrLn loggerHandle $ "counting answer chars " ++ (toString c)
  return $ readInt (toString c)

readAnswer :: Handle -> Int -> IO String
readAnswer h n = hGet h (n+1) >>= return . (( printf "%06d" n) ++) . toString

sendToSlimAndClose :: Int -> [Instruction String] -> IO Answer
sendToSlimAndClose port insts = do cnx <- connectTo "127.0.0.1" (toInteger port)
                                   hSetBuffering cnx (BlockBuffering Nothing)
                                   _ <- hGetLine cnx -- What comes here? I guess we should verify version...
                                   hPutStr cnx ((fromString . encode) insts <> "\n")
                                   hFlush cnx
                                   returns <- (countAnswerChars cnx >>= readAnswer cnx)
                                   hClose cnx
                                   let Just answer = decode  returns
                                   return answer


instance SlimIO IO IOSlimState where
    ioTerminate st = do liftIO $ putStrLn "Waiting for termination of Slim"
                        liftIO $ waitForProcess . fromJust . slimHandle $ st

    fetchAnswers st msgs = liftIO $! do let port = slimport $ slimConfig st
                                        -- connect using low-level Network.Socket
                                        cnx <- connectTo "127.0.0.1" port
                                        hSetBuffering cnx (BlockBuffering Nothing)
                                        _  <- hGetLine cnx -- TODO check input
                                        let calls = (fromString msgs)
                                        hPutStr cnx (calls <> "\n")
                                        if (verbose $ slimConfig st) then putStrLn $ toString calls else return ()
                                        hFlush cnx
                                        returns <- (countAnswerChars cnx >>= readAnswer cnx)
                                        if (verbose $ slimConfig st) then putStrLn returns else return ()
                                        return returns


    doStartSlim initialState = do pr <- liftIO $ runProcess  exe ["-cp", classpath, "fitnesse.slim.SlimService", port] wd Nothing Nothing Nothing Nothing
                                  liftIO $ threadDelay 500000
                                  modify $ \ st -> st { slimHandle = Just pr }
        where
          currentConfig = slimConfig initialState
          exe           = slimexecutable currentConfig
          wd            = Just $ executiondir currentConfig
          port          = show (slimport currentConfig)
          pathSeparator = case os of
                            "windows" -> ";"
                            "mingw32" -> ";"
                            _         -> ":"
          classpath     = foldl (\ x y -> x ++ pathSeparator ++ y) "." (slimclasspath currentConfig)


defaultSlim :: IOSlimState
defaultSlim   = IOSlimState  Nothing defaultConfig
