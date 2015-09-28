
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Text.Read (readMaybe)
import Network.Simple.TCP (connect)
import Control.Exception (catch, SomeException)
import System.Environment (getArgs)

data Option =
    Port Int
  | WaitFor Status

data Status = Listening | Closed deriving Eq

data Config = Config
  { port :: Int
  , waitFor :: Status
    }

parseOptions :: [String] -> [Option]
parseOptions = go
  where
    go ("port":strp:opts) =
      case readMaybe strp of
        Just p -> Port p : parseOptions opts
        _ -> error $ "Failed reading port: " ++ strp
    go ("until":strst:opts) =
      case strst of
        "listening" -> WaitFor Listening : parseOptions opts
        "closed" -> WaitFor Closed : parseOptions opts
        _ -> error $ "Unrecognized status: " ++ strst
    go (opt:_) = error $ "Unknown option: " ++ opt

makeConfig :: [Option] -> Config
makeConfig = go $ Config (error "Undefined port") (error "Undefined status")
  where
    go c (Port p:opts) = go (c { port = p }) opts
    go c (WaitFor st:opts) = go (c { waitFor = st }) opts

checkPort :: Int -> IO Bool
checkPort p =
  connect "127.0.0.1" (show p) (\_ -> return True)
    `catch` \(_ :: SomeException) -> return False

main :: IO ()
main = do
  conf <- makeConfig . parseOptions <$> getArgs
  let loop = do
        isListening <- checkPort $ port conf
        if isListening == (waitFor conf == Listening)
           then loop
           else return ()
  loop
