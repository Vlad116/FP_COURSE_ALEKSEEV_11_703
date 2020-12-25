-- | Клиент для игры

{-# LANGUAGE DeriveFunctor #-}

module Main where

import Api
import Lib
import Types

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Free
import Data.Functor.Identity
import Text.Read

data Command next
  = PrintField next
  | UserAction (Maybe Move -> next)
  | MakeMove Move next
  deriving (Functor)

type Program = Free Command

printField :: Program ()
printField = liftF $ PrintField ()

getUserAction :: Program (Maybe Move)
getUserAction = liftF $ UserAction id

makeMove :: Move -> Program ()
makeMove move = liftF $ MakeMove move ()

realGetAction :: ClientM (Maybe Move)
realGetAction = do
  ans <- liftIO getLine
  case ans of
    "quit" -> pure Nothing
    "random" -> pure $ Just Random
    "no" -> pure $ Just NoGuess
    _ -> case readMaybe ans of
      Just x -> pure $ Just $ Guess x
      Nothing -> realGetAction

runRealProgram :: Program a -> ClientM a
runRealProgram (FreeT (Identity fa)) = case fa of
  Pure a -> pure a
  Free cmd -> case cmd of
    PrintField next -> do
      f <- getField
      liftIO $ do
        putStrLn $ "Score: " ++ show (corrects f)
        forM_ (oldCombinations f) $ \(c,a) -> do
          putStrLn $ show c ++ " -> " ++ show a
        print $ askedCombination f
      runRealProgram next
    UserAction next -> do
      ans <- realGetAction
      runRealProgram $ next ans
    MakeMove move next -> do
      postMove move
      runRealProgram next

--

gameClient :: Program ()
gameClient = forever $ do
  printField
  mmove <- getUserAction
  case mmove of
    Nothing -> pure ()
    Just move -> makeMove move

--

main :: IO ()
main = do
  let baseUrl = BaseUrl Http "localhost" 8081 ""
  res <- runClient baseUrl $ runRealProgram gameClient
  case res of
    Left err -> print err
    Right r -> pure ()
