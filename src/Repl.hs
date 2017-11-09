{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Repl where 

import Control.Monad.Trans (liftIO)
import GHC.Generics (Generic)
import Data.Binary
import Data.Typeable
import Control.Monad (forever)
import Data.Maybe
import Text.Read (readMaybe)
import Control.Distributed.Process

import Types

data Command = Add Flower | Show  -- тип комманды
  deriving (Typeable, Generic, Read)  -- с помощью Read можно считывать тип комманды из REPL,
instance Binary Command               -- как будто это обычная команда

data Answer = Added Flower | HereUR FlowerList
  deriving (Typeable, Generic, Show)  -- аналогично с Command - просто выводим тип данных
instance Binary Answer                -- как будто это просто прикольный текст

runRepl :: ProcessId -> Process ()
runRepl parent = do
  liftIO $ putStrLn "Welcome, scout bee. Write 'Add (x,y)' to add a flower, 'Show' to list available flowers."
  forever $ do
    line <- liftIO $ getLine  -- считываем команду
    case ((readMaybe line) :: Maybe Command) of 
      Just command -> do  -- если пользователь ввел нормальную команду
        send parent command  -- отправляем ее главному процессу
        answer <- expect :: Process Answer  -- ожидаем результат
        liftIO $ print answer
      Nothing ->   -- пользователь ввел фигню и read не смог это прочитать
        liftIO $ putStrLn "No such command"
