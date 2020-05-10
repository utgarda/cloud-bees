{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Chain
-- (
--     runChain,
--     CryptoCommand,
--     MineGenesis
-- ) 
where

import Control.Distributed.Process
import Control.Monad (forever)
import GHC.Generics (Generic)

import Data.Binary
import Data.Maybe
import Data.Typeable

import Data.Blockchain
import Data.Blockchain.Mining
import Data.Blockchain.Types

config :: BlockchainConfig
config = defaultConfig

genesis :: IO Block
genesis = mineGenesisBlock config
    
data CryptoCommand 
    = MineGenesis
  deriving (Typeable, Generic, Show)

instance Binary CryptoCommand

runChain :: ProcessId -> Process ()
runChain parent = do
  forever $ do
    command <- expect :: Process CryptoCommand
    case (command :: CryptoCommand) of
      MineGenesis
       -> do
        block <- liftIO genesis
        liftIO $ print block
