{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Chain
(
    runChain,
    CryptoCommand(..),
    CryptoEvent(..)
) 
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

data CryptoEvent 
    = Mined String
  deriving (Typeable, Generic, Show)

instance Binary CryptoEvent

runChain :: ProcessId -> Process ()
runChain parent = do
  forever $ do
    command <- expect :: Process CryptoCommand
    case (command :: CryptoCommand) of
      MineGenesis
       -> do
        liftIO $ print "mining block"
        block <- liftIO genesis
        send parent $ Mined $ show block --TODO update state 
