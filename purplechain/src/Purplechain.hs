module Purplechain
  ( module Purplechain.Client
  , module Purplechain.Network
  , module Purplechain.Node
  , main
  ) where

import Purplechain.Client
import Purplechain.Network
import Purplechain.Node

main :: IO ()
main = testNetwork
