{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Day7.Day7 where

import           Control.Monad.Memo    (Memo, for2, memo)
import           Data.Bits
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import           Data.Word             (Word16)
import Data.Text (Text)

newtype Label = Label Text deriving (Show, Ord, Eq)

data Circuit = Signal Word16
             | Wire Label
             | AndGate Circuit Circuit
             | OrGate Circuit Circuit
             | LShiftGate Int Circuit
             | RShiftGate Int Circuit
             | NotGate Circuit deriving (Eq, Show, Ord)


type CircuitBox = M.Map Label Circuit

runCircuit :: CircuitBox -> Circuit -> Memo (CircuitBox, Circuit) Word16 Word16
runCircuit b (Signal a) = return a
runCircuit b (Wire a) = for2 memo runCircuit b $ (M.!) b a
runCircuit b (AndGate r l) = (.&.) <$> for2 memo runCircuit b r <*> for2 memo runCircuit b l
runCircuit b (OrGate r l) =  (.|.) <$> for2 memo runCircuit b r <*> for2 memo runCircuit b l
runCircuit b (LShiftGate i a) = flip shiftL i <$> for2 memo runCircuit b a
runCircuit b (RShiftGate i a) = flip shiftR i <$> for2 memo runCircuit b a
runCircuit b (NotGate a) = complement <$> for2 memo runCircuit b a
