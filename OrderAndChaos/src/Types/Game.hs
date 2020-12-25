{-# LANGUAGE DeriveGeneric #-}
module Types.Game where

import GHC.Generics
import Data.Aeson(FromJSON, ToJSON)

data Glyph = X | O deriving (Eq, Show, Read, Generic)

instance FromJSON Glyph
instance ToJSON Glyph

type LocalGrid = [[Maybe Glyph]]

type Grid = [[LocalGrid]]

data Move = Move {
  glyph :: Glyph,
  cell :: (Int, Int)
} deriving (Eq, Show, Read, Generic)

instance FromJSON Move
instance ToJSON Move

data GameState = GameState
  { moveCount :: Int,
    grid :: Grid,
    lastMove :: Move
  } deriving (Eq, Show, Read, Generic)

instance FromJSON GameState
instance ToJSON GameState