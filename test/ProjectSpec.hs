module ProjectSpec where

import Test.Hspec
import MessageEncoding
import MessageDecoding
import System.Random

spec :: Spec
spec = do
  describe "Message operations" $ do
    let msg = "MINESWEEPER"
    it "Check if decodeMessage . encodeMessage == id" $
      decodeMessage (encodeMessage (mkStdGen 42) msg) == msg
