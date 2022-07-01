module ProjectSpec where

import Test.Hspec
import MessageEncoding
import MessageDecoding
import System.Random
import MessageUtils
import Constants

spec :: Spec
spec = do
  describe "Message encoding / decoding" $ do
    let msg = "MINESWEEPER"
    let stdGen = mkStdGen 42
    it "Check if decodeMessage . encodeMessage == id" $
      decodeMessage (encodeMessage stdGen msg) == msg
    it "Check if encodeMessage produces valid boards" $
      all (\b -> onesCount b == numberOfMines && length b == boardWidth * boardHeight) (encodeMessage stdGen msg)
