module Logic where

import Datatype
import Utility

openCell :: Coords -> Board -> Board
openCell (x, y) = updateAt y (updateAt x processCell)
  where
    processCell (Cell content Closed) = Cell content Opened
    processCell cell = cell


