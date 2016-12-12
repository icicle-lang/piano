import           Disorder.Core.Main

import qualified Test.Piano.Data
import qualified Test.Piano.Foreign
import qualified Test.Piano.Parser

main :: IO ()
main =
  disorderMain [
      Test.Piano.Data.tests
    , Test.Piano.Foreign.tests
    , Test.Piano.Parser.tests
    ]
