import           Disorder.Core.Main

import qualified Test.Piano.Parser

main :: IO ()
main =
  disorderMain [
      Test.Piano.Parser.tests
    ]
