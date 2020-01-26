import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.PetriNet

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PetriNet tests" [unitTests]

unitTests :: TestTree
unitTests =
  testGroup
    "PetriNet unit tests"
    [ testCase "pre / post" $ do
        let net =
              PetriNet
                { places = Set.fromList [Place 'A', Place 'B', Place 'C']
                , transitions =
                    Set.fromList
                      [Transition 'q', Transition 'p', Transition 'r']
                , flowRelations =
                    Set.fromList
                      [ PT (Place 'A') (Transition 'p')
                      , PT (Place 'A') (Transition 'q')
                      , PT (Place 'B') (Transition 'q')
                      , PT (Place 'B') (Transition 'r')
                      , TP (Transition 'p') (Place 'B')
                      , TP (Transition 'q') (Place 'A')
                      , TP (Transition 'q') (Place 'C')
                      , TP (Transition 'r') (Place 'A')
                      ]
                , marking = Map.fromList []
                , weights = Map.fromList []
                }
        pre (Transition 'p') net @?= Set.fromList [Place 'A']
        pre (Transition 'q') net @?= Set.fromList [Place 'A', Place 'B']
        pre (Transition 'r') net @?= Set.fromList [Place 'B']
        post (Transition 'p') net @?= Set.fromList [Place 'B']
        post (Transition 'q') net @?= Set.fromList [Place 'A', Place 'C']
        post (Transition 'r') net @?= Set.fromList [Place 'A']
    ]
