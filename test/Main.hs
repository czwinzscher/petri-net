import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.SmallCheck

import Data.Bits
import qualified Data.Map as Map
import qualified Data.Set as Set

import Data.PetriNet

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "PetriNet tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests =
  testGroup
    "PetriNet unit tests"
    [ testCase "pre / post" $ do
        let net =
              PetriNet
                { places = Set.fromList ['A', 'B', 'C']
                , transitions = Set.fromList ['q', 'p', 'r']
                , flowRelations =
                    Set.fromList
                      [ PT 'A' 'p'
                      , PT 'A' 'q'
                      , PT 'B' 'q'
                      , PT 'B' 'r'
                      , TP 'p' 'B'
                      , TP 'q' 'A'
                      , TP 'q' 'C'
                      , TP 'r' 'A'
                      ]
                , marking = Map.fromList []
                , weights = Map.fromList []
                }
        pre 'p' net @?= Set.fromList ['A']
        pre 'q' net @?= Set.fromList ['A', 'B']
        pre 'r' net @?= Set.fromList ['B']
        post 'p' net @?= Set.fromList ['B']
        post 'q' net @?= Set.fromList ['A', 'C']
        post 'r' net @?= Set.fromList ['A']
    , testCase "enabled transitions / fire transitions" $ do
        let net =
              PetriNet
                { places = Set.fromList ['A', 'B', 'C', 'D', 'E']
                , transitions = Set.fromList ['q', 'p', 'r', 's']
                , flowRelations =
                    Set.fromList
                      [ PT 'A' 'p'
                      , PT 'B' 'q'
                      , PT 'C' 'q'
                      , PT 'C' 's'
                      , PT 'D' 's'
                      , PT 'E' 'r'
                      , TP 'p' 'B'
                      , TP 'p' 'C'
                      , TP 'q' 'A'
                      , TP 'r' 'C'
                      , TP 'r' 'D'
                      , TP 's' 'E'
                      ]
                , marking = Map.fromList [('A', 1), ('E', 1)]
                , weights = Map.fromList [(TP 'q' 'A', 2), (TP 's' 'E', 2)]
                }
        isEnabled 'p' net @?= True
        isEnabled 'q' net @?= False
        isEnabled 'r' net @?= True
        isEnabled 's' net @?= False
        fire 'q' net @?= net
        fire 's' net @?= net
        let net2 = fire 'p' net
            net3 = fire 'q' net2
            net4 = fire 'p' net3
        marking net2 @?= Map.fromList [('A', 0), ('B', 1), ('C', 1), ('E', 1)]
        fireSequence "pqp" net @?= net4
    , testCase "accepted words" $ do
        let net =
              PetriNet
                { places = Set.fromList ['A', 'B', 'C', 'D', 'E']
                , transitions = Set.fromList ['q', 'p', 'r', 's']
                , flowRelations =
                    Set.fromList
                      [ PT 'A' 'p'
                      , PT 'B' 'q'
                      , PT 'C' 'q'
                      , PT 'C' 's'
                      , PT 'D' 's'
                      , PT 'E' 'r'
                      , TP 'p' 'B'
                      , TP 'p' 'C'
                      , TP 'q' 'A'
                      , TP 'r' 'C'
                      , TP 'r' 'D'
                      , TP 's' 'E'
                      ]
                , marking = Map.fromList [('A', 1), ('E', 1)]
                , weights = Map.fromList [(TP 'q' 'A', 2), (TP 's' 'E', 2)]
                }
        isAcceptedWord ['p', 'q', 'p', 'p'] net @?= True
        isAcceptedWord ['p', 'q', 'p', 'p', 'p'] net @?= False
        isAcceptedWord ['r', 'p', 's', 'q'] net @?= True
    ]

propertyTests :: TestTree
propertyTests =
  testGroup
    "petri net property tests"
    [ testProperty "or" $ do
        let net =
              PetriNet
                { places = Set.fromList ['A', 'B', 'C']
                , transitions = Set.fromList ['a', 'b', 'c']
                , flowRelations =
                    Set.fromList
                      [ PT 'A' 'a'
                      , PT 'A' 'a'
                      , PT 'A' 'a'
                      , TP 'a' 'B'
                      , TP 'b' 'B'
                      , TP 'b' 'C'
                      , TP 'c' 'C'
                      ]
                , marking = Map.fromList [('A', 1)]
                , weights = Map.empty
                }
        \t ->
          if isAcceptedWord [t] net
            then let net2 = fire t net
                  in marks 'B' net2 == 1 || marks 'C' net2 == 1
            else True
    , testProperty "xor" $ do
        let net =
              PetriNet
                { places = Set.fromList ['A', 'B', 'C']
                , transitions = Set.fromList ['a', 'b']
                , flowRelations =
                    Set.fromList
                      [PT 'A' 'a', PT 'A' 'b', TP 'a' 'B', TP 'b' 'C']
                , marking = Map.fromList [('A', 1)]
                , weights = Map.empty
                }
        \t ->
          if isAcceptedWord [t] net
            then let net2 = fire t net
                  in (marks 'B' net2 == 1) `xor` (marks 'C' net2 == 1)
            else True
    ]
