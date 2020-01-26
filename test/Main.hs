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
    , testCase "enabled transitions / fire transitions" $ do
        let net =
              PetriNet
                { places =
                    Set.fromList
                      [Place 'A', Place 'B', Place 'C', Place 'D', Place 'E']
                , transitions =
                    Set.fromList
                      [ Transition 'q'
                      , Transition 'p'
                      , Transition 'r'
                      , Transition 's'
                      ]
                , flowRelations =
                    Set.fromList
                      [ PT (Place 'A') (Transition 'p')
                      , PT (Place 'B') (Transition 'q')
                      , PT (Place 'C') (Transition 'q')
                      , PT (Place 'C') (Transition 's')
                      , PT (Place 'D') (Transition 's')
                      , PT (Place 'E') (Transition 'r')
                      , TP (Transition 'p') (Place 'B')
                      , TP (Transition 'p') (Place 'C')
                      , TP (Transition 'q') (Place 'A')
                      , TP (Transition 'r') (Place 'C')
                      , TP (Transition 'r') (Place 'D')
                      , TP (Transition 's') (Place 'E')
                      ]
                , marking = Map.fromList [(Place 'A', 1), (Place 'E', 1)]
                , weights =
                    Map.fromList
                      [ (TP (Transition 'q') (Place 'A'), 2)
                      , (TP (Transition 's') (Place 'E'), 2)
                      ]
                }
        isEnabled (Transition 'p') net @?= True
        isEnabled (Transition 'q') net @?= False
        isEnabled (Transition 'r') net @?= True
        isEnabled (Transition 's') net @?= False
        fire (Transition 'q') net @?= net
        fire (Transition 's') net @?= net
        let net2 = fire (Transition 'p') net
            net3 = fire (Transition 'q') net2
            net4 = fire (Transition 'p') net3
        marking net2 @?=
          Map.fromList
            [(Place 'A', 0), (Place 'B', 1), (Place 'C', 1), (Place 'E', 1)]
        fireSequence "pqp" net @?= net4
    , testCase "accepted words" $ do
        let net =
              PetriNet
                { places =
                    Set.fromList
                      [Place 'A', Place 'B', Place 'C', Place 'D', Place 'E']
                , transitions =
                    Set.fromList
                      [ Transition 'q'
                      , Transition 'p'
                      , Transition 'r'
                      , Transition 's'
                      ]
                , flowRelations =
                    Set.fromList
                      [ PT (Place 'A') (Transition 'p')
                      , PT (Place 'B') (Transition 'q')
                      , PT (Place 'C') (Transition 'q')
                      , PT (Place 'C') (Transition 's')
                      , PT (Place 'D') (Transition 's')
                      , PT (Place 'E') (Transition 'r')
                      , TP (Transition 'p') (Place 'B')
                      , TP (Transition 'p') (Place 'C')
                      , TP (Transition 'q') (Place 'A')
                      , TP (Transition 'r') (Place 'C')
                      , TP (Transition 'r') (Place 'D')
                      , TP (Transition 's') (Place 'E')
                      ]
                , marking = Map.fromList [(Place 'A', 1), (Place 'E', 1)]
                , weights =
                    Map.fromList
                      [ (TP (Transition 'q') (Place 'A'), 2)
                      , (TP (Transition 's') (Place 'E'), 2)
                      ]
                }
        isAcceptedWord ['p', 'q', 'p', 'p'] net @?= True
        isAcceptedWord ['p', 'q', 'p', 'p', 'p'] net @?= False
        isAcceptedWord ['r', 'p', 's', 'q'] net @?= True
    ]
