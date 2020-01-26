{-# LANGUAGE RecordWildCards #-}

module Data.PetriNet where

import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set

newtype Place a =
  Place a
  deriving (Eq, Ord, Show)

newtype Transition a =
  Transition a
  deriving (Eq, Ord, Show)

data Edge a b
  = PT (Place a) (Transition b)
  | TP (Transition b) (Place a)
  deriving (Eq, Ord, Show)

data PetriNet a b =
  PetriNet
    { places :: Set.Set (Place a)
    , transitions :: Set.Set (Transition b)
    , flowRelations :: Set.Set (Edge a b)
    , marking :: Map.Map (Place a) Int
    , weights :: Map.Map (Edge a b) Int
    }
  deriving (Eq, Ord, Show)

empty :: PetriNet a b
empty =
  PetriNet
    { places = Set.empty
    , transitions = Set.empty
    , flowRelations = Set.empty
    , marking = Map.empty
    , weights = Map.empty
    }

hasPlace :: Ord a => Place a -> PetriNet a b -> Bool
hasPlace pl net = Set.member pl (places net)

addPlace :: Ord a => Place a -> PetriNet a b -> PetriNet a b
addPlace pl net@PetriNet {..} = net {places = Set.insert pl places}

hasTransition :: Ord b => Transition b -> PetriNet a b -> Bool
hasTransition tr net = Set.member tr (transitions net)

addTransition :: Ord b => Transition b -> PetriNet a b -> PetriNet a b
addTransition tr net@PetriNet {..} =
  net {transitions = Set.insert tr transitions}

hasFlowRelation :: (Ord a, Ord b) => Edge a b -> PetriNet a b -> Bool
hasFlowRelation e net = Set.member e (flowRelations net)

addFlowRelation :: (Ord a, Ord b) => Edge a b -> PetriNet a b -> PetriNet a b
addFlowRelation e net@PetriNet {..} =
  let allowed =
        case e of
          PT pl tr -> hasPlace pl net && hasTransition tr net
          TP tr pl -> hasTransition tr net && hasPlace pl net
   in net
        { flowRelations =
            if allowed
              then Set.insert e flowRelations
              else flowRelations
        , weights =
            if allowed
              then case Map.lookup e weights of
                     Just _ -> Map.adjust (1 +) e weights
                     Nothing -> Map.insert e 1 weights
              else weights
        }

setMarks :: (Ord a, Ord b) => Place a -> Int -> PetriNet a b -> PetriNet a b
setMarks pl n net@PetriNet {..} =
  if hasPlace pl net
    then net {marking = Map.insert pl n marking}
    else net

marks :: (Ord a, Ord b) => a -> PetriNet a b -> Int
marks p net = fromMaybe 0 (Map.lookup (Place p) (marking net))

weight :: (Ord a, Ord b) => Edge a b -> PetriNet a b -> Int
weight e net =
  if hasFlowRelation e net
    then fromMaybe 1 (Map.lookup e (weights net))
    else 0

union :: (Ord a, Ord b) => PetriNet a b -> PetriNet a b -> PetriNet a b
union p1 p2 =
  PetriNet
    { places = Set.union (places p1) (places p2)
    , transitions = Set.union (transitions p1) (transitions p2)
    , flowRelations = Set.union (flowRelations p1) (flowRelations p2)
    , marking = Map.unionWith (+) (marking p1) (marking p2)
    , weights = Map.unionWith (+) (weights p1) (weights p2)
    }

pre :: (Ord a, Ord b) => Transition b -> PetriNet a b -> Set.Set (Place a)
pre tr PetriNet {..} =
  foldr
    (\v acc ->
       if Set.member (PT v tr) flowRelations
         then Set.insert v acc
         else acc)
    Set.empty
    places

post :: (Ord a, Ord b) => Transition b -> PetriNet a b -> Set.Set (Place a)
post tr PetriNet {..} =
  foldr
    (\v acc ->
       if Set.member (TP tr v) flowRelations
         then Set.insert v acc
         else acc)
    Set.empty
    places

isEnabled :: (Ord a, Ord b) => Transition b -> PetriNet a b -> Bool
isEnabled tr net@PetriNet {..} =
  let valid [] = True
      valid (p:ps) =
        let w = weight (PT p tr) net
         in case Map.lookup p marking of
              Just n -> w <= n && valid ps
              Nothing -> w == 0 && valid ps
   in valid (Set.toList $ pre tr net)

fire :: (Ord a, Ord b) => Transition b -> PetriNet a b -> PetriNet a b
fire tr net@PetriNet {..} =
  if isEnabled tr net
    then let updatePre m =
               foldr
                 (\p -> Map.adjust (\v -> v - weight (PT p tr) net) p)
                 m
                 (pre tr net)
             updatePost m =
               foldr
                 (\p acc ->
                    let w = weight (TP tr p) net
                     in case Map.lookup p acc of
                          Just _ -> Map.adjust (w +) p acc
                          Nothing -> Map.insert p w acc)
                 m
                 (post tr net)
          in net {marking = (updatePost . updatePre) marking}
    else net

fireSequence :: (Ord a, Ord b) => [b] -> PetriNet a b -> PetriNet a b
fireSequence trs net = foldl (\acc t -> fire (Transition t) acc) net trs

isAcceptedWord :: (Ord a, Ord b) => [b] -> PetriNet a b -> Bool
isAcceptedWord [] _ = True
isAcceptedWord (t:ts) net =
  if isEnabled (Transition t) net
    then isAcceptedWord ts (fire (Transition t) net)
    else False
