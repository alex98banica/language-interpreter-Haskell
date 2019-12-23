module ClassState
where
import Data.Map (Map)
import qualified Data.Map as Map

data InstrType = Var | Func  deriving (Show, Eq ,Ord)

data ClassState = ClassState(Map [String] InstrType) deriving (Show ,Eq ,Ord)

initEmptyClass :: ClassState
initEmptyClass = ClassState (Map.empty)

insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (ClassState x) y z= ClassState (Map.insert z y x)

getValues :: ClassState -> InstrType -> [[String]]
getValues (ClassState x) y = Map.keys z  where z= Map.filter  (\k->k==y)  x

--returneaza toate elementele dintr-o clasa
getValues2 (ClassState x) = Map.keys x