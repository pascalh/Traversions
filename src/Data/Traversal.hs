-- |Data.Traversal contains datatype-generic traversals. A traversal
-- is parameterized by selector functions and a term. The given term
-- is being traversed and the selector functions are applied whenever 
-- the types of selector function and current subterm match. 
-- The selected values are combined by a given function.
-- 

module Data.Traversal
  ( traverse1
  , traverse2
  , traverse3
  ) where
import Data.Generics.Aliases (mkQ)
import Data.Generics.Schemes (everything)
import Data.Data (Data(..),Typeable(..))
import Data.Maybe (maybeToList)
import Control.Monad (liftM2,liftM3)

traverse1 :: (Data t, Typeable b) => (b -> Maybe a) -> t -> [a]
traverse1 f = everything (++) $ mkQ [] (maybeToList . f)

traverse2 :: (Data t, Typeable b) 
  => (a1 -> a2 -> a) 
  -> (b -> Maybe a1) 
  -> (b -> Maybe a2) 
  -> t -> [a]
traverse2 c f g = everything (++) $ mkQ [] s where
  s x = maybeToList $ liftM2 c (f x) (g x) 

traverse3 :: (Data t, Typeable b) 
  => (a1 -> a2 -> a3 -> a) 
  -> (b -> Maybe a1) 
  -> (b -> Maybe a2) 
  -> (b -> Maybe a3)
  -> t -> [a]
traverse3 c f g h = everything (++) $ mkQ [] s where
  s x = maybeToList $ liftM3 c (f x) (g x) (h x)  
