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
import Control.Applicative

maybeToAlter :: Alternative f => Maybe a -> f a
maybeToAlter Nothing  = empty
maybeToAlter (Just x) = pure x

traverse1 :: (Data t, Typeable b,Alternative f)
          => (b -> Maybe a) -> t -> f a
traverse1 f = everything (<|>) $ mkQ empty (maybeToAlter . f)

traverse2 :: (Data t, Typeable b,Alternative f) 
  => (a1 -> a2 -> a) 
  -> (b -> Maybe a1) 
  -> (b -> Maybe a2) 
  -> t -> f a
traverse2 c f g = everything (<|>) $ mkQ empty s where
  s x = maybeToAlter $ pure c <*> f x <*> g x 

traverse3 :: (Data t, Typeable b,Alternative f) 
  => (a1 -> a2 -> a3 -> a) 
  -> (b -> Maybe a1) 
  -> (b -> Maybe a2) 
  -> (b -> Maybe a3)
  -> t -> f a
traverse3 c f g h = everything (<|>) $ mkQ empty s where
  s x = maybeToAlter $ pure c <*> f x <*> g x <*> h x  
