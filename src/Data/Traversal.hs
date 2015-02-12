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

traverse1 :: (Data t, Typeable b,Alternative f)
          => (b -> f a) -> t -> f a
traverse1 f = everything (<|>) $ mkQ empty  f

traverse2 :: (Data t, Typeable b,Alternative f) 
  => (a1 -> a2 -> a) 
  -> (b -> f a1) 
  -> (b -> f a2) 
  -> t -> f a
traverse2 c f g = everything (<|>) $ mkQ empty s where
  s x = c <$> f x <*> g x 

traverse3 :: (Data t, Typeable b,Alternative f) 
  => (a1 -> a2 -> a3 -> a) 
  -> (b -> f a1) 
  -> (b -> f a2) 
  -> (b -> f a3)
  -> t -> f a
traverse3 c f g h = everything (<|>) $ mkQ empty s where
  s x = c <$> f x <*> g x <*> h x  
