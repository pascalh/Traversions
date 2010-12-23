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
import Control.Monad (MonadPlus(..),ap)

-- |transforms maybe values to monadplus
maybeToMplus :: MonadPlus m => Maybe a -> m a
maybeToMplus Nothing  = mzero
maybeToMplus (Just x) = return x

traverse1 :: (Data t, Typeable b,MonadPlus m) 
          => (b -> Maybe a) -> t -> m a
traverse1 f = everything mplus $ mkQ mzero (maybeToMplus . f)

traverse2 :: (Data t, Typeable b,MonadPlus m) 
  => (a1 -> a2 -> a) 
  -> (b -> Maybe a1) 
  -> (b -> Maybe a2) 
  -> t -> m a
traverse2 c f g = everything mplus $ mkQ mzero s where
  s x = maybeToMplus $ return c `ap` f x `ap` g x 

traverse3 :: (Data t, Typeable b,MonadPlus m) 
  => (a1 -> a2 -> a3 -> a) 
  -> (b -> Maybe a1) 
  -> (b -> Maybe a2) 
  -> (b -> Maybe a3)
  -> t -> m a
traverse3 c f g h = everything mplus $ mkQ mzero s where
  s x = maybeToMplus $ return c `ap` f x `ap` g x `ap` h x  
