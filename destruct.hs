

import Control.Monad.State

type D = ((Int, Int), (Int, Int))
newD :: D 
newD = ((0,1), (2,3))


imp :: State D Int
imp = return 34

a :: State D (Int, Int)
a = do
  f <- get
  let (f', _) = f
  return f'
  
b :: State D Int
b = do
  f <- get
  let (f', _) = f
  let (f'', _) = f'
  return f''

c :: (D -> a) -> State D a
c f = do
  s <- get
  return $ f s

-- d :: (D -> a) -> State D a
-- d f =
--   liftM f 

  