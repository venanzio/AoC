{-

Compilers Course (COMP3012), 2020
  Venanzio Capretta (based on Henrik Nilsson 2006-2013)

Compiler 3 for MiniTriangle

The State monad (based on Graham Hutton, Ch.12)

-}

module State where

newtype ST st a = S (st -> (a, st))

app :: ST st a -> st -> (a,st)
app (S st) x  =  st x

stUpdate :: st -> ST st ()
stUpdate st = S (\_ -> ((),st))

stState :: ST st st
stState = S (\st -> (st,st))

stRevise :: (st -> st) -> ST st ()
stRevise f = stState >>= stUpdate . f

instance Functor (ST st) where
   -- fmap :: (a -> b) -> ST st a -> ST st b
   fmap g st = S (\s -> let (x,s') = app st s in (g x, s'))

instance Applicative (ST st) where
   -- pure :: a -> ST st a
   pure x = S (\s -> (x,s))

   -- (<*>) :: ST st (a -> b) -> ST st a -> ST st b
   stf <*> stx = S (\s ->
      let (f,s')  = app stf s
          (x,s'') = app stx s' in (f x, s''))

instance Monad (ST st) where
   -- return :: a -> ST st a
   return x = S (\s -> (x,s))

   -- (>>=) :: ST st a -> (a -> ST st b) -> ST st b
   st >>= f = S (\s -> let (x,s') = app st s in app (f x) s')


-- Combining State and IO monads
{-
stEmit :: a -> ST st (IO a)
stEmit x = return (return x)

stPrint :: String -> ST st (IO ())
stPrint = return . putStrLn
-}


-- Example: The Fibonacci sequence

type FibST = ST (Integer,Integer)

fib :: FibST Integer
fib = do (a,b) <- stState
         stUpdate (b,a+b)
         return a

fibs :: FibST [Integer]
fibs = do x <- fib
          xs <- fibs
          return (x:xs)

fibonacci :: [Integer]
fibonacci = fst $ app fibs (1,1)



-- State Monad Transformer

newtype StateT st m a = StT (st -> m (a,st))

appT :: StateT st m a -> st -> m (a,st)
appT (StT st) s  = st s

resultT :: Monad m => StateT st m a -> st -> m a
resultT st s = do (x,_) <- appT st s
                  return x

lift :: Monad m => m a -> StateT st m a
lift mx = StT (\s -> do x <- mx
                        return (x,s))

stTUpdate :: Monad m => st -> StateT st m ()
stTUpdate s = StT (\_ -> return ((),s))

stTState :: Monad m => StateT st m st
stTState = StT (\s -> return (s,s))

stTRevise :: Monad m => (st -> st) -> StateT st m ()
stTRevise f = stTState >>= stTUpdate . f

instance Monad m => Functor (StateT st m) where
   -- fmap :: (a -> b) -> StateT st m a -> StateT st m b
   fmap g sta = StT (\s -> do (x,s') <- appT sta s
                              return (g x, s'))

instance Monad m => Applicative (StateT st m) where
   -- pure :: a -> StateT st m a
   pure x = StT (\s -> pure (x,s))

   -- (<*>) :: StateT st m (a -> b) -> StateT st m a -> StateT st m b
   stf <*> stx = StT (\s -> do
          (f,s')  <- appT stf s
          (x,s'') <- appT stx s'
          return (f x, s''))

instance Monad m => Monad (StateT st m) where
   -- return :: a -> StateT st m a
   return x = StT (\s -> return (x,s))

   -- (>>=) :: StateT st m a -> (a -> StateT st m b) -> StateT st mb
   st >>= f = StT (\s -> do (x,s') <- appT st s
                            appT (f x) s')


type StateIO st = StateT st IO
