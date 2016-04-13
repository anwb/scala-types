{-# LANGUAGE TypeFamilies #-}

data Stop = Done
newtype In  a b = In (a -> IO b)
data    Out a b = Out a   (IO b)

class Session a where
  type Dual a
  run :: a -> Dual a -> IO ()

instance (Session b) => Session (In a b) where
     type Dual (In a b) = Out a (Dual b)
     run (In f) (Out a d) = f a >>= \b -> d >>= \c -> run b c

instance (Session b) => Session (Out a b) where
     type Dual (Out a b) = In a (Dual b)
     run (Out a d) (In f) = f a >>= \b -> d >>= \c -> run c b

instance Session Stop where
     type Dual Stop = Stop
     run  Done Done = return ()

add_server :: In Int (In Int (Out Int Stop))
add_server =  In $ \x -> return $ In $ \y ->
              do { putStrLn "Thinking"
                 ; return $ Out (x + y) (return Done) }

add_client :: Out Int (Out Int (In Int Stop))
add_client =  Out 3 $ return $ Out 4 $
              do { putStrLn "Waiting"
                 ; return $ In $ \z -> print z >> return Done }                

                                       