module Data.StateVar where

data StateVar a = StateVar (IO a) (a -> IO ())

makeStateVar :: IO a -> (a -> IO ()) -> StateVar a
makeStateVar = StateVar

