module WebExtension.Storage where

import Prelude

import Control.Monad.Reader.Trans (ReaderT, ask)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Promise (Promise, toAff)
import Effect.Aff (Aff)

-- TODO: these are the simplest cases with String keys/values
foreign import getImpl 
  :: StorageAreaType     -- sync constructor
  -> StorageAreaType     -- local constructor
  -> StorageAreaType 
  -> String              -- key
  -> Promise String
foreign import setImpl 
  :: StorageAreaType  -- sync constructor
  -> StorageAreaType  -- local constructor
  -> StorageAreaType 
  -> String           -- key
  -> String           -- value
  -> Promise Unit
foreign import removeImpl 
  :: StorageAreaType  -- sync constructor
  -> StorageAreaType  -- local constructor
  -> StorageAreaType 
  -> String           -- key
  -> Promise Unit


get :: StorageAreaType -> String -> Aff String
get area key = toAff $ getImpl Sync Local area key

get' :: String -> StorageAreaT Aff String
get' key = StorageAreaT do
  area <- ask
  StorageArea <$> (lift $ get area key)

set :: StorageAreaType -> String -> String -> Aff Unit
set area key value = toAff $ setImpl Sync Local area key value

set' :: String -> String -> StorageAreaT Aff Unit
set' key value = StorageAreaT do
  area <- ask
  StorageArea <$> (lift $ set area key value)

remove :: StorageAreaType -> String -> Aff Unit
remove area key = toAff $ removeImpl Sync Local area key

remove' :: String -> StorageAreaT Aff Unit
remove' key = StorageAreaT do
  area <- ask
  StorageArea <$> (lift $ remove area key)


data StorageAreaType 
  = Sync
  | Local


-- TODO: the monad is still WIP
data StorageArea a = StorageArea a

derive instance functorStorageArea :: Functor StorageArea
instance applyStorageArea :: Apply StorageArea where
  apply (StorageArea f) a = f <$> a
instance applicativeStorageArea :: Applicative StorageArea where
  pure a = StorageArea a
instance bindStorageArea :: Bind StorageArea where
  bind (StorageArea a) m = m a
instance monadStorageArea :: Monad StorageArea


newtype StorageAreaT m a = StorageAreaT (ReaderT StorageAreaType m (StorageArea a))

derive instance functorStorageAreaT :: Functor m => Functor (StorageAreaT m)
instance applyStorageAreaT :: Monad m => Apply (StorageAreaT m) where
  apply = ap
instance applicativeStorageAreaT :: Monad m => Applicative (StorageAreaT m) where
  pure = StorageAreaT <<< pure <<< StorageArea
instance bindStorageAreaT :: Monad m => Bind (StorageAreaT m) where
  bind (StorageAreaT sa) m = StorageAreaT do
     sa >>= \(StorageArea a) -> case m a of StorageAreaT sa' -> sa'
instance monadStorageAreaT :: Monad m => Monad (StorageAreaT m)
instance monadTransStorageAreaT :: MonadTrans StorageAreaT where
  lift = StorageAreaT <<< lift <<< liftM1 StorageArea

