{-|
An optimization over TQueue intended for single reader.

Provides for fastest possible writing and reading.
-}
module Theatre.Util.TBatchQueue
where

import Theatre.Prelude
import qualified StrictList


newtype TBatchQueue a =
  TBatchQueue (TVar (List a))

new :: STM (TBatchQueue a)
new =
  TBatchQueue <$> newTVar StrictList.Nil

write :: TBatchQueue a -> a -> STM ()
write (TBatchQueue var) value =
  do
    list <- readTVar var
    writeTVar var (StrictList.Cons value list)

writeBatch :: TBatchQueue a -> List a -> STM ()
writeBatch (TBatchQueue var) list =
  do
    queueList <- readTVar var
    let !newQueueList = StrictList.prependReversed list queueList
    writeTVar var newQueueList

readBatch :: TBatchQueue a -> STM (List a)
readBatch (TBatchQueue var) =
  do
    list <- readTVar var
    writeTVar var StrictList.Nil
    return (StrictList.reverse list)
