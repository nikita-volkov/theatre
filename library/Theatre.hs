{-|
Minimalistic actor library.

First you declare channels,
then you spawn actors.
This isolates the definition of actors from their connection,
allowing to define recursive networks.
I.e., when two actors send messages to each other.
-}
module Theatre
(
  -- * Channel
  allocateChannel,
  tell,
  spawn,
  -- * Actor definition
  Actor,
  stateful,
)
where

import Theatre.Prelude
import qualified Theatre.Util.TBatchQueue as TBatchQueue


-- * Channel
-------------------------

{-|
An address to which the messages get sent,
and which the actor listens to.
-}
newtype Channel msg =
  Channel (TBatchQueue.TBatchQueue msg)

{-|
Allocate a channel.
-}
allocateChannel :: IO (Channel msg)
allocateChannel =
  Channel <$> atomically TBatchQueue.new

{-|
Send a message to a channel.
-}
tell :: Channel msg -> msg -> IO ()
tell (Channel queue) msg =
  atomically (TBatchQueue.write queue msg)

{-|
Spawn an actor listening for messages on that channel.

When multiple actors are spawned on the same channel,
they will compete for the incoming messages.

It is advised to spawn a single actor per channel.
Otherwise you may end up with a tangled architecture.
-}
spawn :: Channel msg -> Actor msg -> IO ()
spawn (Channel queue) (Actor runLoop) =
  void $ forkIO $ runLoop queue


-- * Actor definition
-------------------------

{-|
Actor definition.

Specifies how to process the messages.

Use 'spawn' to launch actors.
-}
newtype Actor msg =
  Actor (TBatchQueue.TBatchQueue msg -> IO ())

{-|
- Initial state
- State transition function
- State interpretation as effect
- Checking whether to receive another message
-}
stateful :: state -> (message -> state -> state) -> (state -> IO ()) -> (state -> Bool) -> Actor message
stateful initialState transition act checkReceive =
  Actor $ \ queue ->
    let
      receiveBatch state =
        atomically (TBatchQueue.readBatch queue) >>= eliminateBatch state
      eliminateBatch !state =
        \ case
          Cons msg batchTail ->
            let
              newState = transition msg state
              in
                do
                  act newState
                  if checkReceive newState
                    then eliminateBatch newState batchTail
                    else atomically $ TBatchQueue.writeBatch queue batchTail
          Nil -> receiveBatch state
      in
        do
          act initialState
          if checkReceive initialState
            then receiveBatch initialState
            else return ()
