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
  machine,
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
  void $ forkIO $ runLoop $ atomically $ TBatchQueue.readBatch queue


-- * Actor definition
-------------------------

{-|
Actor definition.

Specifies how to process the messages.

Use 'spawn' to launch actors.
-}
newtype Actor msg =
  Actor (IO (List msg) -> IO ())

instance Contravariant Actor where
  contramap fn (Actor def) = Actor (\ recv -> def (fmap (fmap fn) recv))

{-|
Define from fold-like actions over internal state.

Accepts:
- IO-action creating an initial state or nothing,
  if the actor must be immediately terminated.
- Transition function from message and state
  to an IO-action, which produces the new state or
  nothing if the actor must be stopped.
-}
stateful :: IO (Maybe state) -> (message -> state -> IO (Maybe state)) -> Actor message
stateful initialize transition =
  Actor $ \ receiveBatch ->
    let
      receiveBatchAndEliminate state =
        receiveBatch >>= eliminateBatch state
      eliminateBatch !state =
        \ case
          Cons msg batchTail ->
            transition msg state >>= \ case
              Just newState ->
                eliminateBatch newState batchTail
              Nothing ->
                return ()
          Nil ->
            receiveBatchAndEliminate state
      in
        initialize >>= \ case
          Just state ->
            receiveBatchAndEliminate state
          Nothing ->
            return ()

{-|
Like 'stateful', but isolates the pure state.

- Initial state
- State transition function
- State interpretation as effect
- Checking whether to receive another message
-}
machine :: state -> (message -> state -> state) -> (state -> IO ()) -> (state -> Bool) -> Actor message
machine initialState transition act checkReceive =
  Actor $ \ receiveBatch ->
    let
      receiveBatchAndEliminate state =
        receiveBatch >>= eliminateBatch state
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
                    else return ()
          Nil -> receiveBatchAndEliminate state
      in
        do
          act initialState
          if checkReceive initialState
            then receiveBatchAndEliminate initialState
            else return ()
