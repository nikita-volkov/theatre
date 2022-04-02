-- |
-- Minimalistic actor library.
module Theatre
  ( Actor,

    -- * Construction
    graceful,
    disgraceful,
    suicidal,

    -- * Usage
    tell,
    kill,
  )
where

import qualified Control.Concurrent.Chan.Unagi as E
import qualified SlaveThread as F
import Theatre.Prelude

-- |
-- Actor, which processes the messages of type @message@.
--
-- An abstraction over the message channel, thread-forking and killing.
data Actor message = Actor
  { -- | Send a message to the actor
    tell :: message -> IO (),
    -- | Kill the actor
    kill :: IO ()
  }

instance Semigroup (Actor message) where
  (<>) (Actor leftTell leftKill) (Actor rightTell rightKill) =
    Actor tell kill
    where
      tell message =
        leftTell message >> rightTell message
      kill =
        leftKill >> rightKill

instance Monoid (Actor message) where
  mempty =
    Actor (const (return ())) (return ())
  mappend =
    (<>)

instance Contravariant Actor where
  contramap fn (Actor tell kill) =
    Actor (tell . fn) kill

instance Divisible Actor where
  conquer =
    mempty
  divide divisor (Actor leftTell leftKill) (Actor rightTell rightKill) =
    Actor tell kill
    where
      tell message =
        case divisor message of
          (leftMessage, rightMessage) -> leftTell leftMessage >> rightTell rightMessage
      kill =
        leftKill >> rightKill

instance Decidable Actor where
  lose fn =
    Actor (const (return ()) . absurd . fn) (return ())
  choose choice (Actor leftTell leftKill) (Actor rightTell rightKill) =
    Actor tell kill
    where
      tell =
        either leftTell rightTell . choice
      kill =
        leftKill >> rightKill

-- |
-- An actor which cannot die by itself unless explicitly killed.
--
-- Given an interpreter of messages,
-- forks a thread to run the computation on and
-- produces a handle to address that actor.
--
-- Killing that actor will make it process all the messages in the queue first.
-- All the messages sent to it after killing won't be processed.
graceful ::
  -- | Interpreter of a message
  (message -> IO ()) ->
  IO (Actor message)
graceful interpretMessage =
  do
    (inChan, outChan) <- E.newChan
    F.fork $
      fix $ \loop ->
        {-# SCC "graceful/loop" #-}
        do
          message <- E.readChan outChan
          case message of
            Just payload ->
              do
                interpretMessage payload
                loop
            Nothing ->
              return ()
    return (Actor (E.writeChan inChan . Just) (E.writeChan inChan Nothing))

-- |
-- An actor which cannot die by itself unless explicitly killed.
--
-- Given an interpreter of messages,
-- forks a thread to run the computation on and
-- produces a handle to address that actor.
disgraceful ::
  -- | Interpreter of a message
  (message -> IO ()) ->
  IO (Actor message)
disgraceful receiver =
  suicidal (\producer -> forever (producer >>= receiver))

-- |
-- An actor, whose interpreter can decide that the actor should die.
--
-- Given an implementation of a receiver loop of messages,
-- forks a thread to run that receiver on and
-- produces a handle to address that actor.
suicidal ::
  -- | A message receiver loop. When the loop exits, the actor dies
  (IO message -> IO ()) ->
  IO (Actor message)
suicidal receiver =
  do
    (inChan, outChan) <- E.newChan
    threadId <- F.fork (receiver (E.readChan outChan))
    return (Actor (E.writeChan inChan) (killThread threadId))
