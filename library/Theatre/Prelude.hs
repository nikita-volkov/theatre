module Theatre.Prelude
(
  module Exports,
)
where


-- base
-------------------------
import Data.Void as Exports

-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, (<>), First(..), Last(..))

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- semigroups
-------------------------
import Data.Semigroup as Exports
