{-# OPTIONS_GHC -Wno-missing-import-lists #-}

module Catagory.Prelude (
  module Prelude,
  Category ((.), id),
) where

import Control.Category (Category (id, (.)))
import Prelude hiding (id, (.))
