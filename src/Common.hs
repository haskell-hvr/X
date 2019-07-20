-- |
-- Module    : Common
-- Copyright : (c) Herbert Valerio Riedel 2019
-- SPDX-License-Identifier: GPL-3.0-or-later
--
-- Internal vocabulary providing common vocabulary
--

module Common (module X) where

import           Data.Data       as X (Data)
import           Data.Monoid     as X (Monoid (mappend, mempty))
import           Data.String     as X (fromString)
import           Data.Text       as X (Text)
import           Data.Text.Short as X (ShortText)
import           Data.Typeable   as X (Typeable)
