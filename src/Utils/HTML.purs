module Hasyr.Utils.HTML where

import Prelude

import Halogen.HTML as H
import Halogen.HTML.Properties as P

className :: ∀ r i. String -> H.IProp ( class :: String | r ) i
className = P.class_ <<< H.ClassName
