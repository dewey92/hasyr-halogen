module Hasyr.Utils.HTML where

import Prelude

import Data.Maybe (Maybe, fromJust, maybe)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Partial.Unsafe (unsafePartial)
import Web.DOM.Node (contains, fromEventTarget)
import Web.Event.Event (preventDefault, target)
import Web.Event.Internal.Types as ET
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement (toNode)
import Web.UIEvent.MouseEvent as ME

className :: ∀ r i. String -> H.IProp ( class :: String | r ) i
className = P.class_ <<< H.ClassName

appendClassName :: String -> String -> String
appendClassName a b = a <> " " <> b

infixl 6 appendClassName as <:>

-- | `onClick` but with `preventDefault` run by default.
-- | I know it's not idiomatic PS, but I think it's still way simpler than
-- | the solution proposed here:
-- |
-- | https://github.com/slamdata/purescript-halogen/issues/426#issuecomment-320390523
onClick_ :: ∀ r i. (Unit -> Maybe i) -> H.IProp (onClick :: ME.MouseEvent | r) i
onClick_ fn = E.onClick (fn <<< unsafePerformEffect <<< preventDefault <<< ME.toEvent)

whenElem :: ∀ w i. Boolean -> (Unit -> H.HTML w i) -> H.HTML w i
whenElem cond fn = if cond then fn unit else H.text ""

maybeElem :: ∀ a w i. Maybe a -> (a -> H.HTML w i) -> H.HTML w i
maybeElem mayB fn = maybe (H.text "") fn mayB

isClickOutside :: ET.Event -> HTMLElement -> Effect Boolean
isClickOutside ev elem = not <$> contains (toNode elem) evTargetNode where
  evTargetNode = unsafePartial $ fromJust $ fromEventTarget =<< target ev
