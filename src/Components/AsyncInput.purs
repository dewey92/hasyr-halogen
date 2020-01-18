module Hasyr.Components.AsyncInput
  ( Output(..)
  , Query(..)
  , component
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Halogen (Component, defaultEval, get, mkComponent, mkEval, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Hasyr.Utils.HTML (className, (<:>))
import Network.RemoteData (RemoteData(..), isLoading)
import Web.UIEvent.KeyboardEvent as KE

type State e a = { inputValue :: String, input :: Input e a }

type Input e a =
  { initialValue :: String
  , name :: String
  , placeholder :: String
  , asyncStatus :: RemoteData e a
  }

data Action e a
  = UpdateInput String
  | DetectKeyUp KE.KeyboardEvent
  | SyncState (Input e a)

data Output = EnterPressed String | EscPressed

data Query q = ResetInput q

component :: ∀ e a m. Component H.HTML Query (Input e a) Output m
component = mkComponent
  { initialState: \input -> { inputValue: input.initialValue, input }
  , eval: mkEval $ defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , receive = receive
    }
  , render
  } where

  receive = Just <<< SyncState

  handleAction = case _ of
    SyncState input -> modify_ _{ input = input }
    UpdateInput val -> modify_ _{ inputValue = val }
    DetectKeyUp e -> case KE.key e of
      "Enter" -> do
        { inputValue } <- get
        raise $ EnterPressed inputValue
      "Escape" -> raise EscPressed
      _ -> pure unit

  handleQuery :: ∀ q. Query q -> _ (_ q)
  handleQuery (ResetInput next) = do
    modify_ _{ inputValue = "" }
    pure (Just next)

  render { inputValue, input } =
    H.div [className $ "control has-icons-right" <:> cxControl input.asyncStatus] [
      H.input [
        className $ "input" <:> cxInput input.asyncStatus,
        P.value inputValue,
        P.name input.name,
        P.placeholder input.placeholder,
        P.autofocus true,
        P.disabled (isLoading input.asyncStatus),
        E.onValueChange (Just <<< UpdateInput),
        E.onKeyUp (Just <<< DetectKeyUp)
      ],
      case input.asyncStatus of
        (Success _) -> H.span [className "icon is-small is-right"] [H.i [className "fas fa-check"] []]
        (Failure _) -> H.span [className "icon is-small is-right"] [H.i [className "fas fa-exclamation-triangle"] []]
        _ -> H.text ""
    ] where
    cxInput (Success _) = "is-success"
    cxInput (Failure _) = "is-danger"
    cxInput _ = ""

    cxControl Loading = "is-loading"
    cxControl _ = ""
