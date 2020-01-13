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

type Base e a =
  ( name :: String
  , placeholder :: String
  , asyncStatus :: RemoteData e a
  )

type State e a = { inputValue :: String | Base e a }

type Input e a = { initialValue :: String | Base e a }

data Action e a
  = UpdateInput String
  | DetectKeyUp KE.KeyboardEvent
  | SyncState (Input e a)

data Output = EnterPressed String | EscPressed

data Query q = ResetInput q

component :: ∀ e a m. Component H.HTML Query (Input e a) Output m
component = mkComponent
  { initialState: \{ name, placeholder, initialValue, asyncStatus } ->
    { name, placeholder, asyncStatus, inputValue: initialValue }
  , eval: mkEval $ defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , receive = receive
    }
  , render
  } where

  receive = Just <<< SyncState

  handleAction = case _ of
    SyncState { name, placeholder, asyncStatus } -> do
      modify_ _{ name = name, placeholder = placeholder, asyncStatus = asyncStatus }
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

  render { inputValue, name, placeholder, asyncStatus } =
    H.div [className $ "control has-icons-right" <:> cxControl asyncStatus] [
      H.input [
        className $ "input" <:> cxInput asyncStatus,
        P.value inputValue,
        P.name name,
        P.placeholder placeholder,
        P.autofocus true,
        P.disabled (isLoading asyncStatus),
        E.onValueChange (Just <<< UpdateInput),
        E.onKeyUp (Just <<< DetectKeyUp)
      ],
      case asyncStatus of
        (Success _) -> H.span [className "icon is-small is-right"] [H.i [className "fas fa-check"] []]
        (Failure _) -> H.span [className "icon is-small is-right"] [H.i [className "fas fa-exclamation-triangle"] []]
        _ -> H.text ""
    ] where
    cxInput (Success _) = "is-success"
    cxInput (Failure _) = "is-danger"
    cxInput _ = ""

    cxControl Loading = "is-loading"
    cxControl _ = ""
