module Hasyr.Components.AsyncInput
  ( Output(..)
  , Query(..)
  , component
  , defaultInput
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..), isJust)
import Data.Monoid (guard)
import Effect.Class (class MonadEffect)
import Halogen (Component, RefLabel(..), defaultEval, get, getHTMLElementRef, liftEffect, mkComponent, mkEval, modify_, raise)
import Halogen.HTML as H
import Halogen.HTML.Events as E
import Halogen.HTML.Properties as P
import Hasyr.Utils.HTML (className, maybeElem, (<:>))
import Network.RemoteData (RemoteData(..), isLoading, isNotAsked)
import Web.HTML.HTMLElement (focus)
import Web.UIEvent.KeyboardEvent as KE

type State e a = { inputValue :: String, input :: Input e a }

type Input e a =
  { initialValue :: String
  , name :: String
  , placeholder :: String
  , asyncStatus :: RemoteData e a
  , iconLeftClassName :: Maybe String
  , inputClassName :: String
  }

data Action e a
  = UpdateInput String
  | DetectKeyUp KE.KeyboardEvent
  | OnBlur
  | Init
  | Receive (Input e a)

data Output = EnterPressed String | EscPressed | InputBlurred

data Query q = ResetInput q

defaultInput :: ∀ e a. Input e a
defaultInput =
  { initialValue: ""
  , name: ""
  , placeholder: ""
  , asyncStatus: NotAsked
  , iconLeftClassName: Nothing
  , inputClassName: ""
  }

component :: ∀ e a m. MonadEffect m => Component H.HTML Query (Input e a) Output m
component = mkComponent
  { initialState: \input -> { inputValue: input.initialValue, input }
  , eval: mkEval $ defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , initialize = Just Init
    , receive = Just <<< Receive
    }
  , render
  } where

  handleAction = case _ of
    Init -> do
      getHTMLElementRef (RefLabel "async-input") >>= traverse_ \elem ->
        liftEffect $ focus elem
    UpdateInput val -> modify_ _{ inputValue = val }
    DetectKeyUp e -> case KE.key e of
      "Enter" -> do
        { inputValue } <- get
        raise $ EnterPressed inputValue
      "Escape" -> raise EscPressed
      _ -> pure unit
    OnBlur -> raise InputBlurred
    Receive input -> modify_ _{ input = input }

  handleQuery :: ∀ q. Query q -> _ (_ q)
  handleQuery (ResetInput next) = do
    modify_ _{ inputValue = "" }
    pure (Just next)

  render { inputValue, input } =
    H.div [
      className $
            "control"
        <:> guard (not $ isNotAsked input.asyncStatus) "has-icons-right"
        <:> guard (isJust input.iconLeftClassName) "has-icons-left"
        <:> cxControl input.asyncStatus
    ] [
      H.input [
        className $ "input" <:> cxInput input.asyncStatus <:> input.inputClassName,
        P.value inputValue,
        P.name input.name,
        P.placeholder input.placeholder,
        P.ref (RefLabel "async-input"),
        P.disabled (isLoading input.asyncStatus),
        E.onValueChange (Just <<< UpdateInput),
        E.onKeyUp (Just <<< DetectKeyUp),
        E.onBlur \_ -> Just OnBlur
      ],
      maybeElem input.iconLeftClassName \cxIconLeft ->
        H.span [className "icon is-small is-left"] [H.i [className cxIconLeft] []],
      case input.asyncStatus of
        (Success _) -> H.span [className "icon is-small is-right"] [H.i [className "ion-md-checkmark"] []]
        (Failure _) -> H.span [className "icon is-small is-right"] [H.i [className "ion-md-warning"] []]
        _ -> H.text ""
    ] where
    cxInput (Success _) = "is-success"
    cxInput (Failure _) = "is-danger"
    cxInput _ = ""

    cxControl Loading = "is-loading"
    cxControl _ = ""
