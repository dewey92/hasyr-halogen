module Hasyr.Utils.HTTP where

import Prelude

import Affjax as AX
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as Format
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay)

ajaxGet :: AX.URL -> Aff (Either String Json)
ajaxGet url = do
  rawResult <- AX.get Format.json url
  delay (Milliseconds 1000.0) -- not too fast, I want to show loading indicator
  pure $ do
    { body } <- lmap AX.printError rawResult
    pure body

ajaxPost :: ∀ a. EncodeJson a => AX.URL -> a -> Aff (Either String Json)
ajaxPost url payload = do
  let jsonPayload = RB.Json $ encodeJson payload
  rawResult <- AX.post Format.json url (Just jsonPayload)
  delay (Milliseconds 1500.0) -- not too fast, I want to show loading indicator
  pure $ do
    { body } <- lmap AX.printError rawResult
    pure body

ajaxPut :: ∀ a. EncodeJson a => AX.URL -> a -> Aff (Either String Json)
ajaxPut url payload = do
  let jsonPayload = RB.Json $ encodeJson payload
  rawResult <- AX.put Format.json url (Just jsonPayload)
  delay (Milliseconds 1500.0) -- not too fast, I want to show loading indicator
  pure $ do
    { body } <- lmap AX.printError rawResult
    pure body

ajaxPatch :: ∀ a. EncodeJson a => AX.URL -> a -> Aff (Either String Json)
ajaxPatch url payload = do
  let jsonPayload = RB.Json $ encodeJson payload
  rawResult <- AX.patch Format.json url jsonPayload
  delay (Milliseconds 1500.0) -- not too fast, I want to show loading indicator
  pure $ do
    { body } <- lmap AX.printError rawResult
    pure body

ajaxDelete :: AX.URL -> Aff (Either String Json)
ajaxDelete url = do
  rawResult <- AX.delete Format.json url
  delay (Milliseconds 1500.0) -- not too fast, I want to show loading indicator
  pure $ do
    { body } <- lmap AX.printError rawResult
    pure body
