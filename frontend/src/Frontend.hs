{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import qualified Data.Text as T
import Data.Text (Text)
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Frontend.Selection

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = body
  }


body :: (DomBuilder t m, PostBuild t m, Prerender js t m, MonadHold t m) => m ()
body = el "p" $ do
  text "Here is some test text that you can select."

  prerender_ (text "loading") $ do
    sel <- getSelectionString
    widgetHold_ blank $ ffor sel text
  pure ()
