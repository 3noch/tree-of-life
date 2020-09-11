{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Frontend where

import qualified Data.Text as T
import Data.Text (Text)
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core
import Control.Monad (join)
import qualified Data.Text as T
import Control.Monad.Fix (MonadFix)
import Control.Applicative

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Frontend.Selection

frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = el "title" $ text "Obelisk Minimal Example"
  , _frontend_body = body
  }


body :: forall t m js. (DomBuilder t m, PostBuild t m, Prerender js t m, MonadHold t m, MonadFix m) => m ()
body = el "p" $ do
  liveRange :: Dynamic t (Maybe (Int, Int)) <- fmap join $ prerender (pure (pure Nothing)) $ do
    selRange <- getSelection $ \sel -> liftA2 (,) (selectionRange sel) (selectionToString sel)
    let
      isSomething (Just x, Just y) = y - x > 0
    holdDyn Nothing $ ffor selRange $ \case
      ((Just (_, offset), _), str) -> Just (offset, offset + T.length str)
      _ -> Nothing

  let corpus = "Here is some test text that you can select."

  highlight <- button "Highlight"


  low <- holdUniqDyn =<< holdDyn Nothing (current ((fmap.fmap) fst liveRange) <@ highlight)
  high <- holdUniqDyn =<< holdDyn Nothing (current ((fmap.fmap) snd liveRange) <@ highlight)
  dynText $ flip T.take corpus . maybe 0 id <$> low
  dyn_ $ ffor2 low high $ \l' h' -> case (l', h') of
    (Just l, Just h) | h - l > 0 ->
      elAttr "span" ("style" =: "background-color:yellow") (text $ T.take (h - l) $ T.drop l corpus)
    _ -> blank
  dynText $ flip T.drop corpus . maybe 0 id <$> high

  el "p" $

    display liveRange

  pure ()
