{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE CPP                      #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI            #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE TypeFamilies             #-}

module Frontend.Selection where

import qualified GHCJS.DOM.Element   as DOM
import qualified GHCJS.DOM.Types     as DomTypes
import           GHCJS.DOM.Selection
import           Reflex.Dom.Core
import           Control.Monad.Trans (liftIO)
import           Data.Text           (Text)
import qualified GHCJS.DOM.Selection as Selection
import Data.Functor (($>))
import Data.Traversable (for)

#if defined(ghcjs_HOST_OS)
import           GHCJS.Foreign.Callback as Callback
#else
import           Control.Lens.Operators
import qualified Language.Javascript.JSaddle.Object as J
#endif

-- Constraints needed by these FFI functions.
type FfiConstraints t m = (
  DomBuilder t m, DomBuilderSpace m ~ GhcjsDomSpace, MonadHold t m,
  DomTypes.MonadJSM (Performable m), PostBuild t m, PerformEvent t m, TriggerEvent t m
  )


setDocumentOnSelectionChangeRaw :: DomTypes.JSM () -> DomTypes.JSM ()
#if defined(ghcjs_HOST_OS)

setDocumentOnSelectionChangeRaw cb = do
  cb' <- Callback.asyncCallback cb
  js_setDocumentOnSelectionChange cb'

foreign import javascript unsafe
  "document['addEventListener']('selectionchange', $1)"
  js_setDocumentOnSelectionChange :: Callback.Callback (DomTypes.JSM ()) -> DomTypes.JSM ()

#else

setDocumentOnSelectionChangeRaw cb = do
  doc <- J.jsg ("document" :: Text)
  _ <- doc ^. J.js2 ("addEventListener" :: Text) ("selectionchange" :: Text) (J.fun $ \_ _ _ -> cb)
  pure ()

#endif


setDocumentOnSelectionChange
  :: (FfiConstraints t m)
  => Event t () -> m (Event t ())
setDocumentOnSelectionChange register = do
  (event, callback) <- newTriggerEvent
  performEvent_ $ ffor register $ \() -> DomTypes.liftJSM $
    setDocumentOnSelectionChangeRaw (liftIO $ callback ())
  pure event


#if defined(ghcjs_HOST_OS)
foreign import javascript unsafe
  "$r = document['getSelection']()"
  getSelectionRaw :: DomTypes.JSM Selection.Selection
#else
getSelectionRaw :: DomTypes.JSM Selection.Selection
getSelectionRaw = do
  doc <- J.jsg ("document" :: Text)
  r <- doc ^. J.js0 ("getSelection" :: Text)
  DomTypes.fromJSValUnchecked r
#endif


getSelection
  :: (FfiConstraints t m)
  => (Selection.Selection -> DomTypes.JSM a) -> m (Event t a)
getSelection f = do
  pb <- getPostBuild
  changed <- setDocumentOnSelectionChange pb
  performEvent $ ffor changed $ \() -> DomTypes.liftJSM $ f =<< getSelectionRaw

selectionToString :: Selection.Selection -> DomTypes.JSM Text
selectionToString sel = DomTypes.fromJSString <$> Selection.toString sel

selectionRange :: Selection.Selection -> DomTypes.JSM (Maybe (DomTypes.Node, Int), Maybe (DomTypes.Node, Int))
selectionRange sel = do
  anchorNode' <- Selection.getAnchorNode sel
  left <- for anchorNode' $ \node ->
    (,) node . fromIntegral <$> Selection.getAnchorOffset sel -- TODO: Overflow detection
  focusNode' <- Selection.getFocusNode sel
  right <- for focusNode' $ \node ->
    (,) node . fromIntegral <$> Selection.getFocusOffset sel -- TODO: Overflow detection
  pure (left, right)
