{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend where

import qualified Data.Text as T
import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

import Common.Api
import Common.Route
import Obelisk.Generated.Static

import Reflex.Dom
import Servant.Reflex
import Data.Default
import Servant.API
import Control.Monad.Fix
import Zoomeval.API
import qualified Data.Map as M
import Data.Proxy
import qualified Data.Text as T
import Servant.Reflex.Multi
import Data.Semigroup
import GHCJS.DOM.HTMLElement (focus)
import GHCJS.DOM.Element (scrollIntoView)
import Control.Monad.IO.Class
import Control.Monad
import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import Data.List
import Fancy

-- Placeholders for "make appropriate widget"
makeResultWidget (Right (ResponseSuccess _ (EvalResult expr _ (Just v)) _)) = case (fromJSON $ v :: Result FancyDispatch) of
  Success a -> fancyWidget expr a
  Data.Aeson.Error err -> text $ "Broken fancy: " <> T.pack err

makeResultWidget (Right (ResponseSuccess _ (EvalResult _ (Right t) _) _)) = text $ T.pack t
makeResultWidget (Right (ResponseSuccess _ (EvalResult _ (Left (GhcErrors t)) _) _)) = text $ T.pack t
makeResultWidget (Right (ResponseFailure _ t _)) = text $ t
makeResultWidget (Right (RequestFailure _ t)) = text $ t
makeResultWidget (Left t) = text $ T.pack t

makeTypeWidget (Right (ResponseSuccess _ (TypeResult _ (Right t)) _)) = T.pack $ ":: " ++ t
makeTypeWidget (Right (ResponseSuccess _ (TypeResult _ (Left (GhcErrors t))) _)) = T.pack t
makeTypeWidget (Right (ResponseFailure _ t _)) = t
makeTypeWidget (Right (RequestFailure _ t)) = t
makeTypeWidget (Left t) = T.pack t

divAttr :: DomBuilder t m => M.Map T.Text T.Text -> m a -> m a
divAttr = elAttr "div"
divClass_ c m = elAttr "div" (M.fromAscList [("class", c)]) m

data NewLine = NewLine
        deriving Show
data SetFocus = SetFocus
        deriving Show

type HaskellSource = T.Text

showResult :: forall t m. (SupportsServantReflex t m,
                DomBuilder t m,
                DomBuilderSpace m ~ GhcjsDomSpace,
                MonadFix m,
                MonadIO m,
                --HasMountStatus t m,
                PostBuild t m,
                MonadHold t m)
                => Dynamic t Int
                -> Dynamic t Int
                -> Dynamic t [HaskellSource]
                -> Dynamic t (M.Map T.Text (Dynamic t HaskellSource))
                -> Int
                -> HaskellSource
                -> Event t HaskellSource
                -> m ((Event t NewLine, Event t HaskellSource, Event t HaskellSource, Event t ()))

showResult exprCtr totalLines history bindings k (initialInput) inputEvent = divClass_ "card" $ mdo
        let host = constDyn (BasePath "/") --(BaseFullUrl Http "huntnew.tcita.com" 80 "/")
            (_ :<|> (eval) :<|> getType :<|> _) = client theAPI (Proxy :: Proxy m) (Proxy :: Proxy Int) host

        cfgAttrs <- holdDyn (M.fromList [("class", "haskellInput")]) never

        inEl <- divClass_ "haskellInputDiv" $ textInput ((def :: TextInputConfig t)
                { _textInputConfig_attributes = cfgAttrs
                , _textInputConfig_setValue = leftmost [inputEvent] -- add historyRecallEvent when we stop it triggering on (
                , _textInputConfig_initialValue = initialInput })

        pb <- getPostBuild
        ctr <- count $ updated $ value inEl
        valUpdated <- throttle 1 $ void $ updated $ value inEl -- and send if there's an update.
        let sendQueryEvent :: Event t Int
            sendQueryEvent = tag (current ctr) $ leftmost
                [ if initialInput=="" then never else pb -- Send immediately if we already have input.
                , valUpdated
                , keypress Enter inEl -- Not sure why this is here.
                ]
            queryValue = T.unpack <$> _textInput_value inEl

        -- We don't show "I haven't finished typing" errors, but do show errors when enter is pressed.
        showErrors <- hold False $ leftmost $ [True <$ keypress Enter inEl, False <$ updated (value inEl)]

        -- Get and filter the main results for out-of-order returns and hidden errors.
        let pruneIfHideErrors True a = Just a
            pruneIfHideErrors False a@(Right (ResponseSuccess _ (EvalResult _ (Right _) _) _)) = Just a
            pruneIfHideErrors False _ = Nothing

        reqResults <- eval (QParamSome <$> queryValue) sendQueryEvent
        let nonLateEvals = attachWithMaybe (\ctr rr->if reqTag rr>ctr then Just rr else Nothing) ctrSeen reqResults
        ctrSeen <- hold (-1) $ reqTag <$> nonLateEvals
        res <- holdDyn (Left "") $ attachWithMaybe pruneIfHideErrors showErrors $ Right <$> nonLateEvals

        -- Same for types. Probably should factor this out with the above, but types might end up diverging a bit.
        let pruneTErrors a@(Right (ResponseSuccess _ (TypeResult _ (Right _)) _)) = True
            pruneTErrors _ = False

        typeReqResults <- getType (Right <$> queryValue) sendQueryEvent
        let nonLateTypes = attachWithMaybe (\ctr rr->if reqTag rr>ctr then Just rr else Nothing) ctrSeenT typeReqResults
        ctrSeenT <- hold (-1) $ reqTag <$> nonLateTypes
        typeRes <- holdDyn (Left "") $ ffilter pruneTErrors $ Right <$> nonLateTypes

        divClass_ "card-block haskellResult small " $ dynText $ (makeTypeWidget <$> typeRes)

        divClass_ "card-block haskellResult" $ (dyn $ makeResultWidget <$> res)

        -- When we add a block and it's still the newest block added when it hits the DOM, focus it's input and scroll to it.
        --ms <- getMountStatus
        --mounted <- el "script" $ text $ "blockadded(" <> T.pack show k <> ");"

        --let doneMountAndShouldFocus = ffilter (==k) $ tagPromptlyDyn exprCtr $ ffilter (==Mounted) $ updated ms
        --let focusAndScroll = do
        --        focus $ _textInput_element inEl
        --        scrollIntoView (_textInput_element inEl) True
        --performEvent_ $ liftIO focusAndScroll <$ doneMountAndShouldFocus

        -- This is a bit of a mess, but it's an index into the history, or a phantom empty element before the list.
        -- The phantom element is only accessible via down-arrow, and the history list loops if you up-arrow past the end.
        -- At some point this should get a "and this is the string you started with" for when you hit arrow keys with something in the buffer.
        historyCounter <- foldDyn (\(h,n) cur -> let i=cur+n; in case (i>=(-1),i<h) of { (True,True)->i; (False,_)->(-1);(_,False)->i-h; })
                            (-1) $ attach (length <$> current history) $ leftmost [1 <$ keypress ArrowUp inEl, -1 <$ keypress ArrowDown inEl]
        let historyRecallEvent = attachWithMaybe (\l i -> if i==(-1) then Just "" else if i>=0 && i<length l then Just (l!!i) else Nothing)
                                        (current history)
                                        (updated historyCounter)

        return $ (NewLine <$ keypress Enter inEl -- Make a new input line when enter is hit
                 , never -- Input-generating widgets aren't implemented, but they'll feed to here.
                 , tag (current $ value inEl) $ keypress Enter inEl -- Add to the global history list.
                 , gate ((==k) <$> current totalLines) $ void $ updated $ value inEl -- Set allowed-to-add-line if we update here.
                 )


replListWidget :: forall t m. (SupportsServantReflex t m,
                DomBuilder t m,
                DomBuilderSpace m ~ GhcjsDomSpace,
                MonadIO m,
                --HasMountStatus t m,
                MonadFix m,
                PostBuild t m,
                MonadHold t m)
                => Event t HaskellSource
                -> m (Event t HaskellSource)
replListWidget pastedSrc = el "div" $ mdo
        canAddLine <- holdDyn True $ leftmost [ True <$ reenableNewLine, False <$ enters ]
        let
            enterKeyAdd = gate (current canAddLine) $ (\i -> M.fromAscList [(i+1, Just "")]) <$> (tag (current lineNum) $ updated reqNum)
            pastedAdd = attachPromptlyDynWith (\i v->M.fromAscList [(i, Just v)]) reqNum pastedSrc

            env = constDyn mempty
            addLines = leftmost [ pastedAdd, enterKeyAdd ]
        -- Main list
        rvs <- listWithKeyShallowDiff M.empty addLines (showResult lineNum totalLines commandHistory env)

        let enters :: Event t NewLine
            enters = (switchPromptlyDyn $ leftmost . fmap (\(a,_,_,_)->a) . M.elems <$> rvs) :: Event t NewLine
            widgetPastes :: Event t HaskellSource
            widgetPastes = (switchPromptlyDyn $ leftmost . fmap (\(_,b,_,_)->b) . M.elems <$> rvs) :: Event t HaskellSource
            addToHistory :: Event t HaskellSource
            addToHistory = (switchPromptlyDyn $ leftmost . fmap (\(_,_,c,_)->c) . M.elems <$> rvs) :: Event t HaskellSource
            reenableNewLine = (switchPromptlyDyn $ leftmost . fmap (\(_,_,_,d)->d) . M.elems <$> rvs) :: Event t ()

        commandHistory <- foldDyn (:) [] addToHistory
        pb <- getPostBuild
        reqNum <- count $ leftmost [ pb, void enters, void pastedSrc ]
        lineNum <- count $ addLines
        let totalLines = length <$> rvs

        return widgetPastes



frontend :: Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = do
        el "title" $ text "Hunttools"
        elAttr "link" (M.fromList [("rel", "stylesheet"),
                                   ("href", "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css"),
                                   ("integrity", "sha384-Zug+QiDoJOrZ5t4lssLdxGhVrurbmBWopoEl+M6BdEfwnCJZtKxi1KgxUyJq13dy"),
                                   ("crossorigin", "anonymous")]) blank
        elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", static @"hunttools.css")]) blank
  , _frontend_body = do
          divClass_ "navbar bg-light" $ do
            divClass_ "navbar-brand" $ text "HuntTools!"
            divClass_ "" $ elAttr "form" [("action","/hoogle/"),("method","get")] $ do
            elAttr "a" [("href", "/hoogle/?hoogle=hunttools"), ("class", "btn")] $ text "Docs"
            elAttr "script" [("type", "text/javascript"),("src","/hoogle/res/jquery.js")] $ text ""
            elAttr "script" [("type", "text/javascript"),("src","/hoogle/res/hoogle.js")] $ text ""
            elAttr "input" [("type", "text"),("name","hoogle"),("id","hoogle"),("accesskey","1")] $ text ""
            elAttr "input" [("type","submit"),("value","Search")] $ text ""
          divClass_ "container" $ mdo
          --testPaste <- button "ANSWER"
            prerender (return ()) $ mdo
              pastedSrcs <- replListWidget (pastedSrcs) -- <> ("43" <$ testPaste))
              return ()
  }


fancyWidget :: forall t m. (SupportsServantReflex t m,
                DomBuilder t m,
                DomBuilderSpace m ~ GhcjsDomSpace,
                MonadIO m,
                --HasMountStatus t m,
                MonadFix m,
                PostBuild t m,
                MonadHold t m)
                => String -> FancyDispatch -> m ()
fancyWidget expr (Choice a hasMoreInit) = el "div" $ mdo
  let host = constDyn (BasePath "/") --(BaseFullUrl Http "huntnew.tcita.com" 80 "/")
      (_ :<|> (eval) :<|> getType :<|> _) = client theAPI (Proxy :: Proxy m) (Proxy :: Proxy Int) host
  el "h2" $ text "Chooser widget"

  ctr <- count $ updated currentQuery
  let sendQueryEvent :: Event t Int
      sendQueryEvent = tag (current ctr) $ updated currentQuery

  reqResults <- eval (QParamSome <$> currentQuery) sendQueryEvent
  let extractChoice (ResponseSuccess _ (EvalResult _ _ (Just v)) _) = case (fromJSON $ v :: Result FancyDispatch) of
        Success (Choice a hasMore) -> (a, hasMore)
        _ -> (["FAILURE"], False)
      extractChoice _ = (["FAILURE"], False)

  availableWordRes <- holdDyn (a, hasMoreInit) $ extractChoice <$> reqResults
  let availableWords = fst <$> availableWordRes
      hasMoreNow = snd <$> availableWordRes

  currentWords <- foldDyn id [] $ leftmost $ [addWord, removeWord]

  let mkQuery cWds skip = "applyInput (" <> expr <> ") (" <> show cWds <> " :: [String], " <> show skip <> " :: Int )"
      currentQuery = mkQuery <$> currentWords <*> pageNumber

  dynText $ T.pack <$> currentQuery

  pageNumber <- foldDyn (id) 0 $ leftmost [(+ 1) <$ moreEvt, (+ (-1)) <$ lessEvt, const 0 <$ updated currentWords ]

  let wordButton a = do
      recreated <- (dyn $ (\b -> (b <$) <$> (button . T.pack) b) <$> a)
      switchPromptlyDyn <$> holdDyn never recreated

  removeWordEvtsDyn <- el "div" $ do
    el "label" $ text "Currently selected: "
    simpleList currentWords wordButton
  let removeWord = delete <$> (switchPromptlyDyn $ leftmost <$> removeWordEvtsDyn)

  newWordsEvtsDyn <- el "div" $ simpleList availableWords wordButton
  let addWord = (:) <$> (switchPromptlyDyn $ leftmost <$> newWordsEvtsDyn)

  dynText $ (\a -> if a then "...more..." else "") <$> hasMoreNow
  el "br" blank
  lessEvt <- button "<"
  dynText $ T.pack . show <$> pageNumber
  moreEvt <- button ">"

  return ()

fancyWidget expr (StringList a hasMoreInit) = mdo
  let host = constDyn (BasePath "/") --(BaseFullUrl Http "huntnew.tcita.com" 80 "/")
      (_ :<|> (eval) :<|> getType :<|> _) = client theAPI (Proxy :: Proxy m) (Proxy :: Proxy Int) host
  ctr <- count $ updated currentQuery
  let sendQueryEvent :: Event t Int
      sendQueryEvent = tag (current ctr) $ updated currentQuery

  reqResults <- eval (QParamSome <$> currentQuery) sendQueryEvent
  let extractStringList (ResponseSuccess _ (EvalResult _ _ (Just v)) _) = case (fromJSON $ v :: Result FancyDispatch) of
        Success (StringList a hasMore) -> (a, hasMore)
        a -> (["FAILURE" <> show a], False)
      extractStringList _ = (["FAILURE"], False)

  wordListRes <- holdDyn (a, hasMoreInit) $ extractStringList <$> reqResults
  let wordList = fst <$> wordListRes
      hasMoreNow = snd <$> wordListRes

  let mkQuery skip = "applyInput ( " <> expr <> " ) ( " <> show skip <> " :: Int )"
      currentQuery = mkQuery <$> pageNumber

  -- dynText $ T.pack <$> currentQuery

  pageNumber <- foldDyn (id) 0 $ leftmost [(+ 1) <$ moreEvt, (+ (-1)) <$ lessEvt ]

  let wordWidget a = do
          el "div" $ do
            text $ a
            wikiLinkFor a
            return ()

  divClass "wordlist" $ simpleList wordList $ dyn . fmap (wordWidget . T.pack)

  dynText $ (\a -> if a then "...more..." else "") <$> hasMoreNow

  el "br" blank

  lessEvt <- button "<"
  dynText $ T.pack . show <$> pageNumber
  moreEvt <- button ">"

  return ()




wikiLinkFor str = mdo
            pb <- getPostBuild
            text " "
            hasWiki <- getAndDecode $ ("http://en.wikipedia.org/w/api.php?action=query&origin=*&format=json&titles=" <> str <> "") <$ pb
            widgetHold blank $ rvFunc <$> hasWiki
            where
             rvFunc (Nothing :: Maybe Data.Aeson.Value) = blank
             rvFunc (Just nothingRv) =
               case nothingRv ^? key "query" . key "pages" . key "-1" . key "missing" of
                 Just _ -> blank
                 Nothing -> elAttr "a" [("href", ("http://wikipedia.org/wiki/" <> str)), ("target", "_blank")] $ text "WIKI"

