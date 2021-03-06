{-# LANGUAGE OverloadedStrings #-}
module Main where

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


-- Placeholders for "make appropriate widget"
makeResultWidget (Right (ResponseSuccess _ (EvalResult _ (Right t)) _)) = T.pack t
makeResultWidget (Right (ResponseSuccess _ (EvalResult _ (Left (GhcErrors t))) _)) = T.pack t
makeResultWidget (Right (ResponseFailure _ t _)) = t
makeResultWidget (Right (RequestFailure _ t)) = t
makeResultWidget (Left t) = T.pack t

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
                HasMountStatus t m,
                PostBuild t m,
                MonadHold t m)
                => Dynamic t Int
                -> Dynamic t [HaskellSource] 
                -> Dynamic t (M.Map T.Text (Dynamic t HaskellSource))
                -> Int 
                -> HaskellSource
                -> Event t HaskellSource
                -> m ((Event t NewLine, Event t HaskellSource, Event t HaskellSource))

showResult exprCtr history bindings k (initialInput) inputEvent = divClass_ "card" $ mdo
        let host = constDyn (BasePath "/")
            (eval :<|> _ :<|> getType :<|> _) = client theAPI (Proxy :: Proxy m) (Proxy :: Proxy Int) host

        cfgAttrs <- holdDyn (M.fromList [("class", "haskellInput")]) never
        
        inEl <- divClass_ "haskellInputDiv" $ textInput ((def :: TextInputConfig t)
                { _textInputConfig_attributes = cfgAttrs
                , _textInputConfig_setValue = leftmost [inputEvent] -- add historyRecallEvent when we stop it triggering on (
                , _textInputConfig_initialValue = initialInput })
        
        pb <- getPostBuild
        ctr <- count $ updated $ value inEl
        let sendQueryEvent :: Event t Int
            sendQueryEvent = tag (current ctr) $ leftmost 
                [ if initialInput=="" then never else pb -- Send immediately if we already have input.
                , void $ updated $ value inEl -- and send if there's an update.
                , keypress Enter inEl -- Not sure why this is here.
                ]
            queryValue = Right . T.unpack <$> _textInput_value inEl
        
        -- We don't show "I haven't finished typing" errors, but do show errors when enter is pressed.
        showErrors <- hold False $ leftmost $ [True <$ keypress Enter inEl, False <$ updated (value inEl)]

        -- Get and filter the main results for out-of-order returns and hidden errors.
        let pruneIfHideErrors True a = Just a
            pruneIfHideErrors False a@(Right (ResponseSuccess _ (EvalResult _ (Right _)) _)) = Just a
            pruneIfHideErrors False _ = Nothing
       
        reqResults <- eval queryValue (constDyn QNone) sendQueryEvent
        let nonLateEvals = attachWithMaybe (\ctr rr->if reqTag rr>ctr then Just rr else Nothing) ctrSeen reqResults
        ctrSeen <- hold (-1) $ reqTag <$> nonLateEvals
        res <- holdDyn (Left "") $ attachWithMaybe pruneIfHideErrors showErrors $ Right <$> nonLateEvals
        
        -- Same for types. Probably should factor this out with the above, but types might end up diverging a bit.
        let pruneTErrors a@(Right (ResponseSuccess _ (TypeResult _ (Right _)) _)) = True
            pruneTErrors _ = False
        
        typeReqResults <- getType queryValue sendQueryEvent
        let nonLateTypes = attachWithMaybe (\ctr rr->if reqTag rr>ctr then Just rr else Nothing) ctrSeenT typeReqResults
        ctrSeenT <- hold (-1) $ reqTag <$> nonLateTypes
        typeRes <- holdDyn (Left "") $ ffilter pruneTErrors $ Right <$> nonLateTypes

        -- 
        divClass_ "card-block haskellResult small " $ dynText $ (makeTypeWidget <$> typeRes)

        divClass_ "card-block haskellResult" $ dynText $ (makeResultWidget <$> res)

        -- When we add a block and it's still the newest block added when it hits the DOM, focus it's input and scroll to it.
        ms <- getMountStatus
        let doneMountAndShouldFocus = ffilter (==k) $ tagPromptlyDyn exprCtr $ ffilter (==Mounted) $ updated ms
        let focusAndScroll = do
                focus $ _textInput_element inEl
                scrollIntoView (_textInput_element inEl) True
        performEvent_ $ liftIO focusAndScroll <$ doneMountAndShouldFocus

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
                 , tag (current $ value inEl) $ keypress Enter inEl) -- Add to the global history list.


main :: IO ()
main = mainWidgetWithHead myheader $ do
        divClass_ "navbar bg-light" $ do
                divClass_ "navbar-brand" $ text "HuntTools!"
                divClass_ "" $ elAttr "form" [("action","/hoogle/"),("method","get")] $ do
                        elAttr "a" [("href", "http://huntnew.tcita.com/hoogle/?hoogle=hunttools"), ("class", "btn")] $ text "Docs"
                        elAttr "script" [("type", "text/javascript"),("src","/hoogle/res/jquery.js")] $ text ""
                        elAttr "script" [("type", "text/javascript"),("src","/hoogle/res/hoogle.js")] $ text ""
                        elAttr "input" [("type", "text"),("name","hoogle"),("id","hoogle"),("accesskey","1")] $ text ""
                        elAttr "input" [("type","submit"),("value","Search")] $ text ""
        divClass_ "container" $ mdo
                --testPaste <- button "ANSWER"
                pastedSrcs <- replListWidget (pastedSrcs) -- <> ("43" <$ testPaste))
                return ()

myheader :: MonadWidget t m => m ()
myheader = do
        el "title" $ text "Hunttools"
        elAttr "link" (M.fromList [("rel", "stylesheet"),
                                   ("href", "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.3/css/bootstrap.min.css"),
                                   ("integrity", "sha384-Zug+QiDoJOrZ5t4lssLdxGhVrurbmBWopoEl+M6BdEfwnCJZtKxi1KgxUyJq13dy"),
                                   ("crossorigin", "anonymous")]) blank
        elAttr "link" (M.fromList [("rel", "stylesheet"), ("href", "hunttools.css")]) blank


replListWidget :: forall t m. (SupportsServantReflex t m,
                DomBuilder t m,
                DomBuilderSpace m ~ GhcjsDomSpace,
                MonadIO m,
                HasMountStatus t m,
                MonadFix m,
                PostBuild t m,
                MonadHold t m) 
                => Event t HaskellSource 
                -> m (Event t HaskellSource)
replListWidget pastedSrc = el "div" $ mdo
        let
            enterKeyAdd = (\i -> M.fromAscList [(i, Just "")]) <$> updated reqNum
            pastedAdd = attachPromptlyDynWith (\i v->M.fromAscList [(i, Just v)]) reqNum pastedSrc
        
        let env = constDyn mempty
        -- Main list
        rvs <- listWithKeyShallowDiff M.empty (leftmost [ pastedAdd, enterKeyAdd ]) (showResult reqNum commandHistory env)
        
        let enters :: Event t NewLine
            enters = (switchPromptlyDyn $ leftmost . fmap (\(a,_,_)->a) . M.elems <$> rvs) :: Event t NewLine
            widgetPastes :: Event t HaskellSource
            widgetPastes = (switchPromptlyDyn $ leftmost . fmap (\(_,b,_)->b) . M.elems <$> rvs) :: Event t HaskellSource
            addToHistory :: Event t HaskellSource
            addToHistory = (switchPromptlyDyn $ leftmost . fmap (\(_,_,c)->c) . M.elems <$> rvs) :: Event t HaskellSource

        commandHistory <- foldDyn (:) [] addToHistory
        pb <- getPostBuild
        reqNum <- count $ leftmost [ pb, void enters, void pastedSrc ]
        
        return widgetPastes

