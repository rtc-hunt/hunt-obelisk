{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
module Help where

import Control.Monad.Fix
import Reflex
import Reflex.Dom
import Types

helpPanel
  :: ( DomBuilder t m,
       -- DomBuilderSpace m ~ GhcjsDomSpace,
       MonadFix m,
       -- HasMountStatus t m,
       PostBuild t m,
       MonadHold t m)
  => Dynamic t Bool
  -> m (Event t HaskellSource)
helpPanel showSidebar = do
  let sidebarClass = ffor showSidebar $ \case
        True -> "help shown"
        False -> "help hidden"
  elDynAttr "div" (("class" =:) <$> sidebarClass) $ divClass "help-content" $ do
    prev <- button "prev"
    next <- button "next"
    page <- foldDyn id 0 $ leftmost [ succ <$ next, pred <$ prev ]
    panelChange <- dyn $ (panels !!) <$> page
    switchHold never panelChange

panels
  :: ( DomBuilder t m,
       -- DomBuilderSpace m ~ GhcjsDomSpace,
       MonadFix m,
       -- HasMountStatus t m,
       PostBuild t m,
       MonadHold t m)
  => [ m (Event t HaskellSource) ]
panels = el "div" . fmap snd . runEventWriterT <$>
    [ do
      el "h3" $ text "Examples!"
      el "p" $ text "Click any example to insert into the input; hit enter in the input to run it."
      --       '<p><code class="clicky">next</code> and <code class="clicky">back</code> will advance through the help files.' +
      el "ul" $ do
        exaclip "Basic anagram: " "anagram sysDict \"aachinopr\" " blank
        exaclip "Anagram with unknowns: " "anagram sysDict \"aachino??\"" blank
        exaclip "Crossword (in-order, unknowns):" "crossword sysDict \"??f??\"" blank
        exaclip "Crossword with globs (match any number of any characters): " "crossword sysDict \"a*ialist\"" blank
        exaclip "Require use of all letters for anagram (default is half):" "anagramFull sysDict \"aeelmpx\"" blank
        exaclip "Require at least 6 letters: " "anagramMin sysDict \"aeelmpx\" 5" blank
        exaclip "Return all possible words, longest first: " "anagramAny sysDict \"aeelmpx\"" blank
        exaclip "rot13: " "Cipher.rot 13 \"aeelmpx\"" blank
      text "Dictionaries:"
      el "ul" $ do
        el "li" $ text "enwikt - english wiktionary titles"
        el "li" $ text "ukacd - UK advanced cryptics dictionary"
        el "li" $ text "sowpods - Non-US scrabble dictionary"
        el "li" $ text "sysDict - /usr/share/bin/dict"
      el "p" $ do
        text "In general, any haskell code that "
        elAttr "a" ("href" =: "http://tryhaskell.org/") $ text "tryhaskell.org"
        text $ " will accept will work here; the anagram and crossword functions return plain lists of strings. There are some examples of fancy queries on the next page."
    , do
      el "h3" $ text "Fancypants Examples!"
      el "ul" $ do
        exaclip "Filter with a regex after: " "filter (=~\"[hid][rae][bon]d\") $ anagram sysDict \"dha?\"" $ el "sup" $ text "*"
        exaclip "Filter with some weirder constraint (in this case, \"is a palindrome\", given five-letter words with fourth letter \"a\"): " "filter (\\a->a==reverse a) $ crossword sysDict \"???a?\"" blank
        exaclip "Count the results, rather than showing them: " "length $ filter (\\a->a==reverse a) $ crossword sysDict \"?????\"" blank
      el "p" $ do
        text "Regex is provided by "
        elAttr "a" ("href" =: "hoogle/?hoogle=Text.Regex.TDFA") $ text "Text.Regex.TDFA."
        text "Packages can be added to the usable set on request."
      divClass "well .small" $ el "small" $ do
        el "sup" $ text "*"
        text "What\'s up with the " >> el "code" (text "$") >> text "? - haskell separates arguments with whitespace, rather than with parentheses and commas. "
        text "The " >> el "code" (text "$") >> text " means \"everything to the right is a single argument, even things with " >> el "code" (text "$") >> text " in them\". Thinking about it as a backwards sort of unix pipe is not far off. So in the count example above, we\'re passing the <code>length</code> function the result of the (two-argument) <code>filter</code> function on a predicate and the result of the call to <code>crossword</code>. If you\'re trying to do some complex query and getting hung up, please do ask for help!</small></div>"
    ]
  where exaclip a b c = el "li" $ do
          text a
          (clickable, _) <- el' "code" $ text b
          tellEvent $ b <$ domEvent Click clickable
          c

