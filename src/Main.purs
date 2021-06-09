module Main where

import Effect.Console
import Prelude
import Web.UIEvent.KeyboardEvent

import Data.Array (mapMaybe, replicate)
import Data.Foldable (elem, fold, foldMap, intercalate, traverse_)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Traversable (for, for_, traverse)
import Data.Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Format as F
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.DOM.Element as El
import Web.DOM.HTMLCollection (toArray)
import Web.DOM.Node as N
import Web.DOM.NodeList as NL
import Web.DOM.ParentNode as PN
import Web.Event.Event as E
import Web.HTML.HTMLElement (HTMLElement, fromElement, toNode, toParentNode, focus)
import Web.UIEvent.KeyboardEvent as KE

-- TODO: Parse and render markdown input into editor.
-- TODO: Calculate buffer diffs.

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall query input output. Component query input output Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  initialState :: forall a. a -> State
  initialState _ = { buffer: [], toolbarState: { bold: false, italic: false, underline: false } }

  render state =
    HH.section
      [ HP.id "section" ]
      [ HH.div [ HP.class_ $ wrap "columns" ]
          [ HH.div [ HP.class_ $ wrap "column" ] []
          , HH.div [ HP.classes [ wrap "column", wrap "is-5" ] ]
            [ editorToolbar state.toolbarState
            , editorForm
            , bufferPreview state.buffer
            ]
          , HH.div [ HP.class_ $ wrap "column" ] []
          ]
      ]

  handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
  handleAction = case _ of
    Initialize -> do
      focusEditor
      handleAction (MkHeading 0)
    KeyPress e | key e == "b" && ctrlKey e -> do
      liftEffect $ E.preventDefault (KE.toEvent e)
      handleAction $ ApplyStyle Bold
    KeyPress e | key e == "i" && ctrlKey e -> do
      liftEffect $ E.preventDefault (KE.toEvent e)
      handleAction $ ApplyStyle Italic
    KeyPress e | key e `elem` ["0", "1", "2", "3", "4", "5", "6"] && ctrlKey e ->
      case fromString (key e) of
        Just n -> do
          focusEditor
          handleAction $ MkHeading n
        Nothing -> pure unit
    KeyPress e -> pure unit
    UpdateText -> H.getHTMLElementRef editorRef >>= traverse_ \el -> do
      lines <- liftEffect $ flattenNodes $ toNode el
      let buffer' = map printLine lines
      H.modify_ \state -> state { buffer = buffer' }
    ApplyStyle style -> do
      focusEditor
      liftEffect $ applyStyle style
      case style of
        Bold -> H.modify_ \state -> state { toolbarState { bold = not state.toolbarState.bold }}
        Italic -> H.modify_ \state -> state { toolbarState { italic = not state.toolbarState.italic }}
    MkList -> do
      focusEditor
      liftEffect F.mkList
    MkHeading i -> do
      focusEditor
      liftEffect $ F.mkHeading i

-------------
--- State ---
-------------

type ToolbarState = { bold :: Boolean, italic :: Boolean, underline :: Boolean }
type State = { buffer :: Array String, toolbarState :: ToolbarState }

bufferPreview :: forall a b. Array String -> HH.HTML a b
bufferPreview xs = HH.div_ $ map (HH.div_ <<< pure <<< HH.text) xs --HH.p_ <<< pure <<< HH.text

---------------
--- Actions ---
---------------

data Action = Initialize | KeyPress KeyboardEvent | UpdateText | ApplyStyle Style | MkList | MkHeading Int

data Style = Bold | Italic

applyStyle :: Style -> Effect Unit
applyStyle style = case style of
  Bold -> F.bold
  Italic -> F.italic

-------------------
--- Editor Form ---
-------------------

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

focusEditor :: forall a b c d. H.HalogenM a b c d Aff Unit
focusEditor = do
  editor <- H.getHTMLElementRef editorRef
  liftEffect $ maybe mempty focus editor

editorToolbar :: forall a. ToolbarState -> HH.HTML a Action
editorToolbar state = HH.div
  [ HP.classes [ wrap "box" ] ]
  [ HH.div [ HP.classes [ wrap "select" ]]
      [ HH.select []
          [ HH.option [ HE.onClick \_ -> MkHeading 0]
             [HH.text "Normal"]
          , HH.option [ HE.onClick \_ -> MkHeading 1]
             [HH.text "Heading 1"]
          , HH.option [ HE.onClick \_ -> MkHeading 2]
             [HH.text "Heading 2"]
          , HH.option [ HE.onClick \_ -> MkHeading 3]
             [HH.text "Heading 3"]
          , HH.option [ HE.onClick \_ -> MkHeading 4]
             [HH.text "Heading 4"]
          , HH.option [ HE.onClick \_ -> MkHeading 5]
             [HH.text "Heading 5"]
          , HH.option [ HE.onClick \_ -> MkHeading 6]
             [HH.text "Heading 6"]
          ]
      ]
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap (if state.bold then "is-info" else "is-dark") ], HE.onClick \_ -> ApplyStyle Bold ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-bold", wrap "fa-fw" ] ] [] ]
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap (if state.italic then "is-info" else "is-dark") ], HE.onClick \_ -> ApplyStyle Italic ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-italic", wrap "fa-fw" ]] [] ]
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap "is-dark" ], HE.onClick \_ -> MkList ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-list", wrap "fa-fw" ] ] [] ]
  ]

editorForm :: forall a. HH.HTML a Action
editorForm =
  HH.div
  [ HP.classes [ wrap "content", wrap "is-fullheight" ]
  , HP.style "height: 70vh; 0px solid transparent;"
  , HP.id "editor"
  , HP.ref editorRef
  , HH.attr (HH.AttrName "contenteditable") "true"
  , HE.onKeyUp \_ -> UpdateText
  , HE.onKeyDown \e -> KeyPress e
  ] []

---------------------------
--- Text Buffer Parsing ---
---------------------------

data Line = Heading Int (Array Line) | ListItem (Array Line) | Text String | Ann (Array Line) Style

{-

<p>hello <b>are <i>you</i> okay</b></p>

Ann [Text "Hello", Ann [Text "are", Ann [Text "you"] Italic , Text "okay"] Bold]

-}

flattenNodes :: N.Node -> Effect (Array Line)
flattenNodes n
  | N.nodeTypeIndex n == 3 = (pure <<< Text) <$> N.textContent n
  | N.nodeTypeIndex n == 1 = do
       children' <- NL.toArray =<< N.childNodes n
       txts <- fold <$> traverse flattenNodes children'
       case N.nodeName n of
         "B" -> pure [Ann txts Bold]
         "I" -> pure [Ann txts Italic]
         "LI" -> pure [ListItem txts]
         "H1" -> pure [Heading 1 txts]
         "H2" -> pure [Heading 2 txts]
         "H3" -> pure [Heading 3 txts]
         "H4" -> pure [Heading 4 txts]
         "H5" -> pure [Heading 5 txts]
         "H6" -> pure [Heading 6 txts]
         _ -> pure txts
  | otherwise = pure []

printLine :: Line -> String
printLine (Heading i r) = fold (replicate i "#") <> " " <> foldMap printLine r
printLine (Text txt) = txt
printLine (ListItem r) = "- " <> foldMap printLine r
printLine (Ann [] _) = ""
printLine (Ann r ann) =
  let txt = foldMap printLine r
  in case ann of
       Bold -> "**" <> txt <> "**"
       Italic -> "*" <> txt <> "**"
