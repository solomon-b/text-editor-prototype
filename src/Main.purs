module Main where

import Effect.Console
import Prelude

import Data.Array (mapMaybe)
import Data.Foldable (elem, fold, traverse_)
import Data.Maybe (Maybe(..), maybe)
import Data.Int (fromString)
import Data.Newtype (wrap)
import Data.Traversable (for, traverse)
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
import Web.DOM.Node (nodeName, textContent)
import Web.DOM.ParentNode as PN
import Web.Event.Event as E
import Web.UIEvent.KeyboardEvent as KE
import Web.HTML.HTMLElement (HTMLElement, fromElement, toNode, toParentNode, focus)

import Web.UIEvent.KeyboardEvent

-- TODO: Clean up the node conversions using a typeclass
-- TODO: Parse nodes to generate markdown
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
          , HH.div [ HP.classes [ wrap "column", wrap "is-two-thirds" ] ]
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
    KeyPress e | key e == "u" && ctrlKey e -> do
      liftEffect $ E.preventDefault (KE.toEvent e)
      handleAction $ ApplyStyle Underline
    KeyPress e | key e `elem` ["0", "1", "2", "3", "4", "5", "6"] && ctrlKey e ->
      case fromString (key e) of
        Just n -> do
          handleAction $ MkHeading n
        Nothing -> pure unit
    KeyPress e -> pure unit
    UpdateText -> H.getHTMLElementRef editorRef >>= traverse_ \el -> do
      txt <- liftEffect $ fetchTextBuffer el
      H.modify_ \state -> state { buffer = txt }
    ApplyStyle style -> do
      focusEditor
      liftEffect $ applyStyle style
      case style of
        Bold -> H.modify_ \state -> state { toolbarState { bold = not state.toolbarState.bold }}
        Italic -> H.modify_ \state -> state { toolbarState { italic = not state.toolbarState.italic }}
        Underline -> H.modify_ \state -> state { toolbarState { underline = not state.toolbarState.underline }}
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
bufferPreview = HH.div_ <<< map (HH.div_ <<< pure <<< HH.text)

---------------
--- Actions ---
---------------

data Action = Initialize | KeyPress KeyboardEvent | UpdateText | ApplyStyle Style | MkList | MkHeading Int

data Style = Bold | Italic | Underline

applyStyle :: Style -> Effect Unit
applyStyle style = case style of
  Bold -> F.bold
  Italic -> F.italic
  Underline -> F.underline

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
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap (if state.underline then "is-info" else "is-dark") ], HE.onClick \_ -> ApplyStyle Underline ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-underline", wrap "fa-fw"] ] [] ]
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap "is-dark" ], HE.onClick \_ -> MkList ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-list", wrap "fa-fw" ] ] [] ]
  ]

editorForm :: forall a. HH.HTML a Action
editorForm =
  HH.div
  [ HP.classes [ wrap "content", wrap "is-fullheight" ]
  , HP.style "height: 70vh;"
  , HP.id "editor"
  , HP.ref editorRef
  , HH.attr (HH.AttrName "contenteditable") "true"
  , HE.onKeyUp \_ -> UpdateText
  , HE.onKeyDown \e -> KeyPress e
  ] []

---------------------------
--- Text Buffer Parsing ---
---------------------------

fetchTextBuffer :: HTMLElement -> Effect (Array String)
fetchTextBuffer el = do
  children <- mapMaybe fromElement <$> (toArray =<< PN.children (toParentNode el))
  traverse (textContent <<< toNode) children

-- TODO:
parseNode :: HTMLElement -> Effect String
parseNode el = do
  let n = toNode el
  childCollection <- PN.children $ toParentNode el
  childElements <- toArray childCollection
  map fold $ for childElements $ \el' -> do
    let n' = El.toNode el'
    txt <- textContent n'
    --log (nodeName n')
    if nodeName n' == "B" then pure ("**" <> txt <> "**") else pure txt
