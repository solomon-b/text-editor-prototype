module Main where

import Effect.Console
import Prelude

import Data.Array (length, mapMaybe, replicate, splitAt, (..), zip)
import Data.Foldable (elem, fold, foldMap, intercalate, traverse_)
import Data.Int (fromString)
import Data.Lens
import Data.Lens.Index
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Traversable (for, for_, traverse)
import Data.Tuple
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

import Caret as C
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
  initialState _ = { buffer: [""], focus: 0, toolbarState: { bold: false, italic: false, underline: false } }

  render state =
    HH.section
      [ HP.id "section" ]
      [ HH.div [ HP.class_ $ wrap "columns" ]
          [ HH.div [ HP.class_ $ wrap "column" ] []
          , HH.div [ HP.classes [ wrap "column", wrap "is-5" ] ]
            [ editorToolbar state.toolbarState
            , editorForm state.buffer
            --, bufferPreview state.buffer
            ]
          , HH.div [ HP.class_ $ wrap "column" ] []
          ]
      ]

  handleAction :: forall o. Action -> H.HalogenM State Action () o Aff Unit
  handleAction action = do
    updateBuffer
    s <- H.get
    liftEffect $ log $ show s.buffer
    case action of
      Initialize -> do
        focusRow 0
      --KeyPress _ e | key e == "b" && ctrlKey e -> do
      --  liftEffect $ E.preventDefault (KE.toEvent e)
      --  handleAction $ ApplyStyle Bold
      --KeyPress _ e | key e == "i" && ctrlKey e -> do
      --  liftEffect $ E.preventDefault (KE.toEvent e)
      --  handleAction $ ApplyStyle Italic
      --KeyPress _ e | key e `elem` ["0", "1", "2", "3", "4", "5", "6"] && ctrlKey e ->
      --  case fromString (key e) of
      --    Just n -> do
      --      focusRow
      --      handleAction $ MkHeading n
      --    Nothing -> pure unit
      KeyPress r e | KE.key e == "Enter" -> do
        liftEffect $ E.preventDefault (KE.toEvent e)
        addRow r
        focusRow (r + 1)
      KeyPress r e | KE.key e == "ArrowUp" -> do
        liftEffect $ E.preventDefault (KE.toEvent e)
        focusRow (r - 1)
      KeyPress r e | KE.key e == "ArrowDown" -> do
        liftEffect $ E.preventDefault (KE.toEvent e)
        focusRow (r + 1)
      KeyPress r e -> do
        liftEffect $ log $ KE.key e
        pure unit
--      UpdateText -> H.getHTMLElementRef editorRef >>= traverse_ \el -> do
--        lines <- liftEffect $ flattenNodes $ toNode el
--        let buffer' = map printLine lines
--        H.modify_ \state -> state { buffer = buffer' }
      ApplyStyle style -> do
        --focusRow
        liftEffect $ applyStyle style
        case style of
          Bold -> H.modify_ \state -> state { toolbarState { bold = not state.toolbarState.bold }}
          Italic -> H.modify_ \state -> state { toolbarState { italic = not state.toolbarState.italic }}
      MkList -> do
        --focusRow
        liftEffect F.mkList
      MkHeading i -> do
        --focusRow
        liftEffect $ F.mkHeading i

-------------
--- State ---
-------------

type ToolbarState = { bold :: Boolean, italic :: Boolean, underline :: Boolean }
type State = { buffer :: Array String, focus :: Int, toolbarState :: ToolbarState }

bufferPreview :: forall a b. Array String -> HH.HTML a b
bufferPreview xs = HH.div_ $ map (HH.div_ <<< pure <<< HH.text) xs

---------------
--- Actions ---
---------------

data Action = Initialize | KeyPress Int KE.KeyboardEvent | ApplyStyle Style | MkList | MkHeading Int

data Style = Bold | Italic

applyStyle :: Style -> Effect Unit
applyStyle style = case style of
  Bold -> F.bold
  Italic -> F.italic

updateBuffer :: forall a b c. H.HalogenM State a b c Aff Unit
updateBuffer = do
  editorDiv' <- H.getHTMLElementRef editorRef
  case editorDiv' of
    Nothing -> pure unit
    Just editorDiv -> do
      let n = toNode editorDiv
      children' <- liftEffect (NL.toArray =<< N.childNodes n)
      newBuffer <- liftEffect $ traverse N.textContent children'
      H.modify_ \state -> state { buffer = newBuffer }

focusRow :: forall a b c d. Int -> H.HalogenM a b c d Aff Unit
focusRow i = do
  row <- H.getHTMLElementRef $ H.RefLabel $ show i
  offSet <- liftEffect $ C.getOffset
  liftEffect $ do
    maybe mempty focus row
    C.shiftOffset offSet

addRow :: forall o. Int -> H.HalogenM State Action () o Aff Unit
addRow i =
  H.modify_ \state ->
    let res = splitAt (i + 1) state.buffer
    in state { buffer = res.before <> [""] <> res.after }

-------------------
--- Editor Form ---
-------------------

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

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

editorForm :: forall a. Array String -> HH.HTML a Action
editorForm rows =
  HH.div [ HP.id "editor", HP.ref editorRef ] $ map editorRow (0 .. (length rows - 1))

editorRow :: forall a. Int -> HH.HTML a Action
editorRow id =
  HH.p
  [ HP.classes [ wrap "content" ]
  , HP.id $ show id
  , HP.ref $ H.RefLabel $ show id
  , HH.attr (HH.AttrName "contenteditable") "true"
  , HE.onKeyDown \e -> KeyPress id e
  ] [ ]

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
