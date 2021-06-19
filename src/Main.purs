module Main where

import Prelude

import Caret as C
import Data.Array (findIndex, index, length, replicate, splitAt, updateAt)
import Data.Foldable (fold, foldMap)
import Data.Int (fromString)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Editor as Editor
import Effect (Effect)
import Effect.Class (liftEffect, class MonadEffect)
import Effect.Console (log)
import Format as F
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Elements.Keyed as HHK
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.Event (eventListener)
import Halogen.VDom.Driver (runUI)
import Web.DOM.Node as N
import Web.DOM.NodeList as NL
import Web.Event.Event as E
import Web.HTML.HTMLElement (focus, toEventTarget)
import Web.UIEvent.InputEvent as IE
import Web.UIEvent.InputEvent.EventTypes as IET
import Web.UIEvent.KeyboardEvent as KE

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

component :: forall query input output m. MonadEffect m => Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }

-------------
--- State ---
-------------

type Id = Int
type Content = String

data BufferRow = Row Id Content

instance showBuffer :: Show BufferRow where
  show (Row id content) = "Row " <> show id <> " " <> content

type ToolbarState = { bold :: Boolean, italic :: Boolean, underline :: Boolean }
type State = { buffer :: Array BufferRow, toolbarState :: ToolbarState, freshIds :: Stream Int }

initialState :: forall a. a -> State
initialState _ = { buffer: [(Row 0 "")], toolbarState: { bold: false, italic: false, underline: false }, freshIds: idStream }

data Stream a = Stream a (Lazy (Stream a))

headS :: forall a. Stream a -> a
headS (Stream a _) = a

tailS :: forall a. Stream a -> Stream a
tailS (Stream _ as) = force as

idStream :: Stream Int
idStream = go 1
  where
    go i = Stream i (defer $ \_ -> go $ i + 1)

fetchFreshVariable :: forall output m. MonadEffect m => H.HalogenM State Action () output m Int
fetchFreshVariable = do
  fresh <- H.gets $ \s -> headS $ s.freshIds
  H.modify_ $ \s -> s { freshIds = tailS s.freshIds }
  pure fresh

getCaretOffset :: forall output m. MonadEffect m => H.HalogenM State Action () output m Int
getCaretOffset = liftEffect $ C.getOffset

setCaretOffset :: forall output m. MonadEffect m => Int -> H.HalogenM State Action () output m Unit
setCaretOffset offset = liftEffect $ C.shiftOffset offset

---------------
--- Actions ---
---------------

data Action = Initialize | InputChanged String String | KeyPress Int KE.KeyboardEvent | ApplyStyle Style | MkList | MkHeading Int

data Style = Bold | Italic

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction action =
  case action of
    Initialize -> do
      focusRow 0
      editorDiv' <- H.getHTMLElementRef $ H.RefLabel $ show 0
      case editorDiv' of
        Nothing -> pure unit
        Just editorDiv -> do
          H.subscribe' \_ ->
            let toInputChanged r = InputChanged (r.id) (r.content)
            in eventListener IET.input (toEventTarget editorDiv) (map (toInputChanged <<< Editor.getValue) <<< IE.fromEvent)
    InputChanged id str -> do
      offset <- getCaretOffset
      H.modify_ $ \s ->
        let buffer' = do
              i <- findIndex (\(Row id' _) -> id == show id') s.buffer
              id' <- fromString id
              updateAt i (Row id' str) s.buffer
        in s { buffer = maybe s.buffer identity buffer' }
      liftEffect $ do
        log $ "INPUT CHANGED: " <> id <> " " <> str
      setCaretOffset offset
    KeyPress r e | KE.key e == "Enter" -> do
      liftEffect $ do
        E.preventDefault (KE.toEvent e)
        E.stopPropagation (KE.toEvent e)
      addRow r
      focusRow (r + 1)
    KeyPress r e | KE.key e == "ArrowUp" -> do
      liftEffect $ E.preventDefault (KE.toEvent e)
      focusRow (r - 1)
    KeyPress r e | KE.key e == "ArrowDown" -> do
      liftEffect $ E.preventDefault (KE.toEvent e)
      focusNextRow r
    KeyPress _ _ -> do
      s <- H.get
      offset <- getCaretOffset
      liftEffect $ do
        log $ "CARET OFFSET: " <> show offset
        log $ show $ s.buffer
    ApplyStyle style -> do
      liftEffect $ applyStyle style
      case style of
        Bold -> H.modify_ \state -> state { toolbarState { bold = not state.toolbarState.bold }}
        Italic -> H.modify_ \state -> state { toolbarState { italic = not state.toolbarState.italic }}
    MkList -> do
      liftEffect F.mkList
    MkHeading i -> do
      liftEffect $ F.mkHeading i

applyStyle :: Style -> Effect Unit
applyStyle style = case style of
  Bold -> F.bold
  Italic -> F.italic

focusRow :: forall a b c m. MonadEffect m => Int -> H.HalogenM State a b c m Unit
focusRow i = do
  row <- H.gets $ \s -> index s.buffer i
  liftEffect $ log $ show row
  case row of
    Nothing -> pure unit
    Just (Row id _) -> do
      elem <- H.getHTMLElementRef $ H.RefLabel $ show id
      liftEffect $ do
        offSet <- C.getOffset
        maybe (liftEffect (log $ "did not find elem " <> show id)) focus elem
        C.shiftOffset offSet

focusNextRow :: forall a b c m. MonadEffect m => Int -> H.HalogenM State a b c m Unit
focusNextRow id = do
  buffer <- H.gets $ \s -> s.buffer
  case findIndex (\(Row id' _) -> id == id') buffer of
    Just j | j < length buffer - 1 -> do
      focusRow (j + 1)
    _ -> pure unit

addRow :: forall o m. MonadEffect m => Int -> H.HalogenM State Action () o m Unit
addRow focusedRow = do
  liftEffect $ log $ show $ "focusedRow: " <> show focusedRow
  i <- fetchFreshVariable

  H.modify_ \state ->
    let j = maybe (length state.buffer) identity $ findIndex (\(Row id _) -> id == focusedRow) state.buffer
        res = splitAt (j + 1) state.buffer
    in state { buffer = res.before <> [Row i ""] <> res.after }

  -- FFI call to insert new HTML Element
  --liftEffect $ Editor.insertDiv (show focusedRow) (show i)

  rowTag' <- H.getHTMLElementRef $ H.RefLabel $ show i
  case rowTag' of
    Nothing -> pure unit
    Just rowTag -> do
      H.subscribe' \_ ->
        let toInputChanged r' = InputChanged (r'.id) (r'.content)
        in eventListener IET.input (toEventTarget rowTag) (map (toInputChanged <<< Editor.getValue) <<< IE.fromEvent)

--------------
--- Render ---
--------------

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.section
    [ HP.id "section" ]
    [ HH.div [ HP.class_ $ wrap "columns" ]
        [ HH.div [ HP.class_ $ wrap "column" ] []
        , HH.div [ HP.classes [ wrap "column", wrap "is-5" ] ]
          [ editorToolbar state.toolbarState
          , editorForm state.buffer
          ]
        , HH.div [ HP.class_ $ wrap "column" ] []
        ]
    ]

bufferPreview :: forall a b. Array String -> HH.HTML a b
bufferPreview xs = HH.div_ $ map (HH.div_ <<< pure <<< HH.text) xs


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

editorForm :: forall a. Array BufferRow -> HH.HTML a Action
editorForm rows =
  HH.div [ HP.id "editor", HP.ref editorRef ] $ map editorRow rows

editorRow :: forall a. BufferRow -> HH.HTML a Action
editorRow (Row id str) =
  HHK.div_ <<< pure <<< Tuple (show id) $
  HH.p
  [ HP.classes [ wrap "content" ]
  , HP.id $ show id
  , HP.ref $ H.RefLabel $ show id
  , HH.attr (HH.AttrName "contenteditable") "true"
  , HE.onKeyDown \e -> KeyPress id e
  ] [ HH.text str ]

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
