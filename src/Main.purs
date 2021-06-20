module Main where

import Prelude

import Caret as C
import Data.Array (findIndex, index, length, replicate, splitAt, updateAt, insertAt)
import Data.Foldable (fold, foldMap)
import Data.Int (fromString)
import Data.Lazy (Lazy, defer, force)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.String (splitAt) as Str
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
data Content = Styled Style Content | P Content | Txt String
data Style = Bold | Italic
data BufferRow = Row Id Content
data ShiftFocus = Up | Down

instance showContent :: Show Content where
  show (Txt content) = content
  show (P content) = "<p>" <> show content <> "</p>"
  show (Styled Bold content) = "<b>" <> show content <> "</b>"
  show (Styled Italic content) = "<i>" <> show content <> "</i>"

instance showBuffer :: Show BufferRow where
  show (Row id content) = "Row " <> show id <> " " <> show content

type ToolbarState = { bold :: Boolean, italic :: Boolean, underline :: Boolean }
type State = { buffer :: Array BufferRow, toolbarState :: ToolbarState, freshIds :: Stream Int }

initialState :: forall a. a -> State
initialState _ = { buffer: [(Row 0 (Txt ""))], toolbarState: { bold: false, italic: false, underline: false }, freshIds: idStream }

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

isBold :: forall output m. MonadEffect m => H.HalogenM State Action () output m Boolean
isBold = H.gets $ \s -> s.toolbarState.bold

isItalic :: forall output m. MonadEffect m => H.HalogenM State Action () output m Boolean
isItalic = H.gets $ \s -> s.toolbarState.italic

---------------
--- Actions ---
---------------

data Action = Initialize | InputChanged String String | KeyPress Int KE.KeyboardEvent | ApplyStyle Style | MkList | MkHeading Int

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
      isBold' <- isBold
      H.modify_ $ \s ->
        let buffer' = do
              i <- findIndex (\(Row id' _) -> id == show id') s.buffer
              id' <- fromString id
              if isBold'
                then updateAt i (Row id' (Styled Bold (Txt str))) s.buffer
                else updateAt i (Row id' (Txt str)) s.buffer
        in s { buffer = maybe s.buffer identity buffer' }
      setCaretOffset offset
    KeyPress r e | KE.key e == "Enter" -> do
      liftEffect $ do
        E.preventDefault (KE.toEvent e)
        E.stopPropagation (KE.toEvent e)
      addRow r
      adjustFocus r Down
    KeyPress r e | KE.key e == "ArrowUp" -> do
      liftEffect $ E.preventDefault (KE.toEvent e)
      adjustFocus r Up
    KeyPress r e | KE.key e == "ArrowDown" -> do
      liftEffect $ E.preventDefault (KE.toEvent e)
      adjustFocus r Down
    KeyPress _ _ -> do
      s <- H.get
      liftEffect $ log $ show $ s.buffer
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

focusRow :: forall a b c m. MonadEffect m => Id -> H.HalogenM State a b c m Unit
focusRow i = do
  row <- H.gets $ \s -> index s.buffer i
  case row of
    Nothing -> pure unit
    Just (Row id _) -> do
      elem <- H.getHTMLElementRef $ H.RefLabel $ show id
      liftEffect $ do
        offSet <- C.getOffset
        maybe (liftEffect (log $ "did not find elem " <> show id)) focus elem
        C.shiftOffset offSet

adjustFocus :: forall a b c m. MonadEffect m => Id -> ShiftFocus -> H.HalogenM State a b c m Unit
adjustFocus id dir = do
  buffer <- H.gets $ \s -> s.buffer
  case findIndex (\(Row id' _) -> id == id') buffer of
    Just j ->
      case dir of
        Up -> focusRow (j - 1)
        Down -> focusRow (j + 1)
    _ -> pure unit

addRow :: forall o m. MonadEffect m => Int -> H.HalogenM State Action () o m Unit
addRow focusedRow = do
  offset <- getCaretOffset
  i <- fetchFreshVariable

  H.modify_ \state -> state { buffer = bufferInsert focusedRow offset i state.buffer }

  rowTag' <- H.getHTMLElementRef $ H.RefLabel $ show i
  case rowTag' of
    Nothing -> pure unit
    Just rowTag -> do
      H.subscribe' \_ ->
        let toInputChanged r' = InputChanged (r'.id) (r'.content)
        in eventListener IET.input (toEventTarget rowTag) (map (toInputChanged <<< Editor.getValue) <<< IE.fromEvent)

contentSplitAt :: Int -> Content -> { before :: Content, after :: Content }
contentSplitAt offset (Txt str) =
  let { before: before, after: after} = Str.splitAt offset str
  in { before: Txt before, after: Txt after }
contentSplitAt offset (P content) =
  let { before: before, after: after} = contentSplitAt offset content
  in { before: P before, after: P after }
contentSplitAt offset (Styled style content) =
  let { before: before, after: after} = contentSplitAt offset content
  in { before: Styled style before, after: Styled style after }

rowSplitAt :: Int -> BufferRow -> Id -> { before :: BufferRow, after :: BufferRow }
rowSplitAt offset (Row i content) j =
  let { before: before, after: after} = contentSplitAt offset content
  in { before: Row i before, after: Row j after}

bufferInsert :: Id -> Int -> Int -> Array BufferRow -> Array BufferRow
bufferInsert id offset j buff =
 let i = maybe (length buff) identity $ findIndex (\(Row id' _) -> id == id') buff
     buff' = do
       x <- index buff i
       let {before: before, after: after} = rowSplitAt offset x j
       insertAt (i + 1) after =<< updateAt i before buff
  in maybe buff identity buff'

---------------
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
editorRow (Row id content) =
  HHK.div_ <<< pure <<< Tuple (show id) $
  HH.p
  [ HP.classes [ wrap "content" ]
  , HE.onKeyDown \e -> KeyPress id e
  ] [ HH.span [ HP.classes $ pure $ wrap "icon-text" ]
      [ HH.span [ HP.classes $ pure $ wrap "icon" ]
        [ HH.i [ HP.classes [ wrap "fas", wrap "fa-grip-lines-vertical"] ] []]
      , HH.span
          [ HP.id $ show id
          , HP.ref $ H.RefLabel $ show id
          , HH.attr (HH.AttrName "contenteditable") "true"
          ] [ HH.text $ show content ]
      ]
    ]

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
