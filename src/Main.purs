module Main where

import Prelude

import Data.Array (mapMaybe)
import Data.Foldable (fold, traverse_)
import Data.Maybe (Maybe(..), maybe, isJust)
import Data.Traversable (for, traverse)
import Data.Newtype (wrap)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLElement (HTMLElement, fromElement, toNode, toParentNode, focus)
import Web.DOM.Element as El
import Web.DOM.Node (Node, nodeName, textContent)
import Web.DOM.ParentNode as PN
import Web.DOM.HTMLCollection (toArray)

import Effect.Console

import Format as F

main :: Effect Unit
main = HA.runHalogenAff do
  editor <- liftAff $ getElementById "foo"
  liftEffect $ log $ show $ isJust editor
  body <- HA.awaitBody
  runUI component unit body

data Action = Initialize | UpdateText | Bold | Italic | Underline | MkList | MkHeading Int

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

editorToolbar :: forall a. HH.HTML a Action
editorToolbar = HH.div
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
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap "is-info" ] ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-bold", wrap "fa-fw"]
              , HE.onClick \_ -> Bold
              ] []
    ]
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap "is-info" ] ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-italic", wrap "fa-fw"]
              , HE.onClick \_ -> Italic
              ] []
    ]
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap "is-info" ] ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-underline", wrap "fa-fw"]
              , HE.onClick \_ -> Underline
              ] []
    ]
  , HH.button [ HP.classes [ wrap "button", wrap "is-inverted", wrap "is-info" ] ]
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-list", wrap "fa-fw"]
              , HE.onClick \_ -> MkList
              ] []
    ]
  ]

editorForm :: forall a. HH.HTML a Action
editorForm =
  HH.div
  [ HP.classes [ wrap "box", wrap "content" ]
  , HP.id "editor"
  , HP.ref editorRef
  , HH.attr (HH.AttrName "contenteditable") "true"
  , HE.onKeyUp \_ -> UpdateText
  ] []

statePreview :: forall a b. Array String -> HH.HTML a b
statePreview = HH.div_ <<< map (HH.div_ <<< pure <<< HH.text)

component :: forall query input output. Component query input output Aff
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  initialState :: forall a. a -> Array String
  initialState _ = []

  render state =
    HH.section
      [ HP.id "section" ]
      [ HH.div [ HP.class_ $ wrap "columns" ]
          [ HH.div [ HP.class_ $ wrap "column" ] []
          , HH.div [ HP.classes [ wrap "column", wrap "is-two-thirds" ] ]
            [ editorToolbar
            , editorForm
            , statePreview state
            ]
          , HH.div [ HP.class_ $ wrap "column" ] []
          ]
      ]

  handleAction :: forall o. Action -> H.HalogenM (Array String) Action () o Aff Unit
  handleAction = case _ of
    Initialize -> do
      editor <- H.getHTMLElementRef editorRef
      liftEffect $ maybe mempty focus editor
      handleAction (MkHeading 0)
    UpdateText -> H.getHTMLElementRef editorRef >>= traverse_ \el -> do
      txt <- liftEffect $ fetchTextBuffer el
      H.modify_ \_ -> txt
    Bold -> do
      editor <- H.getHTMLElementRef editorRef
      liftEffect $ maybe mempty focus editor
      liftEffect F.bold
    Italic -> do
      editor <- H.getHTMLElementRef editorRef
      liftEffect $ maybe mempty focus editor
      liftEffect F.italic
    Underline -> do
      editor <- H.getHTMLElementRef editorRef
      liftEffect $ maybe mempty focus editor
      liftEffect F.underline
    MkList -> do
      editor <- H.getHTMLElementRef editorRef
      liftEffect $ maybe mempty focus editor
      liftEffect F.mkList
    MkHeading i -> do
      editor <- H.getHTMLElementRef editorRef
      liftEffect $ maybe mempty focus editor
      liftEffect $ F.mkHeading i

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
