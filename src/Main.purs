module Main where

import Prelude

import Data.Array (mapMaybe)
import Data.Foldable (traverse_)
import Data.Traversable (traverse)
import Data.Newtype (wrap)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.Component (Component)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

import Web.HTML.HTMLElement (HTMLElement, fromElement, toNode, toParentNode)
import Web.DOM.Node (textContent)
import Web.DOM.ParentNode (children)
import Web.DOM.HTMLCollection (toArray)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body

data Action = UpdateText | Bold

editorRef :: H.RefLabel
editorRef = H.RefLabel "editor"

editorToolbar :: forall a. HH.HTML a Action
editorToolbar = HH.div
  [ HP.classes [ wrap "box" ] ]
  [ HH.a []
    [ HH.span [ HP.classes [ wrap "fa", wrap "fa-bold", wrap "fa-fw"]
              , HE.onClick \_ -> Bold
              ] []
    ]
  ]

editorForm :: forall a. HH.HTML a Action
editorForm =
  HH.div
  [ HP.classes [ wrap "box", wrap "editor" ]
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
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
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
    UpdateText -> H.getHTMLElementRef editorRef >>= traverse_ \el -> do
      txt <- liftEffect $ fetchTextBuffer el
      H.modify_ \_ -> txt
    Bold -> pure unit

fetchTextBuffer :: HTMLElement -> Effect (Array String)
fetchTextBuffer el = do
  children <- mapMaybe fromElement <$> (toArray =<< children (toParentNode el))
  traverse (textContent <<< toNode) children
