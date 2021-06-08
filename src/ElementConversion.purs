module ElementConversion where

--import Prelude
--
--import Data.Maybe (Maybe)
--import Web.DOM.ChildNode as CN
--import Web.DOM.Element as EL
--import Web.DOM.Node as N
--import Web.DOM.NonDocumentTypeChildNode as NDTCN
--import Web.DOM.ParentNode as PN
--import Web.Event.EventTarget as ET
--import Web.HTML.HTMLElement as HEL
--
--class Convertable a where
--  toHTMLElement :: a -> Maybe HEL.HTMLElement
--  toElement :: a -> Maybe EL.Element
--  toNode :: a -> N.Node
--  toParentNode :: a -> PN.ParentNode
--  toChildNode :: a -> CN.ChildNode
--  toNonDocumentTypeChildNode :: a -> NDTCN.NonDocumentTypeChildNode
--  toEventTarget :: a -> ET.EventTarget
--
--instance convertElement :: Convertable EL.Element where
--  toHTMLElement = HEL.fromElement
--  toElement = pure
--  toNode = EL.toNode
--  toParentNode =  EL.toParentNode
--  toChildNode = EL.toChildNode
--  toNonDocumentTypeChildNode = EL.toNonDocumentTypeChildNode
--  toEventTarget = EL.toEventTarget
--
--instance convertNode :: Convertable N.Node where
--  toHTMLElement = HEL.fromNode
--  toElement = EL.fromNode
--  toNode = identity
--  toParentNode =
{-

HTMLElement
Element
Node
ParentNode
ChildNode
NonDocumentTypeChildNode
EventTraget

--------------------------
-- Web.HTML.HTMLElement --
--------------------------

fromElement :: Element -> Maybe HTMLElement
fromNode :: Node -> Maybe HTMLElement
fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe HTMLElement
fromParentNode :: ParentNode -> Maybe HTMLElement
fromEventTarget :: EventTarget -> Maybe HTMLElement

toElement :: HTMLElement -> Element
toNode :: HTMLElement -> Node
toChildNode :: HTMLElement -> ChildNode
toNonDocumentTypeChildNode :: HTMLElement -> NonDocumentTypeChildNode
toParentNode :: HTMLElement -> ParentNode
toEventTarget :: HTMLElement -> EventTarget

---------------------
-- Web.DOM.Element --
---------------------

fromNode :: Node -> Maybe Element
fromChildNode :: ChildNode -> Maybe Element
fromNonDocumentTypeChildNode :: NonDocumentTypeChildNode -> Maybe Element
fromParentNode :: ParentNode -> Maybe Element
fromEventTarget :: EventTarget -> Maybe Element

toNode :: Element -> Node
toChildNode :: Element -> ChildNode
toNonDocumentTypeChildNode :: Element -> NonDocumentTypeChildNode
toParentNode :: Element -> ParentNode
toEventTarget :: Element -> EventTarget

------------------
-- Web.DOM.Node --
------------------

fromEventTarget :: EventTarget -> Maybe Node

toEventTarget :: Node -> EventTarget


-}
