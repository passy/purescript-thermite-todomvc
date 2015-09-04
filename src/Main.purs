module Main (main) where

import Prelude

import Data.Tuple
import Data.Maybe (fromMaybe)
import Data.Maybe.Unsafe (fromJust)
import Data.Nullable (toMaybe)
import Data.Foreign (toForeign)
import Data.Foreign.Lens
import Data.List ( List(..)
                 , (:)
                 , deleteAt
                 , updateAt
                 , filter
                 , fromList
                 , length
                 , range
                 , singleton
                 , zip
                 )

import Control.Plus (empty)
import Control.Monad.Eff

import Optic.Core
import Optic.Monad ((#~))
import Optic.Index (ix)
import Optic.Monad.Setter ((.=), (++=))

import qualified Thermite as T
import qualified Thermite.Action as T

import qualified React as R
import qualified React.DOM as RD
import qualified React.DOM.Props as RP

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Types as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Types as DOM

type Index = Int

data Action
  = NewItem String
  | RemoveItem Index
  | SetEditText String
  | SetCompleted Index Boolean
  | SetFilter Filter
  | DoNothing

data Item = Item String Boolean

data Filter = All | Active | Completed

instance eqFilter :: Eq Filter where
  eq All       All       = true
  eq Active    Active    = true
  eq Completed Completed = true
  eq _         _         = false

showFilter :: Filter -> String
showFilter All = "All"
showFilter Active = "Active"
showFilter Completed = "Completed"

data State = State
  { items       :: List Item
  , editText    :: String
  , filter      :: Filter
  }

_State :: LensP State { items :: _, editText :: _, filter :: _ }
_State f (State st) = State <$> f st

items :: forall r. LensP { items :: _ | r } _
items f st = f st.items <#> \i -> st { items = i }

editText :: forall r. LensP { editText :: _ | r } _
editText f st = f st.editText <#> \i -> st { editText = i }

filter_ :: forall r. LensP { filter :: _ | r } _
filter_ f st = f st.filter <#> \i -> st { filter = i }

itemBoolean :: LensP Item Boolean
itemBoolean f (Item str b) = Item str <$> f b

getValue :: forall event. event -> String
getValue = fromMaybe "" <<< get (prop "target" <<< prop "value" <<< string) <<< toForeign

getChecked :: forall event. event -> Boolean
getChecked = fromMaybe false <<< get (prop "target" <<< prop "checked" <<< boolean) <<< toForeign

getKeyCode :: forall event. event -> Int
getKeyCode = fromMaybe 0 <<< get (prop "keyCode" <<< int) <<< toForeign

handleKeyPress :: forall event. event -> Action
handleKeyPress e = case getKeyCode e of
                     13 -> NewItem $ getValue e
                     27 -> SetEditText ""
                     _  -> DoNothing

handleChangeEvent :: forall event. event -> Action
handleChangeEvent e = SetEditText (getValue e)

handleCheckEvent :: forall event. Index -> event -> Action
handleCheckEvent index e = SetCompleted index (getChecked e)

initialState :: State
initialState = State { items: empty, editText: "", filter: All }

applyFilter :: Filter -> Item -> Boolean
applyFilter All       _ = true
applyFilter Active    (Item _ b) = not b
applyFilter Completed (Item _ b) = b

render :: T.Render _ State _ Action
render send (State st) _ _ =
  RD.div [ RP.className "container" ] [ title, filters, items ]
  where
  title :: R.ReactElement
  title = RD.h1' [ RD.text "todos" ]

  items :: R.ReactElement
  items = RD.table [ RP.className "table table-striped" ]
                   [ RD.thead' [ RD.th [ RP.className "col-md-1"  ] []
                               , RD.th [ RP.className "col-md-10" ] [ RD.text "Description" ]
                               , RD.th [ RP.className "col-md-1"  ] [] 
                               ]
                   , RD.tbody' (fromList (newItem : (map item <<< filter (applyFilter st.filter <<< fst) $ zip st.items (range 0 $ length st.items))))
                   ]

  newItem :: R.ReactElement
  newItem = RD.tr' [ RD.td' []
                   , RD.td' [ RD.input [ RP.className "form-control"
                                       , RP.placeholder "Create a new task"
                                       , RP.value st.editText
                                       , RP.onKeyUp (send <<< handleKeyPress)
                                       , RP.onChange (send <<< handleChangeEvent)
                                       ] []
                            ]
                   , RD.td' []
                   ]

  item :: Tuple Item Index -> R.ReactElement
  item (Tuple (Item name completed) index) =
    RD.tr' <<< map (RD.td' <<< pure) $ 
          [ RD.input [ RP._type "checkbox"
                     , RP.className "checkbox"
                     , RP.checked (if completed then "checked" else "")
                     , RP.title "Mark as completed"
                     , RP.onChange (send <<< handleCheckEvent index)
                     ] []
          , RD.text name
          , RD.a [ RP.className "btn btn-danger pull-right"
                 , RP.title "Remove item"
                 , RP.onClick \_ -> send (RemoveItem index)
                 ] 
                 [ RD.text "✖" ]
          ]

  filters :: R.ReactElement
  filters = RD.div [ RP.className "btn-group" ] (filter_ <$> [All, Active, Completed])

  filter_ :: Filter -> R.ReactElement
  filter_ f = RD.button [ RP.className (if f == st.filter then "btn toolbar active" else "btn toolbar")
                        , RP.onClick \_ -> send (SetFilter f)
                        ]
                        [ RD.text (showFilter f) ]

performAction :: T.PerformAction _ State _ Action
performAction _ action = T.modifyState (updateState action)
  where
  updateState :: Action -> State -> State
  updateState (NewItem s)        = \st -> st #~ do _State .. items ++= singleton (Item s false)
                                                   _State .. editText .= ""
  updateState (RemoveItem i)     = over (_State .. items) (\xs -> fromMaybe xs (deleteAt i xs))
  updateState (SetEditText s)    = _State .. editText .~ s
  updateState (SetCompleted i c) = _State .. items .. ix i .. itemBoolean .~ c
  updateState (SetFilter f)      = _State .. filter_ .~ f
  updateState DoNothing          = id

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render

main = do
  let component = T.createClass spec
  body >>= R.render (R.createFactory component {}) 
  
  where
  body :: forall eff. Eff (dom :: DOM.DOM | eff) DOM.Element
  body = do
    win <- DOM.window
    doc <- DOM.document win
    elm <- fromJust <$> toMaybe <$> DOM.body doc
    return $ DOM.htmlElementToElement elm
