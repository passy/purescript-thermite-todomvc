module Main (main) where

import Data.Array (map, deleteAt, updateAt, filter, length, range)
import Data.Tuple

import Optic.Core ((..), (.~), LensP())
import Optic.Extended ((.=), (++=), (#~))
import Optic.Index (ix)

import Prelude.Unsafe (unsafeIndex)

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T

type Index = Number

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
  (==) All       All       = true
  (==) Active    Active    = true
  (==) Completed Completed = true
  (==) _         _         = false
  (/=) x         y         = not (x == y)

showFilter :: Filter -> String
showFilter All = "All"
showFilter Active = "Active"
showFilter Completed = "Completed"

data State = State
  { items :: [Item]
  , editText :: String
  , filter :: Filter
  }

_State :: LensP State {items :: _, editText :: _, filter :: _}
_State f (State st) = State <$> f st

items :: forall r. LensP {items :: _ | r} _
items f st = f st.items <#> \i -> st{items = i}

editText :: forall r. LensP {editText :: _ | r} _
editText f st = f st.editText <#> \i -> st{editText = i}

itemBoolean :: LensP Item Boolean
itemBoolean f (Item str b) = Item str <$> f b

foreign import getValue
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: forall event. event -> String

foreign import getChecked
  "function getChecked(e) {\
  \  return e.target.checked;\
  \}" :: T.FormEvent -> Boolean

foreign import getKeyCode
  "function getKeyCode(e) {\
  \  return e.keyCode;\
  \}" :: T.KeyboardEvent -> Number

handleKeyPress :: T.KeyboardEvent -> Action
handleKeyPress e = case getKeyCode e of
                     13 -> NewItem $ getValue e
                     27 -> SetEditText ""
                     _  -> DoNothing

handleChangeEvent :: T.FormEvent -> Action
handleChangeEvent e = SetEditText (getValue e)

handleCheckEvent :: Index -> T.FormEvent -> Action
handleCheckEvent index e = SetCompleted index (getChecked e)

initialState :: State
initialState = State { items: [], editText: "", filter: All }

applyFilter :: Filter -> Item -> Boolean
applyFilter All       _ = true
applyFilter Active    (Item _ b) = not b
applyFilter Completed (Item _ b) = b

render :: T.Render State _ Action
render ctx (State st) _ =
  T.div [ A.className "body" ] [ title, items, filters ]
  where
  title :: T.Html _
  title = T.h1 [ A.className "title" ] [ T.text "todos" ]

  items :: T.Html _
  items = T.ul [ A.className "items" ] (newItem : (map item <<< filter (applyFilter st.filter <<< fst) $ zip st.items (range 0 $ length st.items)))

  newItem :: T.Html _
  newItem = T.li [ A.className "newItem" ]
                 [ T.input [ A.placeholder "Create a new task"
                           , A.value st.editText
                           , T.onKeyUp ctx handleKeyPress
                           , T.onChange ctx handleChangeEvent
                           ] []
                 ]

  item :: Tuple Item Index -> T.Html _
  item (Tuple (Item name completed) index) =
    T.li' [ T.input  [ A._type "checkbox"
                     , A.className "completed"
                     , A.checked (if completed then "checked" else "")
                     , A.title "Mark as completed"
                     , T.onChange ctx (handleCheckEvent index)
                     ] []
          , T.span   [ A.className "description" ] [ T.text name ]
          , T.button [ A.className "complete"
                     , A.title "Remove item"
                     , T.onClick ctx \_ -> RemoveItem index
                     ] [ T.text "✖" ]
          ]

  filters :: T.Html _
  filters = T.ul [ A.className "filters" ] (filter_ <$> [All, Active, Completed])

  filter_ :: Filter -> T.Html _
  filter_ f = T.li [] [ T.a [ A.href "#"
                            , A.className (if f == st.filter then "selected" else "")
                            , T.onClick ctx (\_ -> SetFilter f)
                            ] [ T.text (showFilter f) ]
                      ]

performAction :: T.PerformAction State _ Action _
performAction st         _ (NewItem s)        k = k $ st #~ do
  _State..items ++= [Item s false]
  _State..editText .= ""
performAction (State st) _ (RemoveItem i)     k =
  k (State (st { items = deleteAt i 1 st.items }))
performAction (State st) _ (SetEditText s)    k =
  k (State (st { editText = s }))
performAction (State st) _ (SetCompleted i c) k =
  k (State (st # items..ix i..itemBoolean .~ c))
performAction (State st) _ (SetFilter f)      k =
  k (State (st { filter = f }))
performAction st         _ DoNothing          k =
  return unit

spec :: T.Spec _ State _ Action
spec = T.Spec { initialState: initialState
              , performAction: performAction
              , render: render
              }

main = do
  let component = T.createClass spec
  T.render component {}
