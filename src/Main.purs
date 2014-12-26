module Main (main) where

import Data.Array (deleteAt)

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as T
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T

type Index = Number

data Action 
  = NewItem String
  | RemoveItem Index
  | DoNothing

data Item = Item String Boolean

data State = State 
  { items :: [Item] 
  , newItemName :: String
  }

foreign import getValue 
  "function getValue(e) {\
  \  return e.target.value;\
  \}" :: T.KeyboardEvent -> String

foreign import getKeyCode
  "function getKeyCode(e) {\
  \  return e.keyCode;\
  \}" :: T.KeyboardEvent -> Number

handleKeyPress :: T.KeyboardEvent -> Action
handleKeyPress e | getKeyCode e == 13 = NewItem $ getValue e
handleKeyPress _ = DoNothing

handleCheckEvent :: T.FormEvent -> Action
handleCheckEvent _ = DoNothing

initialState :: State
initialState = State { items: [], newItemName: "" }

render :: T.Render State _ Action
render ctx (State st) _ =
  T.div [ A.className "body" ] [ title, items ]
  where
  title :: T.Html _
  title = T.h1 [ A.className "title" ] [ T.text "todos" ]

  items :: T.Html _
  items = T.ul [ A.className "items" ] (newItem : (item <$> st.items))
  
  newItem :: T.Html _
  newItem = T.li [ A.className "newItem" ]
                 [ T.input [ A.placeholder "Create a new task" 
                           , T.onKeyUp ctx handleKeyPress
                           ] []
                 ]

  item :: Item -> T.Html _
  item (Item name completed) =
    T.li' [ T.input [ A._type "checkbox"
                    , A.className "completed"
                    , A.value (show completed)
                    , T.onChange ctx handleCheckEvent 
                    ] []
          , T.span [ A.className "description" ] [ T.text name ]
          ]

performAction :: T.PerformAction State _ Action _ 
performAction (State st) _ (NewItem s)    k = 
  k (State (st { items = st.items ++ [ Item s false ], newItemName = "" }))
performAction (State st) _ (RemoveItem i) k = 
  k (State (st { items = deleteAt i 1 st.items }))
performAction st         _ DoNothing      k =
  return unit

spec :: T.Spec _ State _ Action
spec = T.Spec { initialState: initialState
              , performAction: performAction
              , render: render
              }

main = do
  let component = T.createClass spec
  T.render component {}
