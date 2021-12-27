module ElmModule.Update exposing (update)

-- Elm -------------------------------------------------------------------------

-- Local Module ----------------------------------------------------------------
import Struct.Event
import Struct.Model

import Update.Story

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
update : (
      Struct.Event.Type ->
      Struct.Model.Type ->
      (Struct.Model.Type, (Cmd Struct.Event.Type))
   )
update event model =
   case event of
      Struct.Event.None -> (model, Cmd.none)
      (Struct.Event.ChoiceSelected ix) ->
         ((Update.Story.select_choice ix model), Cmd.none)
