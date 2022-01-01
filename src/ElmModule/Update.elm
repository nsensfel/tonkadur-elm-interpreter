module ElmModule.Update exposing (update)

-- Elm -------------------------------------------------------------------------
import Html

-- Local Module ----------------------------------------------------------------
import Struct.Event
import Struct.Model
import Struct.UI

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
      (Struct.Event.ChoiceSelected ix) ->
         ((Update.Story.select_choice ix model), Cmd.none)

      Struct.Event.None -> (model, Cmd.none)
      (Struct.Event.LoadStory http_result) ->
         case http_result of
            (Ok story) ->
               ((Update.Story.start {model | tonkadur = story}), Cmd.none)

            (Err error) ->
               (
                  {model |
                     ui =
                        -- TODO: display the actual error.
                        (Struct.UI.display_error
                           (Html.text "Failed to load story")
                           model.ui
                        )
                  },
                  Cmd.none
               )
