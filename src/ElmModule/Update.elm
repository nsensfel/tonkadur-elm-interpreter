module ElmModule.Update exposing (update)

-- Elm -------------------------------------------------------------------------
import Http

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
      (Struct.Event.UserInputInProgress string) ->
         (
            {model | ui = (Struct.UI.set_field_content string model.ui)},
            Cmd.none
         )

      Struct.Event.UserInputValidated ->
         ((Update.Story.handle_prompt_input model), Cmd.none)

      (Struct.Event.LoadStory http_result) ->
         case http_result of
            (Ok story) ->
               (
                  (Update.Story.start
                     {model |
                        tonkadur =
                           {story |
                              random_seed = model.tonkadur.random_seed
                           }
                     }
                  ),
                     Cmd.none
               )

            (Err error) ->
               (
                  {model |
                     ui =
                        (Struct.UI.display_string_error
                           (
                              "Failed to load story:\n"
                              ++
                              (
                                 case error of
                                    (Http.BadUrl details) ->
                                       ("Bad URL: " ++ details)

                                    Http.Timeout -> "Timeout."
                                    Http.NetworkError -> "Network Error."
                                    (Http.BadStatus code) ->
                                       (
                                          "Error code "
                                          ++ (String.fromInt code)
                                          ++ "."
                                       )

                                    (Http.BadBody details) ->
                                       (
                                          "Invalid content: "
                                          ++ details
                                       )
                              )
                           )
                           model.ui
                        )
                  },
                  Cmd.none
               )
