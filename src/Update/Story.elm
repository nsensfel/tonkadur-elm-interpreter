module Update.Story exposing
   (
      start,
      select_choice,
      input_string,
      input_integer,
      input_command
   )

-- Elm -------------------------------------------------------------------------
import Array
import Html

-- Local Module ----------------------------------------------------------------
import Struct.Event
import Struct.Model
import Struct.UI

import Util.TonkadurToHtml

-- Tonkadur --------------------------------------------------------------------
import Tonkadur.Execute
import Tonkadur.Types

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------
step : Struct.Model.Type -> Struct.Model.Type
step model =
   case model.tonkadur.last_instruction_effect of
      Tonkadur.Types.MustContinue ->
         case (Array.get model.tonkadur.program_counter model.tonkadur.code) of
            (Just instruction) ->
               (step
                  {model |
                     tonkadur =
                        (Tonkadur.Execute.execute instruction model.tonkadur)
                  }
               )

            Nothing -> model -- TODO: error

      Tonkadur.Types.MustEnd -> model -- TODO

      (Tonkadur.Types.MustPromptCommand min max label) ->
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui =
               (Struct.UI.prompt_command
                  (Tonkadur.Types.value_to_int min)
                  (Tonkadur.Types.value_to_int max)
                  model.ui
               )
         }

      (Tonkadur.Types.MustPromptInteger min max label) ->
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui =
               (Struct.UI.prompt_integer
                  (Tonkadur.Types.value_to_int min)
                  (Tonkadur.Types.value_to_int max)
                  model.ui
               )
         }

      (Tonkadur.Types.MustPromptString min max label) ->
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui =
               (Struct.UI.prompt_string
                  (Tonkadur.Types.value_to_int min)
                  (Tonkadur.Types.value_to_int max)
                  model.ui
               )
         }

      Tonkadur.Types.MustPromptChoice ->
         let (last_ix, new_ui) =
               (List.foldl
                  (\option (ix, ui) ->
                     case option of
                        (Tonkadur.Types.Choice rich_text) ->
                           (
                              (ix + 1),
                              (Struct.UI.display_choice
                                 ix
                                 (Util.TonkadurToHtml.convert
                                    (Tonkadur.Types.TextValue rich_text)
                                 )
                                 ui
                              )
                           )

                        _ -> ((ix + 1), ui)
                  )
                  (0, model.ui)
                  model.tonkadur.available_options
               )
         in
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui = new_ui
         }

      (Tonkadur.Types.MustDisplay text) ->
         (step
            {model |
               tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
               ui =
                  (Struct.UI.display_text
                     (Util.TonkadurToHtml.convert text)
                     model.ui
                  )
            }
         )

      (Tonkadur.Types.MustDisplayError text) ->
         (step
            {model |
               tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
               ui =
                  (Struct.UI.display_error
                     (Util.TonkadurToHtml.convert text)
                     model.ui
                  )
            }
         )

      _ -> model

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
start : Struct.Model.Type -> Struct.Model.Type
start model = (step model)

select_choice : Int -> Struct.Model.Type -> Struct.Model.Type
select_choice ix model = model
   -- TODO: implement

input_string : String -> Struct.Model.Type -> Struct.Model.Type
input_string string model = model
   -- TODO: implement

input_integer : String -> Struct.Model.Type -> Struct.Model.Type
input_integer string model = model
   -- TODO: implement

input_command : String -> Struct.Model.Type -> Struct.Model.Type
input_command string model = model
   -- TODO: implement
