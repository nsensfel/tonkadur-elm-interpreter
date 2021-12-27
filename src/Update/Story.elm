module Update.Story exposing
   (
      new,
      select_choice,
      input_string,
      input_integer,
      input_command
   )

-- Elm -------------------------------------------------------------------------
import Html

-- Local Module ----------------------------------------------------------------
import Struct.Event
import Struct.Model

import Util.TonkadurToHtml

import Tonkadur.Execute

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------
step : Struct.Model.Type -> Struct.Model.Type
step model =
   case model.tonkadur.last_instruction_effect of
      MustContinue ->
         (step
            {model |
               tonkadur =
                  (Tonkadur.Execute.execute
                     model.tonkadur.code[model.tonkadur.program_counter]
                     model.tonkadur
                  )
            }
         )

      MustEnd -> model -- TODO

      (MustPromptCommand min max label) ->
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui =
               (Struct.UI.prompt_command
                  (Tonkadur.Types.value_to_int min)
                  (Tonkadur.Types.value_to_int max)
                  model.ui
               )
         }

      (MustPromptInteger min max label) ->
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui =
               (Struct.UI.prompt_integer
                  (Tonkadur.Types.value_to_int min)
                  (Tonkadur.Types.value_to_int max)
                  model.ui
               )
         }

      (MustPromptString min max label) ->
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui =
               (Struct.UI.prompt_string
                  (Tonkadur.Types.value_to_int min)
                  (Tonkadur.Types.value_to_int max)
                  model.ui
               )
         }

      MustPromptChoice ->
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui =
               (List.foldl
                  (\option (ix, ui) ->
                     case option of
                        (Choice rich_text) ->
                           (
                              (ix + 1),
                              (Struct.UI.display_choice
                                 ix
                                 (Util.TonkadurToHtml.convert
                                    (TextValue rich_text)
                                 )
                              )
                           )

                        _ -> ((ix + 1), ui)
                  )
                  (0, model.ui)
                  model.tonkadur.available_options
               )
         }

      (MustDisplay text) ->
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

      (MustDisplayError text) ->
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

new : Type
new =
   {
      displayed_text = [],
      displayed_options = []
   }


