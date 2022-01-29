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

            Nothing ->
               {model |
                  ui =
                     (Struct.UI.display_string_error
                        (
                           "[Programming error] Cannot 'step' Tonkadur story:"
                           ++ " Not in MustContinue state."
                        )
                        model.ui
                     )
               }

      Tonkadur.Types.MustEnd -> model -- TODO, although that might be right.

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

      (Tonkadur.Types.MustPromptFloat min max label) ->
         {model |
            tonkadur = (Tonkadur.Types.allow_continuing model.tonkadur),
            ui =
               (Struct.UI.prompt_float
                  (Tonkadur.Types.value_to_float min)
                  (Tonkadur.Types.value_to_float max)
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
         let
            (last_ix, new_ui) =
               (List.foldl
                  (\option (ix, ui) ->
                     case option of
                        (Tonkadur.Types.TextOption rich_text) ->
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

--      _ -> model

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
start : Struct.Model.Type -> Struct.Model.Type
start model = (step model)

select_choice : Int -> Struct.Model.Type -> Struct.Model.Type
select_choice ix model =
   (step
      {model |
         tonkadur =
            (Tonkadur.Types.clear_all_options
               (Tonkadur.Types.set_last_choice_index
                  ix
                  model.tonkadur
               )
            )
      }
   )

input_string : String -> Struct.Model.Type -> Struct.Model.Type
input_string string model =
   let string_length = (String.length string) in
   if ((string_length < model.ui.min) || (string_length > model.ui.max))
   then
      {model |
         ui =
            (Struct.UI.display_string_error
               (
                  "Input string should be between "
                  ++ (String.fromInt model.ui.min)
                  ++ " and "
                  ++ (String.fromInt model.ui.max)
                  ++ " characters."
               )
               model.ui
            )
      }
   else
      (step
         {model |
            tonkadur =
               (Tonkadur.Types.set_target_from_string
                  string
                  model.tonkadur
               )
         }
      )

input_float : String -> Struct.Model.Type -> Struct.Model.Type
input_float string model =
   case (String.toFloat string) of
      Nothing ->
         {model |
            ui =
               (Struct.UI.display_string_error
                  "Input expects a float."
                  model.ui
               )
         }

      (Just float) ->
         if ((float < model.ui.min_float) || (float > model.ui.max_float))
         then
            {model |
               ui =
                  (Struct.UI.display_string_error
                     (
                        "Input float should be between "
                        ++ (String.fromFloat model.ui.min_float)
                        ++ " and "
                        ++ (String.fromFloat model.ui.max_float)
                        ++ "."
                     )
                     model.ui
                  )
            }
         else
            (step
               {model |
                  tonkadur =
                     (Tonkadur.Types.set_target_from_float
                        float
                        model.tonkadur
                     )
               }
            )

input_integer : String -> Struct.Model.Type -> Struct.Model.Type
input_integer string model =
   case (String.toInt string) of
      Nothing ->
         {model |
            ui =
               (Struct.UI.display_string_error
                  "Input expects an integer."
                  model.ui
               )
         }

      (Just int) ->
         if ((int < model.ui.min) || (int > model.ui.max))
         then
            {model |
               ui =
                  (Struct.UI.display_string_error
                     (
                        "Input integer should be between "
                        ++ (String.fromInt model.ui.min)
                        ++ " and "
                        ++ (String.fromInt model.ui.max)
                        ++ "."
                     )
                     model.ui
                  )
            }
         else
            (step
               {model |
                  tonkadur =
                     (Tonkadur.Types.set_target_from_integer
                        int
                        model.tonkadur
                     )
               }
            )

input_command : String -> Struct.Model.Type -> Struct.Model.Type
input_command string model =
   let string_length = (String.length string) in
   if ((string_length < model.ui.min) || (string_length > model.ui.max))
   then
      {model |
         ui =
            (Struct.UI.display_string_error
               (
                  "Input string should be between "
                  ++ (String.fromInt model.ui.min)
                  ++ " and "
                  ++ (String.fromInt model.ui.max)
                  ++ " characters."
               )
               model.ui
            )
      }
   else
      (step
         {model |
            tonkadur =
               (Tonkadur.Types.set_target_from_command
                  (String.words string)
                  model.tonkadur
               )
         }
      )
