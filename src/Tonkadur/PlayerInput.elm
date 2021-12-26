module Tonkadur.PlayerInput exposing (..)

-- Elm -------------------------------------------------------------------------
import Dict
import List

-- Tonkadur --------------------------------------------------------------------
import Tonkadur.Types

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
select_choice : Int -> Tonkadur.Types.State -> Tonkadur.Types.State
select_choice index state = {state | last_choice_index = index}

input_string : String -> Tonkadur.Types.State -> Tonkadur.Types.State
input_string string state =
   {state |
      memory =
         (Tonkadur.Types.apply_at_address
            (Tonkadur.Types.value_to_address state.memorized_target)
            (\last_address dict ->
               (Dict.insert last_address (StringValue string) dict)
            )
            state.memory
         )
   }

input_int : Int -> Tonkadur.Types.State -> Tonkadur.Types.State
input_int int state =
   {state |
      memory =
         (Tonkadur.Types.apply_at_address
            (Tonkadur.Types.value_to_address state.memorized_target)
            (\last_address dict ->
               (Dict.insert last_address (IntValue int) dict)
            )
            state.memory
         )
   }

input_command : (
      (List String) ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
input_command commands state =
   {state |
      memory =
         (Tonkadur.Types.apply_at_address
            (Tonkadur.Types.value_to_address state.memorized_target)
            (\last_address dict ->
               (Dict.insert
                  last_address
                  (ListValue
                     (Dict.fromList
                        (List.indexedMap
                           (\index value ->
                              (
                                 (
                                    case (String.fromInt index) of
                                       (Just i) -> i
                                       Nothing -> "invalid_index"
                                 ),
                                 value
                              )
                           )
                           commands
                        )
                     )
                  )
                  dict
               )
            )
            state.memory
         )
   }
