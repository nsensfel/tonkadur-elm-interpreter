module Tonkadur.Execute exposing (execute)

-- Elm -------------------------------------------------------------------------
import Dict
import List

-- Tonkadur --------------------------------------------------------------------
import Tonkadur.Types

import Tonkadur.Compute

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------
increment_program_counter : Tonkadur.Types.State -> Tonkadur.Types.State
increment_program_counter state =
   {state | program_counter = program_counter + 1}

---- INSTRUCTIONS --------------------------------------------------------------
add_event_option : (
      String ->
      (List Tonkadur.Types.Computation) ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
add_event_option name parameters state =
   (Tonkadur.Types.append_option
      (Event name (List.map (Tonkadur.Compute.compute state) parameters))
      state
   )

add_text_option : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
add_text_option label state =
   (Tonkadur.Types.append_option
      (Choice (Tonkadur.Compute.compute label state))
      state
   )

assert : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
assert condition label state =
   if (Tonkadur.Types.value_to_bool (Tonkadur.Compute.compute state condition))
   then
      -- TODO: some special error report
      state
   else state

display : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
)
display label state =
   -- TODO: where do we put displayed values?
   state

end : Tonkadur.Types.State -> Tonkadur.Types.State
end state =
   -- TODO: what do we do with this?
   state

extra_instruction : (
      String ->
      (List Tonkadur.Types.Computation) ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
extra_instruction name parameters state =
   -- No extra instruction supported.
   -- TODO: error report.
   state

initialize : (
      String ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
initialize type_name address state =
   {state |
      memory =
         (Tonkadur.Types.apply_at_address
            (Tonkadur.Types.value_to_list
               (Tonkadur.Compute.compute state address)
            )
            (\last_addr dict ->
               (Dict.insert
                  last_addr
                  (Tonkadur.Types.get_default state type_name)
                  dict
               )
            )
            state.memory
         )
      -- TODO: detect allocated memory for special handling.
   }

prompt_command : (
      Tonkadur.Types.PromptInstructionData ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
prompt_command prompt_data state =
   {state |
      memorized_target = (Tonkadur.Compute.compute state prompt_data.address),
      last_instruction_effect =
         (PromptCommand
            (Tonkadur.Compute.compute state prompt_data.min)
            (Tonkadur.Compute.compute state prompt_data.max)
            (Tonkadur.Compute.compute state prompt_data.label)
         )
   }

prompt_string : (
      Tonkadur.Types.PromptInstructionData ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
prompt_string prompt_data state =
   {state |
      memorized_target = (Tonkadur.Compute.compute state prompt_data.address),
      last_instruction_effect =
         (PromptString
            (Tonkadur.Compute.compute state prompt_data.min)
            (Tonkadur.Compute.compute state prompt_data.max)
            (Tonkadur.Compute.compute state prompt_data.label)
         )
   }

prompt_integer : (
      Tonkadur.Types.PromptInstructionData ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
prompt_integer prompt_data state =
   {state |
      memorized_target = (Tonkadur.Compute.compute state prompt_data.address),
      last_instruction_effect =
         (PromptInteger
            (Tonkadur.Compute.compute state prompt_data.min)
            (Tonkadur.Compute.compute state prompt_data.max)
            (Tonkadur.Compute.compute state prompt_data.label)
         )
   }

remove : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
remove address state =
   {state |
      memory =
         (Tonkadur.Types.apply_at_address
            (Tonkadur.Types.value_to_list
               (Tonkadur.Compute.compute state address)
            )
            (\last_addr dict -> (Dict.remove last_addr dict))
            state.memory
         )
      -- TODO: detect allocated memory for special handling.
   }

resolve_choice : Tonkadur.Types.State -> Tonkadur.Types.State
resolve_choice state =
   {state | last_instruction_effect = PromptChoice}

set_pc : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
)
set_pc value state =
   {state |
      program_counter =
         (Tonkadur.Types.value_to_int
            (Tonkadur.Compute.compute state value)
         )
   }

set_random : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
)
set_random address min max state =
   let
      (value, next_random_seed) =
         (Random.step
            (Random.int
               (Tonkadur.Types.value_to_int
                  (Tonkadur.Compute.compute state min)
                  (Tonkadur.Compute.compute state max)
               )
            )
            state.random_seed
         )
   in
   {state |
      memory =
         (Tonkadur.Types.apply_at_address
            (Tonkadur.Types.value_to_list
               (Tonkadur.Compute.compute state address)
            )
            (\last_addr dict -> (Dict.insert last_addr (IntValue value) dict))
            state.memory
         ),

      random_seed = next_random_seed
   }

set : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
)
set address value state =
   {state |
      memory =
         (Tonkadur.Types.apply_at_address
            (Tonkadur.Types.value_to_list
               (Tonkadur.Compute.compute state address)
            )
            (\last_addr dict ->
               (Dict.insert
                  last_addr
                  (Tonkadur.Compute.compute state value)
                  dict
               )
            )
            state.memory
         )
   }

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
execute : (
      Tonkadur.Types.Instruction ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State ->
   )
execute instruction state =
   let new_state = {state | last_instruction_effect = Continue} in
   case instruction of
      (AddEventOption name parameters) ->
         (increment_program_counter
            (add_event_option name parameters new_state)
         )

      (AddTextOption label) ->
         (increment_program_counter
            (add_text_option name parameters new_state)
         )

      (Assert condition label) ->
         (increment_program_counter
            (assert condition label new_state)
         )

      (Display label) ->
         (increment_program_counter (display label new_state))

      End -> (end new_state)
      (ExtraInstruction name parameters) ->
         (extra_instruction name parameters new_state)

      (Initialize type_name address) ->
         (increment_program_counter
            (initialize type_name address new_state)
         )
      (PromptCommand prompt_data) ->
         (increment_program_counter (prompt_command prompt_data new_state))

      (PromptInteger prompt_data) ->
         (increment_program_counter (prompt_integer prompt_data new_state))

      (PromptString prompt_data) ->
         (increment_program_counter (prompt_string prompt_data new_state))

      (Remove address) -> (increment_program_counter (remove address new_state))
      ResolveChoice -> (increment_program_counter (resolve_choice new_state))
      (SetPC value) -> (set_pc value new_state)
      (SetRandom address min max) ->
         (increment_program_counter (set_random address min max new_state))

      (Set address value) ->
         (increment_program_counter (set address value new_state))

