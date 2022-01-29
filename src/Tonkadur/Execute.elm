module Tonkadur.Execute exposing (execute)

-- Elm -------------------------------------------------------------------------
import Dict
import List
import Random

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
   {state | program_counter = state.program_counter + 1}

---- INSTRUCTIONS --------------------------------------------------------------
add_event_option : (
      String ->
      (List Tonkadur.Types.Computation) ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
add_event_option name parameters state =
   (Tonkadur.Types.append_option
      (Tonkadur.Types.EventOption
         name
         (List.map (Tonkadur.Compute.compute state) parameters)
      )
      state
   )

add_text_option : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
add_text_option label state =
   (Tonkadur.Types.append_option
      (Tonkadur.Types.TextOption
         (Tonkadur.Types.value_to_text_or_string
            (Tonkadur.Compute.compute state label)
         )
      )
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
   then state
   else
      {state |
         last_instruction_effect =
            (Tonkadur.Types.MustDisplayError
               (Tonkadur.Compute.compute state label)
            )
      }

display : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
display label state =
   {state |
      last_instruction_effect =
         (Tonkadur.Types.MustDisplay (Tonkadur.Compute.compute state label))
   }

end : Tonkadur.Types.State -> Tonkadur.Types.State
end state = {state | last_instruction_effect = Tonkadur.Types.MustEnd }

extra_instruction : (
      String ->
      (List Tonkadur.Types.Computation) ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
extra_instruction name parameters state =
   {state |
      last_instruction_effect =
         (Tonkadur.Types.MustDisplayError
            (Tonkadur.Types.StringValue
               (
                  "No such extra instruction \""
                  ++ name
                  ++ "\"."
               )
            )
         )
   }

initialize : (
      String ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
initialize type_name address state =
   let
      address_as_list =
         (Tonkadur.Types.value_to_address
            (Tonkadur.Compute.compute state address)
         )
      new_state =
         {state |
            memory =
               (Tonkadur.Types.set_at_address
                  address_as_list
                  (Tonkadur.Types.get_default state type_name)
                  state.memory
               )
         }
   in
      case address_as_list of
         (single_element :: []) ->
            if (String.startsWith ".alloc." single_element)
            then
               if
               (
                  single_element
                  == (".alloc." ++ (String.fromInt new_state.allocated_data))
               )
               then
                  {new_state |
                     allocated_data = new_state.allocated_data + 1
                  }
               else
                  {new_state |
                     freed_addresses =
                        (List.filter
                           (\addr -> (addr /= single_element))
                           new_state.freed_addresses
                        )
                  }

            else new_state

         _ -> new_state

prompt_command : (
      Tonkadur.Types.PromptInstructionData ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
prompt_command prompt_data state =
   {state |
      memorized_target = (Tonkadur.Compute.compute state prompt_data.address),
      last_instruction_effect =
         (Tonkadur.Types.MustPromptCommand
            (Tonkadur.Compute.compute state prompt_data.min)
            (Tonkadur.Compute.compute state prompt_data.max)
            (Tonkadur.Compute.compute state prompt_data.label)
         )
   }

prompt_float : (
      Tonkadur.Types.PromptInstructionData ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
prompt_float prompt_data state =
   {state |
      memorized_target = (Tonkadur.Compute.compute state prompt_data.address),
      last_instruction_effect =
         (Tonkadur.Types.MustPromptFloat
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
         (Tonkadur.Types.MustPromptInteger
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
         (Tonkadur.Types.MustPromptString
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
   let
      address_as_list =
         (Tonkadur.Types.value_to_address
            (Tonkadur.Compute.compute state address)
         )
      new_state =
         {state |
            memory =
               (Tonkadur.Types.apply_at_address
                  address_as_list
                  (\last_addr dict -> (Dict.remove last_addr dict))
                  state.memory
               )
         }
   in
      case address_as_list of
         (single_element :: []) ->
            if (String.startsWith ".alloc." single_element)
            then
               {new_state |
                  freed_addresses =
                     (single_element :: new_state.freed_addresses)
               }
            else new_state

         _ -> new_state


resolve_choice : Tonkadur.Types.State -> Tonkadur.Types.State
resolve_choice state =
   {state | last_instruction_effect = Tonkadur.Types.MustPromptChoice}

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
               )
               (Tonkadur.Types.value_to_int
                  (Tonkadur.Compute.compute state max)
               )
            )
            state.random_seed
         )
   in
   {state |
      memory =
         (Tonkadur.Types.set_at_address
            (Tonkadur.Types.value_to_address
               (Tonkadur.Compute.compute state address)
            )
            (Tonkadur.Types.IntValue value)
            state.memory
         ),

      random_seed = next_random_seed
   }

set_value : (
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
set_value address value state =
   {state |
      memory =
         (Tonkadur.Types.set_at_address
            (Tonkadur.Types.value_to_address
               (Tonkadur.Compute.compute state address)
            )
            (Tonkadur.Compute.compute state value)
            state.memory
         )
   }

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
execute : (
      Tonkadur.Types.Instruction ->
      Tonkadur.Types.State ->
      Tonkadur.Types.State
   )
execute instruction state =
   let
      new_state =
         {state | last_instruction_effect = Tonkadur.Types.MustContinue}
   in
   case instruction of
      (Tonkadur.Types.AddEventOption name parameters) ->
         (increment_program_counter
            (add_event_option name parameters new_state)
         )

      (Tonkadur.Types.AddTextOption label) ->
         (increment_program_counter
            (add_text_option label new_state)
         )

      (Tonkadur.Types.Assert condition label) ->
         (increment_program_counter
            (assert condition label new_state)
         )

      (Tonkadur.Types.Display label) ->
         (increment_program_counter (display label new_state))

      Tonkadur.Types.End -> (end new_state)
      (Tonkadur.Types.ExtraInstruction name parameters) ->
         (extra_instruction name parameters new_state)

      (Tonkadur.Types.Initialize type_name address) ->
         (increment_program_counter
            (initialize type_name address new_state)
         )
      (Tonkadur.Types.PromptCommand prompt_data) ->
         (increment_program_counter (prompt_command prompt_data new_state))

      (Tonkadur.Types.PromptFloat prompt_data) ->
         (increment_program_counter (prompt_float prompt_data new_state))

      (Tonkadur.Types.PromptInteger prompt_data) ->
         (increment_program_counter (prompt_integer prompt_data new_state))

      (Tonkadur.Types.PromptString prompt_data) ->
         (increment_program_counter (prompt_string prompt_data new_state))

      (Tonkadur.Types.Remove address) ->
         (increment_program_counter (remove address new_state))

      Tonkadur.Types.ResolveChoice ->
         (increment_program_counter (resolve_choice new_state))

      (Tonkadur.Types.SetPC value) -> (set_pc value new_state)
      (Tonkadur.Types.SetRandom address min max) ->
         (increment_program_counter (set_random address min max new_state))

      (Tonkadur.Types.SetValue address value) ->
         (increment_program_counter (set_value address value new_state))

