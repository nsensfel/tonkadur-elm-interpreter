module Tonkadur.Json exposing (decoder)

-- Elm -------------------------------------------------------------------------
import Array
import Dict
import List
import Random

import Json.Decode
import Json.Decode.Pipeline

-- Tonkadur --------------------------------------------------------------------
import Tonkadur.Types

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------
---- COMPUTATIONS --------------------------------------------------------------
specific_computation_decoder : (
      String ->
      (Json.Decode.Decoder Tonkadur.Types.Computation)
   )
specific_computation_decoder name =
   case name of
      "add_text_effect" ->
         (Json.Decode.map3
            (\ename params content ->
               (Tonkadur.Types.AddTextEffect ename params content)
            )
            (Json.Decode.field "effect" (Json.Decode.string))
            (Json.Decode.field
               "parameters"
               (Json.Decode.list (computation_decoder))
            )
            (Json.Decode.field
               "content"
               (Json.Decode.list (computation_decoder))
            )
         )

      "address" ->
         (Json.Decode.map
            (\address -> (Tonkadur.Types.Address address))
            (Json.Decode.field "address" (computation_decoder))
         )

      "cast" ->
         (Json.Decode.map3
            (\from to value -> (Tonkadur.Types.Cast from to value))
            (Json.Decode.field "from" (Json.Decode.string))
            (Json.Decode.field "to" (Json.Decode.string))
            (Json.Decode.field "content" (computation_decoder))
         )

      "constant" ->
         (Json.Decode.map2
            (\type_name value -> (Tonkadur.Types.Constant type_name value))
            (Json.Decode.field "type" (Json.Decode.string))
            (Json.Decode.field "value" (Json.Decode.string))
         )

      "get_allocable_address" ->
         (Json.Decode.succeed Tonkadur.Types.NextAllocableAddress)

      "if_else" ->
         (Json.Decode.map3
            (\condition if_true if_false ->
               (Tonkadur.Types.IfElse condition if_true if_false)
            )
            (Json.Decode.field "condition" (computation_decoder))
            (Json.Decode.field "if_true" (computation_decoder))
            (Json.Decode.field "if_false" (computation_decoder))
         )

      "last_choice_index" ->
         (Json.Decode.succeed Tonkadur.Types.LastChoiceIndex)

      "newline" -> (Json.Decode.succeed Tonkadur.Types.Newline)
      "operation" ->
         (Json.Decode.map3
            (\op x maybe_y ->
               (Tonkadur.Types.Operation
                  op
                  x
                  (
                     case maybe_y of
                        (Just y) -> y
                        Nothing ->
                           (Tonkadur.Types.Constant
                              "string"
                              "No second operand provided."
                           )
                  )
               )
            )
            (Json.Decode.field "operator" (Json.Decode.string))
            (Json.Decode.field "x" (computation_decoder))
            (Json.Decode.maybe (Json.Decode.field "y" (computation_decoder)))
         )

      "relative_address" ->
         (Json.Decode.map2
            (\base extra -> (Tonkadur.Types.RelativeAddress base extra))
            (Json.Decode.field "base" (computation_decoder))
            (Json.Decode.field "extra" (computation_decoder))
         )

      "text" ->
         (Json.Decode.map
            (\computation -> (Tonkadur.Types.Text computation))
            (Json.Decode.field
               "content"
               (Json.Decode.list (computation_decoder))
            )
         )

      "value_of" ->
         (Json.Decode.map
            (\computation -> (Tonkadur.Types.ValueOf computation))
            (Json.Decode.field "reference" (computation_decoder))
         )

      _ ->
         (Json.Decode.map
            (\params -> (Tonkadur.Types.ExtraComputation name params))
            (Json.Decode.field "params"
               (Json.Decode.list (computation_decoder))
            )
         )

computation_decoder : (Json.Decode.Decoder Tonkadur.Types.Computation)
computation_decoder =
   (Json.Decode.andThen
      (specific_computation_decoder)
      (Json.Decode.field "category" (Json.Decode.string))
   )

---- INSTRUCTIONS --------------------------------------------------------------
prompt_instruction_data_decoder : (
      (Json.Decode.Decoder Tonkadur.Types.PromptInstructionData)
   )
prompt_instruction_data_decoder =
   (Json.Decode.succeed
      Tonkadur.Types.PromptInstructionData
      -- min
      |> (Json.Decode.Pipeline.required "min" (computation_decoder))

      -- max
      |> (Json.Decode.Pipeline.required "max" (computation_decoder))

      -- address
      |> (Json.Decode.Pipeline.required "target" (computation_decoder))

      -- label
      |> (Json.Decode.Pipeline.required "label" (computation_decoder))
   )

specific_instruction_decoder : (
      String -> (Json.Decode.Decoder Tonkadur.Types.Instruction)
   )
specific_instruction_decoder name =
   case name of
      "add_event_option" ->
         (Json.Decode.map2
            (\oname params -> (Tonkadur.Types.AddEventOption oname params))
            (Json.Decode.field "event" (Json.Decode.string))
            (Json.Decode.field
               "reference"
               (Json.Decode.list (computation_decoder))
            )
         )

      "add_text_option" ->
         (Json.Decode.map
            (\computation -> (Tonkadur.Types.AddTextOption computation))
            (Json.Decode.field "label" (computation_decoder))
         )

      "assert" ->
         (Json.Decode.map2
            (\condition label -> (Tonkadur.Types.Assert condition label))
            (Json.Decode.field "condition" (computation_decoder))
            (Json.Decode.field "message" (computation_decoder))
         )

      "display" ->
         (Json.Decode.map
            (\computation -> (Tonkadur.Types.Display computation))
            (Json.Decode.field "content" (computation_decoder))
         )

      "end" -> (Json.Decode.succeed Tonkadur.Types.End)
      "initialize" ->
         (Json.Decode.map2
            (\type_name ref -> (Tonkadur.Types.Initialize type_name ref))
            (Json.Decode.field "type" (Json.Decode.string))
            (Json.Decode.field "reference" (computation_decoder))
         )

      "prompt_command" ->
         (Json.Decode.map
            (\data -> (Tonkadur.Types.PromptCommand data))
            (prompt_instruction_data_decoder)
         )

      "prompt_integer" ->
         (Json.Decode.map
            (\data -> (Tonkadur.Types.PromptInteger data))
            (prompt_instruction_data_decoder)
         )

      "prompt_string" ->
         (Json.Decode.map
            (\data -> (Tonkadur.Types.PromptString data))
            (prompt_instruction_data_decoder)
         )

      "remove" ->
         (Json.Decode.map
            (\computation -> (Tonkadur.Types.Remove computation))
            (Json.Decode.field "reference" (computation_decoder))
         )

      "resolve_choice" -> (Json.Decode.succeed Tonkadur.Types.ResolveChoice)
      "set_pc" ->
         (Json.Decode.map
            (\computation -> (Tonkadur.Types.SetPC computation))
            (Json.Decode.field "value" (computation_decoder))
         )

      "set_random" ->
         (Json.Decode.map3
            (\min max target -> (Tonkadur.Types.SetRandom min max target))
            (Json.Decode.field "min" (computation_decoder))
            (Json.Decode.field "max" (computation_decoder))
            (Json.Decode.field "target" (computation_decoder))
         )

      "set_value" ->
         (Json.Decode.map2
            (\target value -> (Tonkadur.Types.Set target value))
            (Json.Decode.field "reference" (computation_decoder))
            (Json.Decode.field "value" (computation_decoder))
         )

      _ ->
         (Json.Decode.map
            (\params -> (Tonkadur.Types.ExtraInstruction name params))
            (Json.Decode.field "params"
               (Json.Decode.list (computation_decoder))
            )
         )

instruction_decoder : (Json.Decode.Decoder Tonkadur.Types.Instruction)
instruction_decoder =
   (Json.Decode.andThen
      (specific_instruction_decoder)
      (Json.Decode.field "category" (Json.Decode.string))
   )


---- TYPES --------------------------------------------------------------------
-- There's a slight issue: we're getting the type definitions before they're
-- used, yes, but we're getting all the type definitions before we're able to
-- use the previous results.
-- To mitigate this issue, we first get type names for fields instead of values
-- (the 'raw' decoder), then we generate the actual values.
raw_user_type_decoder : (
      (Json.Decode.Decoder (String, (Dict.Dict String String)))
   )
raw_user_type_decoder =
   (Json.Decode.map2
      (\name pair_list -> (name, (Dict.fromList pair_list)))
      (Json.Decode.field "name" (Json.Decode.string))
      (Json.Decode.field "fields"
         (Json.Decode.list
            (Json.Decode.map2
               (\name line -> (name, line))
               (Json.Decode.field "name" (Json.Decode.string))
               (Json.Decode.field "type" (Json.Decode.string))
            )
         )
      )
   )

user_types_decoder : (
      (Json.Decode.Decoder (Dict.Dict String Tonkadur.Types.Value))
   )
user_types_decoder =
   (Json.Decode.map
      (\list_of_raw_types ->
         (List.foldl
            (\(name, fields) defined_types ->
               (Dict.insert
                  name
                  (Tonkadur.Types.StructureValue
                     (Dict.map
                        (\field_name field_type_name ->
                           case
                              (Tonkadur.Types.maybe_get_default_primitive
                                 field_type_name
                              )
                           of
                              (Just default) -> default
                              Nothing ->
                                 case
                                    (Dict.get field_type_name defined_types)
                                 of
                                    (Just default) -> default
                                    Nothing ->
                                       (Tonkadur.Types.StringValue
                                          (
                                             "Undefined type '"
                                             ++ field_type_name
                                             ++ "'"
                                          )
                                       )
                        )
                        fields
                     )
                  )
                  defined_types
               )
            )
            (Dict.empty)
            list_of_raw_types
         )
      )
      (Json.Decode.list (raw_user_type_decoder))
   )

---- STATE ---------------------------------------------------------------------
code_decoder : (Json.Decode.Decoder (Array.Array Tonkadur.Types.Instruction))
code_decoder = (Json.Decode.array (instruction_decoder))

sequences_decoder : (Json.Decode.Decoder (Dict.Dict String Int))
sequences_decoder =
   (Json.Decode.map
      (\pair_list -> (Dict.fromList pair_list))
      (Json.Decode.list
         (Json.Decode.map2
            (\name line -> (name, line))
            (Json.Decode.field "name" (Json.Decode.string))
            (Json.Decode.field "value" (Json.Decode.int))
         )
      )
   )

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
decoder : (Json.Decode.Decoder Tonkadur.Types.State)
decoder =
   (Json.Decode.succeed
      Tonkadur.Types.State
      -- memory
      |> (Json.Decode.Pipeline.hardcoded (Dict.empty))

      -- user_types
      |> (Json.Decode.Pipeline.optional
            "structure_types"
            (user_types_decoder)
            (Dict.empty)
         )

      -- sequences
      |> (Json.Decode.Pipeline.optional
            "sequences"
            (sequences_decoder)
            (Dict.empty)
         )

      -- code
      |> (Json.Decode.Pipeline.required "code" (code_decoder))

      -- program_counter
      |> (Json.Decode.Pipeline.hardcoded 0)

      -- allocated_data
      |> (Json.Decode.Pipeline.hardcoded 0)

      -- last_choice_index
      |> (Json.Decode.Pipeline.hardcoded 0)

      -- available_options
      |> (Json.Decode.Pipeline.hardcoded [])

      -- memorized_target
      |> (Json.Decode.Pipeline.hardcoded (Tonkadur.Types.PointerValue []))

      -- last_instruction_effect
      |> (Json.Decode.Pipeline.hardcoded Tonkadur.Types.MustContinue)

      -- freed_addresses
      |> (Json.Decode.Pipeline.hardcoded [])

      -- random_seed
      |> (Json.Decode.Pipeline.hardcoded (Random.initialSeed 42))
   )
