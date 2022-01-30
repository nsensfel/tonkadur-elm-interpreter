module Tonkadur.Types exposing (..)

-- Elm -------------------------------------------------------------------------
import Array
import Dict
import List

import Random

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------
type alias TextData =
   {
      content : (List RichText),
      effect_name : String,
      effect_parameters : (List Value)
   }

type RichText =
   StringText String
   | AugmentedText TextData
   | NewlineText

type Value =
   BoolValue Bool
   | FloatValue Float
   | IntValue Int
   | TextValue RichText
   | StringValue String
   | ListValue (Dict.Dict String Value)
   | PointerValue (List String)
   | StructureValue (Dict.Dict String Value)

type Option =
   TextOption RichText
   | EventOption String (List Value)

type Computation =
   AddTextEffect String (List Computation) (List Computation)
   | Address Computation
   | Cast String String Computation
   | Constant String String
   | ExtraComputation String (List Computation)
   | IfElse Computation Computation Computation
   | LastChoiceIndex
   | Newline
   | NextAllocableAddress
   | Operation String Computation Computation
   | RelativeAddress Computation Computation
   | Size Computation
   | Text (List Computation)
   | ValueOf Computation

type alias PromptInstructionData =
   {
      min : Computation,
      max : Computation,
      address : Computation,
      label : Computation
   }

type Instruction =
   AddEventOption String (List Computation)
   | AddTextOption Computation
   | Assert Computation Computation
   | Display Computation
   | End
   | ExtraInstruction String (List Computation)
   | Initialize String Computation
   | PromptCommand PromptInstructionData
   | PromptFloat PromptInstructionData
   | PromptInteger PromptInstructionData
   | PromptString PromptInstructionData
   | Remove Computation
   | ResolveChoice
   | SetPC Computation
   | SetRandom Computation Computation Computation
   | SetValue Computation Computation

type InstructionEffect =
   MustContinue
   | MustEnd
   | MustPromptCommand Value Value Value
   | MustPromptFloat Value Value Value
   | MustPromptInteger Value Value Value
   | MustPromptString Value Value Value
   | MustPromptChoice
   | MustDisplay Value
   | MustDisplayError Value

type alias State =
   {
      memory : (Dict.Dict String Value),
      user_types : (Dict.Dict String Value),
      sequences : (Dict.Dict String Int),
      code : (Array.Array Instruction),
      program_counter : Int,
      allocated_data : Int,
      last_choice_index : Int,
      available_options : (List Option),
      memorized_target : Value,

      last_instruction_effect : InstructionEffect,
      freed_addresses : (List String),
      random_seed : Random.Seed
   }

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
new_state : Int -> State
new_state random_seed =
   {
      memory = (Dict.empty),
      user_types = (Dict.empty),
      sequences = (Dict.empty),
      code = (Array.empty),
      program_counter = 0,
      allocated_data = 0,
      last_choice_index = 0,
      available_options = [],
      memorized_target = (PointerValue [""]),

      last_instruction_effect = MustContinue,
      freed_addresses = [],
      random_seed = (Random.initialSeed random_seed)
   }

value_to_bool : Value -> Bool
value_to_bool value =
   case value of
      (BoolValue result) -> result
      _ ->
         (Debug.log
            ("Can't value_to_bool " ++ (debug_value_to_string value))
            False
         )

value_to_float : Value -> Float
value_to_float value =
   case value of
      (FloatValue result) -> result
      _ ->
         (Debug.log
            ("Can't value_to_float " ++ (debug_value_to_string value))
            0.0
         )

value_to_int : Value -> Int
value_to_int value =
   case value of
      (IntValue result) -> result
      _ ->
         (Debug.log
            ("Can't value_to_int " ++ (debug_value_to_string value))
            0
         )

value_to_text_or_string : Value -> RichText
value_to_text_or_string value =
   case value of
      (TextValue result) -> result
      (StringValue string) -> (StringText string)
      _ ->
         (Debug.log
            ("Can't value_to_text_or_string" ++ (debug_value_to_string value))
            (StringText "")
         )

value_to_string : Value -> String
value_to_string value =
   case value of
      (StringValue result) -> result
      (TextValue text) ->
         case text of
            (StringText result) -> result
            (AugmentedText rich_text) ->
               (String.concat
                  (List.map
                     (\text_value -> (value_to_string (TextValue text_value)))
                     rich_text.content
                  )
               )

            NewlineText -> "\n"

      _ -> "Cannot turn this value into string without cast."

debug_value_to_string : Value -> String
debug_value_to_string value =
   case value of
      (StringValue result) -> result
      (TextValue text) ->
         case text of
            (StringText result) -> result
            (AugmentedText rich_text) ->
               (String.concat
                  (List.map
                     (\text_value -> (value_to_string (TextValue text_value)))
                     rich_text.content
                  )
               )

            NewlineText -> "\n"
      (BoolValue bool) ->
         if (bool)
         then "true"
         else "false"

      (FloatValue float) -> (String.fromFloat float)
      (IntValue int) -> (String.fromInt int)
      (ListValue dict) ->
         (
            "["
            ++
            (String.join
               ", "
               (List.map
                  (\(key, val) ->
                     (
                        key
                        ++ ": "
                        ++ (debug_value_to_string val)
                     )
                  )
                  (Dict.toList dict)
               )
            )
            ++
            "]"
         )

      (PointerValue list) -> ("(addr [" ++ (String.join ", " list) ++ "])")
      (StructureValue dict) ->
         (
            "["
            ++
            (String.join
               ", "
               (List.map
                  (\(key, val) ->
                     (
                        key
                        ++ ": "
                        ++ (debug_value_to_string val)
                     )
                  )
                  (Dict.toList dict)
               )
            )
            ++
            "]"
         )


value_to_dict : Value -> (Dict.Dict String Value)
value_to_dict value =
   case value of
      (StructureValue dict) -> dict
      (ListValue dict) -> dict
      _ ->
         (Debug.log
            ("Can't value_to_dict" ++ (debug_value_to_string value))
            (Dict.empty)
         )

value_to_address : Value -> (List String)
value_to_address value =
   case value of
      (PointerValue result) -> result
      _ ->
         (Debug.log
            ("Can't value_to_adress " ++ (debug_value_to_string value))
            []
         )

no_text_effect : String
no_text_effect = ""

append_text_content : RichText -> RichText -> RichText
append_text_content base addition =
   case base of
      (AugmentedText text_data) ->
         case addition of
            (AugmentedText other_text_data) ->
               -- Optimize text to avoid increasing depth if no new effect is
               -- introduced.
               if (other_text_data.effect_name == (no_text_effect))
               then
                  (AugmentedText
                     {text_data |
                        content =
                           (List.append
                              text_data.content
                              other_text_data.content
                           )
                     }
                  )
               else
                  (AugmentedText
                     {text_data |
                        content =
                           (List.append
                              text_data.content
                              (List.singleton addition)
                           )
                     }
                  )

            other ->
               (AugmentedText
                  {text_data |
                     content =
                        (List.append text_data.content (List.singleton other))
                  }
               )

      non_augmented_text_data ->
         (append_text_content
            (append_text_content (AugmentedText (default_text_data)) base)
            addition
         )

default_text_data : TextData
default_text_data =
   {
      effect_name = (no_text_effect),
      effect_parameters = [],
      content = []
   }

append_option : Option -> State -> State
append_option option state =
   {state |
      available_options =
         (List.append state.available_options (List.singleton option))
   }

get_default : State -> String -> Value
get_default state type_name =
   case (maybe_get_default_primitive type_name) of
      (Just value) -> value
      Nothing ->
         case (Dict.get type_name state.user_types) of
            (Just default) -> default
            Nothing -> (StringValue ("Unknown type '" ++ type_name ++ "'"))

-- Used during the decoding process, prior to 'state' being available, hence
-- its separation from 'get_default'.
maybe_get_default_primitive : String -> (Maybe Value)
maybe_get_default_primitive type_name =
   case type_name of
      "bool" -> (Just (BoolValue False))
      "float" -> (Just (FloatValue 0.0))
      "int" -> (Just (IntValue 0))
      "text" -> (Just (TextValue (StringText "")))
      "string" -> (Just (StringValue ""))
      "list" -> (Just (ListValue (Dict.empty)))
      "ptr" -> (Just (PointerValue []))
      "wild dict" -> (Just (StructureValue (Dict.empty)))
      _ -> Nothing

apply_at_address : (
      (List String) ->
      (
         String ->
         (Dict.Dict String Value) ->
         (Dict.Dict String Value)
      ) ->
      (Dict.Dict String Value) ->
      (Dict.Dict String Value)
   )
apply_at_address address fun memory =
   case address of
      [] -> memory
      (last_element :: []) -> (fun last_element memory)
      (next_element :: next_address) ->
         (Dict.update
            next_element
            (\maybe_value ->
               case maybe_value of
                  (Just (StructureValue value)) ->
                     (Just
                        (StructureValue
                           (apply_at_address
                              next_address
                              (fun)
                              value
                           )
                        )
                     )

                  (Just (ListValue value)) ->
                     (Just
                        (ListValue
                           (apply_at_address
                              next_address
                              (fun)
                              value
                           )
                        )
                     )

                  _ -> Nothing
            )
            memory
         )

set_at_address : (
      (List String) ->
      Value ->
      (Dict.Dict String Value) ->
      (Dict.Dict String Value)
   )
set_at_address address value memory =
   (apply_at_address
      address
      (\last_address dict -> (Dict.insert last_address value dict))
      memory
   )

allow_continuing : State -> State
allow_continuing state = {state | last_instruction_effect = MustContinue}

compare_pointers : (List String) -> (List String) -> Int
compare_pointers p0 p1 =
   case (p0, p1) of
      ((h0 :: t0), (h1 :: t1)) ->
         if (h0 == h1)
         then (compare_pointers t0 t1)
         else if (h0 < h1)
         then -1
         else 1

      ([], []) -> 0
      (_, []) -> 1
      ([], _) -> -1

elm_list_to_wyrd_list : (x -> Value) -> (List x) -> Value
elm_list_to_wyrd_list elm_value_to_wyrd_value list =
   let
      (final_next_index, final_as_dict) =
         (List.foldl
            (\value (next_index, as_dict) ->
               (
                  (next_index + 1),
                  (Dict.insert
                     (String.fromInt next_index)
                     (elm_value_to_wyrd_value value)
                     as_dict
                  )
               )
            )
            (0, (Dict.empty))
            list
         )
   in
      (ListValue final_as_dict)

set_last_choice_index : Int -> State -> State
set_last_choice_index ix state = {state | last_choice_index = ix}

clear_all_options : State -> State
clear_all_options state = {state | available_options = []}

set_target_from_string : String -> State -> State
set_target_from_string str state =
   {state |
      memory =
         (set_at_address
            (value_to_address state.memorized_target)
            (StringValue str)
            state.memory
         )
   }

set_target_from_float : Float -> State -> State
set_target_from_float float state =
   {state |
      memory =
         (set_at_address
            (value_to_address state.memorized_target)
            (FloatValue float)
            state.memory
         )
   }


set_target_from_integer : Int -> State -> State
set_target_from_integer int state =
   {state |
      memory =
         (set_at_address
            (value_to_address state.memorized_target)
            (IntValue int)
            state.memory
         )
   }

set_target_from_command : (List String) -> State -> State
set_target_from_command list state =
   {state |
      memory =
         (set_at_address
            (value_to_address state.memorized_target)
            (elm_list_to_wyrd_list (\command -> (StringValue command)) list)
            state.memory
         )
   }
