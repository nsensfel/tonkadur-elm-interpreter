module Tonkadur.Types exposing (..)

-- Elm -------------------------------------------------------------------------
import Dict
import List

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
   Choice RichText
   | Event (String, (List Value))

type Computation =
   AddTextEffect (String, (List Computation), (List Computation))
   | Address Computation
   | Cast (String, String, Computation)
   | Constant (String, String)
   | ExtraComputation (String, (List Computation))
   | IfElse (Computation, Computation, Computation)
   | LastChoiceIndex
   | Newline
   | NextAllocableAddress
   | Operation (String, Computation, Computation)
   | RelativeAddress (Computation, Computation)
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
   AddEventOption (String, (List Computation))
   | AddTextOption Computation
   | Assert (Computation, Computation)
   | Display Computation
   | End
   | ExtraInstruction (String, (List Computation))
   | Initialize (String, Computation)
   | PromptCommand PromptInstructionData
   | PromptInteger PromptInstructionData
   | PromptString PromptInstructionData
   | Remove Computation
   | ResolveChoice
   | SetPC Computation
   | SetRandom (Computation, Computation, Computation)
   | Set (Computation, Computation)

type InstructionEffect =
   MustContinue
   | MustEnd
   | MustPromptCommand (Value, Value, Value)
   | MustPromptInteger (Value, Value, Value)
   | MustPromptString (Value, Value, Value)
   | MustPromptChoice
   | MustDisplay Value
   | MustDisplayError Value
   | MustExtraEffect (String, (List Value))

type alias State =
   {
      memory : (Dict.Dict String Value),
      user_types : (Dict.Dict String Value),
      sequences : (Dict.Dict String Int),
      code : (List Instruction),
      program_counter : Int,
      allocated_data : Int,
      last_choice_index : Int,
      available_options : (List Option),
      memorized_target : Value,

      last_instruction_effect : InstructionEffect
   }

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
value_to_bool : Value -> Bool
value_to_bool value =
   case value of
      (BoolValue result) -> result
      _ -> False

value_to_float : Value -> Float
value_to_float value =
   case value of
      (FloatValue result) -> result
      _ -> 0.0

value_to_int : Value -> Int
value_to_int value =
   case value of
      (IntValue result) -> result
      _ -> 0

value_to_text_or_string : Value -> RichText
value_to_text_or_string value =
   case value of
      (TextValue result) -> result
      (StringValue string) -> (StringText string)
      _ -> (StringText "")

value_to_string : Value -> String
value_to_string value =
   case value of
      (StringValue result) -> result
      (TextValue text) ->
         case text of
            (StringText result) -> result
            (AugmentedText rich_text) ->
               (String.concat
                  (List.map (value_to_string) rich_text.content)
               )

            NewlineText -> "\n"

      _ -> (StringText "")

value_to_dict : Value -> (Dict.Dict String Value)
value_to_dict value =
   case value of
      (StructureValue dict) -> dict
      (ListValue dict) -> dict
      _ -> (Dict.empty)

value_to_address : Value -> (List String)
value_to_address value =
   case value of
      (PointerValue result) -> result
      _ -> []

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
                           (List.append base.content other_text_data.content)
                     }
                  )
               else
                  (AugmentedText
                     {text_data |
                        content =
                           (List.append
                              base.content
                              (List.singleton other_text_data)
                           )
                     }
                  )

            other ->
               (AugmentedText
                  {text_data |
                     content =
                        (List.append base.content (List.singleton other))
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
   case type_name of
      "bool" -> (BoolValue False)
      "float" -> (FloatValue 0.0)
      "int" -> (IntValue 0)
      "text" -> (TextValue (StringText ""))
      "string" -> (StringValue "")
      "list" -> (ListValue (Dict.empty))
      "ptr" -> (PointerValue [])
      other ->
         case (Dict.get other state.user_types) of
            (Just default) -> default
            Nothing -> (StringValue ("Unknown type '" + other + "'"))

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
                  (Just value) ->
                     (Just
                        (apply_at_address
                           next_address
                           fun
                           (value_to_dict value)
                        )
                     )

                  Nothing -> Nothing
            )
         )
