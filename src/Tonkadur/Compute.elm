module Tonkadur.Compute exposing (compute)

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
add_text_effect : (
      Tonkadur.Types.State ->
      String ->
      (List Tonkadur.Types.Computation) ->
      (List Tonkadur.Types.Computation) ->
      Tonkadur.Types.Value
   )
add_text_effect state name parameters content =
   (Tonkadur.Types.TextValue
      (Tonkadur.Types.AugmentedText
         {
            content =
               (List.map
                  (\val ->
                     (Tonkadur.Types.value_to_text_or_string
                        (compute state val)
                     )
                  )
                  content
               ),
            effect_name = name,
            effect_parameters = (List.map (compute state) parameters)
         }
      )
   )

address : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
address state param =
   case (compute state param) of
      (Tonkadur.Types.PointerValue address_v) ->
         (Tonkadur.Types.PointerValue address_v)

      (Tonkadur.Types.StringValue singleton) ->
         (Tonkadur.Types.PointerValue (List.singleton singleton))

      _ -> (Tonkadur.Types.PointerValue [])

unsupported_cast : String -> String -> Tonkadur.Types.Value
unsupported_cast from to =
   (Tonkadur.Types.StringValue
      ("Unsupported cast from " ++ from ++ " to " ++ to ++ ".")
   )

cast : (
      Tonkadur.Types.State ->
      String ->
      String ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
cast state from to param =
   case (compute state param) of
      (Tonkadur.Types.BoolValue bool) ->
         case to of
            "string" ->
               if bool
               then (Tonkadur.Types.StringValue "true")
               else (Tonkadur.Types.StringValue "false")

            "text" ->
               if bool
               then
                  (Tonkadur.Types.TextValue (Tonkadur.Types.StringText "true"))
               else
                  (Tonkadur.Types.TextValue (Tonkadur.Types.StringText "false"))

            "bool" -> (Tonkadur.Types.BoolValue bool)
            _ -> (unsupported_cast from to)

      (Tonkadur.Types.FloatValue float) ->
         case to of
            "string" -> (Tonkadur.Types.StringValue (String.fromFloat float))
            "text" ->
               (Tonkadur.Types.TextValue
                  (Tonkadur.Types.StringText (String.fromFloat float))
               )

            "int" -> (Tonkadur.Types.IntValue (floor float))
            "float" -> (Tonkadur.Types.FloatValue float)
            _ -> (unsupported_cast from to)

      (Tonkadur.Types.IntValue int) ->
         case to of
            "string" -> (Tonkadur.Types.StringValue (String.fromInt int))
            "text" ->
               (Tonkadur.Types.TextValue
                  (Tonkadur.Types.StringText (String.fromInt int))
               )

            "float" -> (Tonkadur.Types.FloatValue (toFloat int))
            "bool" -> (Tonkadur.Types.BoolValue (not (int == 0)))
            "int" -> (Tonkadur.Types.IntValue int)
            _ -> (unsupported_cast from to)

      (Tonkadur.Types.TextValue text_v) ->
         let
            as_string =
               (Tonkadur.Types.value_to_string
                  (Tonkadur.Types.TextValue text_v)
               )
         in
         case to of
            "string" -> (Tonkadur.Types.StringValue as_string)
            "float" ->
               case (String.toFloat as_string) of
                  Nothing -> (unsupported_cast from to)
                  (Just result) -> (Tonkadur.Types.FloatValue result)

            "int" ->
               case (String.toInt as_string) of
                  Nothing -> (unsupported_cast from to)
                  (Just result) -> (Tonkadur.Types.IntValue result)

            "bool" ->
               (Tonkadur.Types.BoolValue
                  ((String.toLower (String.trim as_string)) == "true")
               )

            "text" -> (Tonkadur.Types.TextValue text_v)
            _ -> (unsupported_cast from to)

      (Tonkadur.Types.StringValue string) ->
         case to of
            "string" -> (Tonkadur.Types.StringValue string)
            "float" ->
               case (String.toFloat string) of
                  Nothing -> (unsupported_cast from to)
                  (Just result) -> (Tonkadur.Types.FloatValue result)

            "int" ->
               case (String.toInt string) of
                  Nothing -> (unsupported_cast from to)
                  (Just result) -> (Tonkadur.Types.IntValue result)

            "bool" ->
               (Tonkadur.Types.BoolValue
                  ((String.toLower (String.trim string)) == "true")
               )

            "text" ->
               (Tonkadur.Types.TextValue (Tonkadur.Types.StringText string))

            _ -> (unsupported_cast from to)

      _ -> (unsupported_cast from to)

constant : (
      Tonkadur.Types.State ->
      String ->
      String ->
      Tonkadur.Types.Value
   )
constant state target_type as_string =
   case target_type of
      "string" -> (Tonkadur.Types.StringValue as_string)
      "float" ->
         case (String.toFloat as_string) of
            Nothing -> (unsupported_cast as_string target_type)
            (Just result) -> (Tonkadur.Types.FloatValue result)

      "int" ->
         case (String.toInt as_string) of
            Nothing -> (unsupported_cast as_string target_type)
            (Just result) -> (Tonkadur.Types.IntValue result)

      "text" ->
         (Tonkadur.Types.TextValue (Tonkadur.Types.StringText as_string))

      _ -> (unsupported_cast as_string target_type)

extra_computation : (
      Tonkadur.Types.State ->
      String ->
      (List Tonkadur.Types.Computation) ->
      Tonkadur.Types.Value
   )
extra_computation state name parameters =
   case name of
      _ ->
         (Tonkadur.Types.StringValue
            ("Unsupported extra computation '" ++ name ++ "'")
         )

if_else : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
if_else state condition if_true if_false =
   if (Tonkadur.Types.value_to_bool (compute state condition))
   then (compute state if_true)
   else (compute state if_false)

last_choice_index : Tonkadur.Types.State -> Tonkadur.Types.Value
last_choice_index state = (Tonkadur.Types.IntValue state.last_choice_index)

newline : Tonkadur.Types.State -> Tonkadur.Types.Value
newline state = (Tonkadur.Types.TextValue Tonkadur.Types.NewlineText)

next_allocable_address : Tonkadur.Types.State -> Tonkadur.Types.Value
next_allocable_address state =
   case state.freed_addresses of
      [] ->
         (Tonkadur.Types.PointerValue
            [(".alloc." ++ (String.fromInt state.allocated_data))]
         )

      (available_address :: _) ->
         (Tonkadur.Types.PointerValue [available_address])

operation : (
      Tonkadur.Types.State ->
      String ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
operation state name param0 param1 =
   let
      value0 = (compute state param0)
      value1 = (compute state param1)
   in
   case name of
      "divide" ->
         case value0 of
            (Tonkadur.Types.IntValue val) ->
               (Tonkadur.Types.IntValue
                  (val // (Tonkadur.Types.value_to_int value1))
               )

            _ ->
               (Tonkadur.Types.FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     / (Tonkadur.Types.value_to_float value1)
                  )
               )

      "minus" ->
         case value0 of
            (Tonkadur.Types.IntValue val) ->
               (Tonkadur.Types.IntValue
                  (val - (Tonkadur.Types.value_to_int value1))
               )

            _ ->
               (Tonkadur.Types.FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     - (Tonkadur.Types.value_to_float value1)
                  )
               )

      "modulo" ->
         (Tonkadur.Types.IntValue
            (modBy
               (Tonkadur.Types.value_to_int value0)
               (Tonkadur.Types.value_to_int value1)
            )
         )

      "plus" ->
         case value0 of
            (Tonkadur.Types.IntValue val) ->
               (Tonkadur.Types.IntValue
                  (val + (Tonkadur.Types.value_to_int value1))
               )

            _ ->
               (Tonkadur.Types.FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     + (Tonkadur.Types.value_to_float value1)
                  )
               )

      "power" ->
         case value0 of
            (Tonkadur.Types.IntValue val) ->
               (Tonkadur.Types.IntValue
                  (val ^ (Tonkadur.Types.value_to_int value1))
               )

            _ ->
               (Tonkadur.Types.FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     ^ (Tonkadur.Types.value_to_float value1)
                  )
               )

      "times" ->
         case value0 of
            (Tonkadur.Types.IntValue val) ->
               (Tonkadur.Types.IntValue
                  (val * (Tonkadur.Types.value_to_int value1))
               )

            _ ->
               (Tonkadur.Types.FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     * (Tonkadur.Types.value_to_float value1)
                  )
               )

      "and" ->
         (Tonkadur.Types.BoolValue
            (
               (Tonkadur.Types.value_to_bool value0)
               && (Tonkadur.Types.value_to_bool value1)
            )
         )

      "not" ->
         (Tonkadur.Types.BoolValue (not (Tonkadur.Types.value_to_bool value0)))

      "less_than" ->
         case value0 of
            (Tonkadur.Types.BoolValue bool) ->
               (Tonkadur.Types.BoolValue
                  ((Tonkadur.Types.value_to_bool value1) && (not bool))
               )

            (Tonkadur.Types.FloatValue float) ->
               (Tonkadur.Types.BoolValue
                  (float < (Tonkadur.Types.value_to_float value1))
               )

            (Tonkadur.Types.IntValue int) ->
               (Tonkadur.Types.BoolValue
                  (int < (Tonkadur.Types.value_to_int value1))
               )

            (Tonkadur.Types.StringValue str) ->
               (Tonkadur.Types.BoolValue
                  (str < (Tonkadur.Types.value_to_string value1))
               )

            (Tonkadur.Types.PointerValue ptr) ->
               (Tonkadur.Types.BoolValue
                  (
                     (Tonkadur.Types.compare_pointers
                        ptr
                        (Tonkadur.Types.value_to_address value1)
                     )
                     > 0
                  )
               )

            _ -> (Tonkadur.Types.StringValue ("Not a comparable type."))

      "equals" -> (Tonkadur.Types.BoolValue (value0 == value1))

      other ->
         (Tonkadur.Types.StringValue
            ("Unknown operation '" ++ other ++ "'")
         )

relative_address : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
relative_address state base extra =
   (Tonkadur.Types.PointerValue
      (List.append
         (Tonkadur.Types.value_to_address (compute state base))
         (Tonkadur.Types.value_to_address (compute state extra))
      )
   )

size : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
size state computation =
   (Tonkadur.Types.IntValue
      (Dict.size (Tonkadur.Types.value_to_dict (compute state computation)))
   )


text : (
      Tonkadur.Types.State ->
      (List Tonkadur.Types.Computation) ->
      Tonkadur.Types.Value
   )
text state content =
   (List.foldl
      (\addition result ->
         (Tonkadur.Types.TextValue
            (Tonkadur.Types.append_text_content
               (Tonkadur.Types.value_to_text_or_string result)
               (Tonkadur.Types.value_to_text_or_string (compute state addition))
            )
         )
      )
      (Tonkadur.Types.TextValue
         (Tonkadur.Types.AugmentedText (Tonkadur.Types.default_text_data))
      )
      content
   )

value_of : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
value_of state computation =
   (List.foldl
      (\next_step object ->
         case (Dict.get next_step (Tonkadur.Types.value_to_dict object)) of
            (Just value) -> value
            Nothing ->
               (Tonkadur.Types.StringValue
                  "Segmentation Fault (incorrect address)"
               )
      )
      (Tonkadur.Types.StructureValue state.memory)
      (Tonkadur.Types.value_to_address (compute state computation))
   )

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
compute : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
compute state computation =
   case computation of
      (Tonkadur.Types.AddTextEffect effect_name effect_parameters content) ->
         (add_text_effect state effect_name effect_parameters content)

      (Tonkadur.Types.Address param) -> (address state param)
      (Tonkadur.Types.Cast from to value) -> (cast state from to value)
      (Tonkadur.Types.Constant true_type as_string) ->
         (constant state true_type as_string)

      (Tonkadur.Types.ExtraComputation name parameters) ->
         (extra_computation state name parameters)

      (Tonkadur.Types.IfElse condition if_true if_false) ->
         (if_else state condition if_true if_false)

      Tonkadur.Types.LastChoiceIndex -> (last_choice_index state)
      Tonkadur.Types.Newline -> (newline state)
      Tonkadur.Types.NextAllocableAddress -> (next_allocable_address state)
      (Tonkadur.Types.Operation name arg_0 arg_1) ->
         (operation state name arg_0 arg_1)
      (Tonkadur.Types.RelativeAddress base extra) ->
         (relative_address state base extra)

      (Tonkadur.Types.Size value) -> (size state value)
      (Tonkadur.Types.Text content) -> (text state content)
      (Tonkadur.Types.ValueOf address_c) -> (value_of state address_c)
