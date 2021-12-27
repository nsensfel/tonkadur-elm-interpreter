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
      Tonkadur.Types.Value
   )
add_text_effect state name parameters content =
   (TextValue
      (AugmentedText
         {
            content = (List.map (compute state) content),
            effect_name = name,
            effect_parameters = parameters
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
      (PointerValue address) -> (PointerValue address)
      (StringValue singleton) -> (PointerValue (List.singleton singleton))
      _ -> (PointerValue [])

unsupported_cast : String -> String -> Tonkadur.Types.Value
unsupported_cast from to =
   (StringValue ("Unsupported cast from " + from + " to " + to + "."))

cast : (
      Tonkadur.Types.State ->
      String ->
      String ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
cast state from to param =
   case (compute state param) of
      (BoolValue bool) ->
         case to of
            "string" ->
               if bool
               then (StringValue "true")
               else (StringValue "false")

            "text" ->
               if bool
               then (TextValue (StringText "true"))
               else (TextValue (StringText "false"))

            "bool" -> (BoolValue bool)
            _ -> (unsupported_cast from to)

      (FloatValue float) ->
         case to of
            "string" -> (StringValue (String.fromFloat float))
            "text" -> (TextValue (StringText (String.fromFloat float)))
            "int" -> (IntValue (Math.floor float))
            "float" -> (FloatValue float)
            _ -> (unsupported_cast from to)

      (IntValue int) ->
         case to of
            "string" -> (StringValue (String.fromInt int))
            "text" -> (TextValue (StringText (String.fromInt int)))
            "float" -> (FloatValue (Math.toFloat int))
            "int" -> (IntValue int)
            _ -> (unsupported_cast from to)

      (TextValue text) ->
         let as_string = (Tonkadur.Types.value_to_string (TextValue text)) in
         case to of
            "string" -> (StringValue as_string)
            "float" ->
               case (String.toFloat as_string) of
                  Nothing -> (unsupported_cast from to)
                  (Just result) -> (FloatValue result)

            "int" ->
               case (String.toInt as_string) of
                  Nothing -> (unsupported_cast from to)
                  (Just result) -> (IntValue result)

            "text" -> (TextValue text)
            _ -> (unsupported_cast from to)

      (StringValue string) ->
         case to of
            "string" -> (StringValue string)
            "float" ->
               case (String.fromFloat string) of
                  Nothing -> (unsupported_cast from to)
                  (Just result) -> (FloatValue result)

            "int" ->
               case (String.toInt string) of
                  Nothing -> (unsupported_cast from to)
                  (Just result) -> (IntValue result)

            "text" -> (TextValue (StringText string))

            _ -> (unsupported_cast from to)

      _ -> (unsupported_cast from to)

constant : (
      Tonkadur.Types.State ->
      String ->
      String ->
      Tonkadur.Types.Value
   )
constant state target_type as_string =
   (cast state "string" target_type as_string)

extra_computation : (
      Tonkadur.Types.State ->
      String ->
      (List Tonkadur.Types.Computation) ->
      Tonkadur.Types.Value
   )
extra_computation state name parameters =
   case name of
      _ -> (StringValue ("Unsupported extra computation '" + name + "'"))

if_else : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
if_else state condition if_true if_false =
   if (WyrdType.to_boolean (compute state condition))
   then (compute state if_true)
   else (compute state if_false)

last_choice_index : Tonkadur.Types.State -> Tonkadur.Types.Value
last_choice_index state = (IntValue state.last_choice_index)

newline : Tonkadur.Types.State -> Tonkadur.Types.Value
newline state = (TextValue Newline)

next_allocable_address : Tonkadur.Types.State -> Tonkadur.Types.Value
next_allocable_address state =
   if (List.isEmpty state.freed_addresses)
   then (PointerValue [(".alloc." ++ (String.fromInt state.allocated_data))])
   else (PointerValue [state.freed_addresses[0]])

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
            (IntValue val) ->
               (IntValue (val // (Tonkadur.Types.value_to_int value1)))

            _ ->
               (FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     / (Tonkadur.Types.value_to_float value1)
                  )
               )

      "minus" ->
         case value0 of
            (IntValue val) ->
               (IntValue (val - (Tonkadur.Types.value_to_int value1)))

            _ ->
               (FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     - (Tonkadur.Types.value_to_float value1)
                  )
               )

      "modulo" ->
         (IntValue
            (modBy
               (Tonkadur.Types.value_to_int value0)
               (Tonkadur.Types.value_to_int value1)
            )
         )

      "plus" ->
         case value0 of
            (IntValue val) ->
               (IntValue (val + (Tonkadur.Types.value_to_int value1)))

            _ ->
               (FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     + (Tonkadur.Types.value_to_float value1)
                  )
               )

      "power" ->
         case value0 of
            (IntValue val) ->
               (IntValue (val ^ (Tonkadur.Types.value_to_int value1)))

            _ ->
               (FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     ^ (Tonkadur.Types.value_to_float value1)
                  )
               )

      "times" ->
         case value0 of
            (IntValue val) ->
               (IntValue (val * (Tonkadur.Types.value_to_int value1)))

            _ ->
               (FloatValue
                  (
                     (Tonkadur.Types.value_to_float value0)
                     * (Tonkadur.Types.value_to_float value1)
                  )
               )

      "and" ->
         (BoolValue
            (and
               (Tonkadur.Types.value_to_bool value0)
               (Tonkadur.Types.value_to_bool value1)
            )
         )

      "not" -> (BoolValue (not (Tonkadur.Types.value_to_bool value0)))

      "less_than" ->
         case value0 of
            (BoolValue bool) ->
               (and (Tonkadur.Types.value_to_bool value1) (not boot))

            (FloatValue float) ->
               (BoolValue (float < (Tonkadur.Types.value_to_float value1)))

            (IntValue int) ->
               (BoolValue (int < (Tonkadur.Types.value_to_int value1)))

            (StringValue str) ->
               (BoolValue (str < (Tonkadur.Types.value_to_string value1)))

            (PointerValue ptr) ->
               (BoolValue
                  (
                     (Tonkadur.Types.compare_pointers
                        ptr
                        (Tonadur.Wyrd.value_to_dict value1)
                     )
                     > 0
                  )
               )

      "equals" -> (value0 == value1)

relative_address : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
relative_address state base extra =
   (PointerValue
      (List.append
         (Tonkadur.Types.value_to_list (compute state base))
         (Tonkadur.Types.value_to_list (compute state extra))
      )
   )

size : (
      Tonkadur.Types.State ->
      Tonkadur.Types.Computation ->
      Tonkadur.Types.Value
   )
size state computation =
   (IntValue
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
         (TextValue
            (Tonkadur.Types.append_text_content
               (Tonkadur.Types.value_to_text result)
               (Tonkadur.Types.value_to_text (compute state addition))
            )
         )
      )
      (TextValue (Tonkadur.Types.default_text_data))
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
            Nothing -> (StringValue "Segmentation Fault (incorrect address)")
            (Just value) -> value
      )
      (StructureValue state.memory)
      (Tonkadur.Types.value_to_list (compute state computation))
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
      (AddTextEffect effect_name effect_parameters content) ->
         (add_text_effect state effect_name effect_parameters content)

      (Address param) -> (address state param)
      (Cast from to value) -> (cast state from to value)
      (Constant true_type as_string) -> (constant state true_type as_string)
      (ExtraComputation name parameters) ->
         (extra_computation state name parameters)

      (IfElse condition if_true if_false) ->
         (if_else state condition if_true if_false)

      LastChoiceIndex -> (last_choice_index state)
      Newline -> (newline state)
      NextAllocableAddress -> (next_allocable_address state)
      (Operation name arg_0 arg_1) -> (operation state name arg_0 arg_1)
      (RelativeAddress base extra) -> (relative_address state base extra)
      (Size value) -> (size state value)
      (Text content) -> (text state content)
      (ValueOf address) -> (value_of state address)
