module Struct.UI exposing (..)

-- Elm -------------------------------------------------------------------------
import List
import Html

-- Local Module ----------------------------------------------------------------
import Struct.Event

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------
type InputType =
   NoInput
   | IntegerInput
   | StringInput
   | CommandInput

type alias Type =
   {
      displayed_texts : (List (Html.Html Struct.Event.Type)),
      displayed_errors : (List (Html.Html Struct.Event.Type)),
      displayed_choices : (List (Int, (Html.Html Struct.Event.Type))),
      min : Int,
      max : Int,
      input : InputType
   }

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
new : Type
new =
   {
      displayed_texts = [],
      displayed_errors = [],
      displayed_choices = [],
      min = -1,
      max = -1,
      input = NoInput
   }

display_text : (Html.Html Struct.Event.Type) -> Type -> Type
display_text html ui =
   {ui | displayed_texts = (List.append ui.displayed_texts [html])}

display_error : (Html.Html Struct.Event.Type) -> Type -> Type
display_error html ui =
   {ui | displayed_errors = (List.append ui.displayed_errors [html])}

display_string_error : String -> Type -> Type
display_string_error string ui =
   {ui |
      displayed_errors = (List.append ui.displayed_errors [(Html.text string)])
   }

display_choice : Int -> (Html.Html Struct.Event.Type) -> Type -> Type
display_choice ix html ui =
   {ui | displayed_choices = (List.append ui.displayed_choices [(ix, html)])}

prompt_string : Int -> Int -> Type -> Type
prompt_string min max ui = {ui | min = min, max = max, input = StringInput}

prompt_integer : Int -> Int -> Type -> Type
prompt_integer min max ui = {ui | min = min, max = max, input = IntegerInput}

prompt_command : Int -> Int -> Type -> Type
prompt_command min max ui = {ui | min = min, max = max, input = CommandInput}

clear_prompt : Type -> Type
clear_prompt ui = {ui | min = -1, max = -1, input = NoInput}

clear_displayed_texts : Type -> Type
clear_displayed_texts ui = {ui | displayed_texts = []}

clear_displayed_errors : Type -> Type
clear_displayed_errors ui = {ui | displayed_errors = []}

clear_displayed_choices : Type -> Type
clear_displayed_choices ui = {ui | displayed_choices = []}
