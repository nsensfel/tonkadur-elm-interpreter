module Struct.Flags exposing
   (
      Type,
      force_get_parameter,
      get_parameters_as_url
   )

-- Elm -------------------------------------------------------------------------
import List

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------
type alias Type =
   {
      url_parameters : (List (List String)),
      random_number : Int
   }

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------
parameter_as_url : (List String) -> String
parameter_as_url parameter =
   case parameter of
      [name, value] -> (name ++ "=" ++ value)
      _ -> ""

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
force_get_parameter : String -> Type -> String
force_get_parameter parameter flags = ""
   -- TODO: implement using Tactician Online's, but without Shared.Util.List

get_parameters_as_url : Type -> String
get_parameters_as_url flags =
   (List.foldl
      (\parameter -> \current_parameters ->
         (current_parameters ++ "&" ++ (parameter_as_url parameter))
      )
      ""
      flags.url_parameters
   )
