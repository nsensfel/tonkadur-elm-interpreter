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
      random_seed : Int
   }

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------
parameter_as_url : (List String) -> String
parameter_as_url parameter =
   case parameter of
      [name, value] -> (name ++ "=" ++ value)
      _ -> ""

get_first : (a -> Bool) -> (List a) -> (Maybe a)
get_first fun list =
   (List.head (List.filter fun list))

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
force_get_parameter : Type -> String -> String
force_get_parameter flags parameter =
   case
      (get_first
         (\e -> ((List.head e) == (Just parameter)))
         flags.url_parameters
      )
   of
      Nothing -> ""
      (Just a) ->
         case (List.tail a) of
            (Just (h :: t)) -> h
            _ -> ""

get_parameters_as_url : Type -> String
get_parameters_as_url flags =
   (List.foldl
      (\parameter -> \current_parameters ->
         (current_parameters ++ "&" ++ (parameter_as_url parameter))
      )
      ""
      flags.url_parameters
   )
