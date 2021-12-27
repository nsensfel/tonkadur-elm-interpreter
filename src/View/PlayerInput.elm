module View.PlayerInput exposing (get_html)

-- Elm -------------------------------------------------------------------------
import List
import Html
import Html.Attributes
import Html.Events

-- Local Module ----------------------------------------------------------------
import Struct.Event
import Struct.Model
import Struct.UI

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------
get_choice_html : (
      (Int, (Html.Html Struct.Event.Type)) ->
      (Html.Html Struct.Event.Type)
   )
get_choice_html data =
   let (ix, html) = data in
   (Html.div
      [
         (Html.Attributes.class "tonkadur-choice"),
         (Html.Events.onClick (Struct.Event.ChoiceSelected ix))
      ]
      [html]
   )

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
get_html : Struct.Model.Type -> (Html.Html Struct.Event.Type)
get_html model =
   if (List.isEmpty model.ui.displayed_choices)
   then
      case model.ui.input of
         Struct.UI.NoInput -> (Html.div [] [])
         Struct.UI.IntegerInput ->
            (Html.div
               [
                  (Html.Attributes.class "tonkadur-input")
               ]
               [
                  (Html.div
                     [
                        (Html.Attributes.class "tonkadur-input-instruction")
                     ]
                     [
                        (Html.text
                           (
                              "A number between "
                              ++ (String.fromInt model.ui.min)
                              ++ " and "
                              ++ (String.fromInt model.ui.max)
                              ++ " is expected:"
                           )
                        )
                     ]
                  ),
                  (Html.input
                     [
                        (Html.Attributes.class "tonkadur-input-field"),
                        (Html.Attributes.min (String.fromInt model.ui.min)),
                        (Html.Attributes.max (String.fromInt model.ui.max))
                     ]
                     [
                     ]
                  )
               ]
            )

         Struct.UI.StringInput ->
            (Html.div
               [
                  (Html.Attributes.class "tonkadur-input")
               ]
               [
                  (Html.div
                     [
                        (Html.Attributes.class "tonkadur-input-instruction")
                     ]
                     [
                        (Html.text
                           (
                              "A string of size between "
                              ++ (String.fromInt model.ui.min)
                              ++ " and "
                              ++ (String.fromInt model.ui.max)
                              ++ " characters is expected:"
                           )
                        )
                     ]
                  ),
                  (Html.input
                     [
                        (Html.Attributes.class "tonkadur-input-field"),
                        (Html.Attributes.minlength model.ui.min),
                        (Html.Attributes.maxlength model.ui.max)
                     ]
                     [
                     ]
                  )
               ]
            )

         Struct.UI.CommandInput ->
            (Html.div
               [
                  (Html.Attributes.class "tonkadur-input")
               ]
               [
                  (Html.div
                     [
                        (Html.Attributes.class "tonkadur-input-instruction")
                     ]
                     [
                        (Html.text
                           (
                              "A space-separated list of strings (total size "
                              ++ " between "
                              ++ (String.fromInt model.ui.min)
                              ++ " and "
                              ++ (String.fromInt model.ui.max)
                              ++ " characters) is expected:"
                           )
                        )
                     ]
                  ),
                  (Html.input
                     [
                        (Html.Attributes.class "tonkadur-input-field"),
                        (Html.Attributes.minlength model.ui.min),
                        (Html.Attributes.maxlength model.ui.max)
                     ]
                     [
                     ]
                  )
               ]
            )
   else
      (Html.div
         [
            (Html.Attributes.class "tonkadur-choice-list")
         ]
         (List.map (get_choice_html) model.ui.displayed_choices)
      )
