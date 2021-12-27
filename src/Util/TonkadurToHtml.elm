module Util.TonkadurToHtml exposing (..)

-- Elm -------------------------------------------------------------------------
import List
import Html
import Html.Attributes

-- Tonkadur --------------------------------------------------------------------
import Tonkadur.Types

-- Local Module ----------------------------------------------------------------
import Struct.Event

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
convert : Tonkadur.Types.Value -> (Html.Html Struct.Event.Type)
convert value =
   case (Tonkadur.Types.value_to_text_or_string value) of
      (Tonkadur.Types.AugmentedText text_data) ->
         (Html.div
            [
               (Html.Attributes.class "tonkadur-value")
               -- TODO: more classes depending on effects.
            ]
            (List.map
               (\v -> (convert (Tonkadur.Types.TextValue v)))
               text_data.content
            )
         )

      (Tonkadur.Types.StringText string) ->
         (Html.div
            [
               (Html.Attributes.class "tonkadur-value")
            ]
            [(Html.text string)]
         )

      Tonkadur.Types.NewlineText -> (Html.br [] [])
