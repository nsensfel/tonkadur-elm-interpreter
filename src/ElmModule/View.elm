module ElmModule.View exposing (view)

-- Elm -------------------------------------------------------------------------
import Html
import Html.Attributes

-- Local Module ----------------------------------------------------------------
import Struct.Event
import Struct.Model

import View.PlayerInput
--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
view : Struct.Model.Type -> (Html.Html Struct.Event.Type)
view model =
   (Html.div
      [
         (Html.Attributes.class "fullscreen-module")
      ]
      [
         (Html.div
            [
               (Html.Attributes.class "tonkadur-errors")
            ]
            model.ui.displayed_errors
         ),
         (Html.div
            [
               (Html.Attributes.class "tonkadur-texts")
            ]
            model.ui.displayed_texts
         ),
         (View.PlayerInput.get_html model)
      ]
   )
