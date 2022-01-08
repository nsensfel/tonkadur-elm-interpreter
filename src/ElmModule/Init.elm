module ElmModule.Init exposing (init)

-- Local Module ----------------------------------------------------------------
import Struct.Flags
import Struct.Event
import Struct.Model

import Comm.LoadStory

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
init : Struct.Flags.Type -> (Struct.Model.Type, (Cmd Struct.Event.Type))
init flags =
   (
      (Struct.Model.new flags),
      (Comm.LoadStory.request
         (
            "/story/"
            ++ (Struct.Flags.force_get_parameter flags "story")
            ++ ".json"
         )
      )
   )
