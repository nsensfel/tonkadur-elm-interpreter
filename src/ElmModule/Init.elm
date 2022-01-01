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
   -- TODO: read flags and request story.
   ((Struct.Model.new), (Comm.LoadStory.request "/story/0.json"))
