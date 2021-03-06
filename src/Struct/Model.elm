module Struct.Model exposing
   (
      Type,
      new
   )

-- Elm -------------------------------------------------------------------------

-- Tonkadur --------------------------------------------------------------------
import Tonkadur.Types

-- Local Module ----------------------------------------------------------------
import Struct.UI
import Struct.Flags

--------------------------------------------------------------------------------
-- TYPES -----------------------------------------------------------------------
--------------------------------------------------------------------------------
type alias Type =
   {
      tonkadur : Tonkadur.Types.State,
      flags : Struct.Flags.Type,
      ui : Struct.UI.Type
   }

--------------------------------------------------------------------------------
-- LOCAL -----------------------------------------------------------------------
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- EXPORTED --------------------------------------------------------------------
--------------------------------------------------------------------------------
new : Struct.Flags.Type -> Type
new flags =
   {
      tonkadur = (Tonkadur.Types.new_state flags.random_seed),
      flags = flags,
      ui = (Struct.UI.new)
   }
