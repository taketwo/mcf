module MCF.Polybar
  ( polybarColor
  , polybarAction
  ) where

import XMonad.Hooks.DynamicLog (wrap)

-- | Use polybar escape codes to output a string with given foreground and background colors.
polybarColor :: String  -- ^ foreground color: a color name, or #rrggbb format
             -> String  -- ^ background color (can be empty)
             -> String  -- ^ output string
             -> String
polybarColor fg bg = wrap t1 t2
  where
    t1 = concat ["%{", if null bg then "" else "B" ++ bg ++ " ", "F" ++ fg, "}"]
    t2 = concat ["%{", if null bg then "" else "B- ", "F-}"]

-- | Use polybar escape codes to output a string with a given action.
polybarAction :: String  -- ^ command (colons should be escaped with a backslash)
              -> Int     -- ^ mouse button index
              -> String  -- ^ inner text
              -> String
polybarAction cmd btn txt = "%{A" ++ show btn ++ ":" ++ cmd ++ ":}" ++ txt ++ "%{A}"
