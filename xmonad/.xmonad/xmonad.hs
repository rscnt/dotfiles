-- most configs were extracted from http://web.mit.edu/nelhage/Public/xmonad.hs
-- mirror: https://phabricator.destruction.io/file/data/culjp6p72tuneu56viz5/PHID-FILE-f46key5kgx67rn57rj65/xmonad.
import XMonad
import XMonad.Config.Desktop
import XMonad.Actions.GridSelect
import XMonad.Util.EZConfig
import XMonad.Layout.Spacing
import XMonad.Layout.Circle
import XMonad.Layout.Spiral
import XMonad.Layout.ThreeColumns
import XMonad.Hooks.ManageDocks
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W


-- main = xmonad $ baseConfig
--     { terminal    = "termite"
--     , modMask     = mod4Mask
--     }

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "termite"
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- window spacing
mySpacing = 2

-- border colors
myNormalBorderColor  = "#f5f6f7"
myFocusedBorderColor = "#f7f6f5"

renameWS :: String -> X ()
renameWS newTag = windows $ \s -> let old = W.tag $ W.workspace $ W.current s
                                  in W.renameTag old newTag s

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = spacing mySpacing $ avoidStruts $
           tiled
           ||| Mirror tiled
           ||| Full
           ||| threeCol
           ||| spiral (4/3)
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     threeCol = ThreeCol nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 2/100


main = xmonad defaults

defaults = desktopConfig {
      -- simple stuff
        terminal           = myTerminal,
        modMask            = myModMask,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
      -- hooks, layouts
        layoutHook         = myLayout
    }
    `additionalKeysP`
    [ ("M-p", spawn "rofi -combi-modi window,run -show combi -font 'Roboto 10' -o 85")
    , ("M-g", goToSelected defaultGSConfig)
    , ("M-w", spawn "feh --randomize --bg-scale /home/r/.wallpapers/")
    , ("M-f", spawn "rofi  -show fb -modi fb:/home/r/.bin/rofi-file-browser.sh -font 'Roboto 10' -o 85")
    ]

