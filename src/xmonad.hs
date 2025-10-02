import Data.List as L
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Actions.RotSlaves
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig
import XMonad.Util.Loggers

import Graphics.X11.ExtraTypes.XF86

import XMonadConfig.GruvboxMaterial

{- FOURMOLU_DISABLE -}

--
-- The main function
--

main :: IO ()
main = xmonad
     . docks
     . ewmhFullscreen
     . ewmh
     . withNavigation2DConfig def
     . withSB myXmobar
     $ myConfig

myConfig = def
    { modMask            = myModMask
    , layoutHook         = myLayouts
    , terminal           = myTerminal
    , borderWidth        = myBorderWidth
    , normalBorderColor  = myNormalBorderColor
    , focusedBorderColor = myFocusedBorderColor
    , workspaces         = myWorkspaces
    , manageHook         = myManageHook
    , keys               = (`mkKeymap` myKeymap)
    } 

--
-- Configuration
--

myTerminal           = "wezterm"
myModMask            = mod4Mask
myBorderWidth        = 5
myNormalBorderColor  = "#32302f"
myFocusedBorderColor = "#32302f"
myWorkspaces         = ["code", "chat", "web", "games", "misc"]
myLauncher           = "dmenu_run -fn 'Fisa Code-10'"
myFileManager        = "thunar"
myScrot              = "scrot -s '%Y%m%d_%H%M%S.png' -e 'mv $f ~/Pictures/scrots/'"


--
-- Keybindings
--

myKeymap =
    --
    -- LeaderKey
    --

    -- spawning programs
    [ ("M-a n", spawn myTerminal   )
    , ("M-a e", spawn myLauncher   )
    , ("M-a t", spawn myFileManager)
    , ("M-a s", spawn myScrot      )

    -- toggling layouts
    , ("M-a d", sendMessage $ JumpToLayout "dynamic tiling")
    , ("M-a m", sendMessage $ JumpToLayout "maximised"     )
    , ("M-a f", sendMessage $ JumpToLayout "fullscreen"    )

    -- quit window
    , ("M-a q", kill)


    --
    -- Layout Navigation
    --

    -- directional navigation of windows
    , ("M-<Up>"     , windowGo   U False)
    , ("M-<Left>"   , windowGo   L False)
    , ("M-<Down>"   , windowGo   D False)
    , ("M-<Right>"  , windowGo   R False)

    -- cycle windows
    , ("M-<Page_Up>"  , windows W.focusUp  )
    , ("M-<Page_Down>", windows W.focusDown)

    -- window rotation
    , ("M-C-<Page_Up>",   rotAllUp)
    , ("M-C-<Page_Down>", rotAllDown)

    -- cycle workspaces
    , ("M-<Home>", moveTo Prev hiddenWS)
    , ("M-<End>" , moveTo Next hiddenWS)

    -- focus screens (directional)
    , ("M-C-<Home>", screenGo L False)  -- focus left monitor
    , ("M-C-<End>",  screenGo R False)  -- focus right monitor

    -- switch workspaces
    , ("M-1", windows $ W.greedyView "code" )
    , ("M-2", windows $ W.greedyView "chat" )
    , ("M-3", windows $ W.greedyView "web"  )
    , ("M-4", windows $ W.greedyView "games")
    , ("M-5", windows $ W.greedyView "misc" )

    -- send window to workspace
    , ("M-S-1", windows $ W.shift "code" )
    , ("M-S-2", windows $ W.shift "chat" )
    , ("M-S-3", windows $ W.shift "web"  )
    , ("M-S-4", windows $ W.shift "games")
    , ("M-S-5", windows $ W.shift "misc" )

    -- master pane manipulation
    , ("M-C-<Left>",  sendMessage Shrink)            -- control layout: shrink master
    , ("M-C-<Right>", sendMessage Expand)            -- control layout: expand master
    , ("M-S-<Left>",  sendMessage (IncMasterN 1))    -- shift windows: more in master
    , ("M-S-<Right>", sendMessage (IncMasterN (-1))) -- shift windows: fewer in master

    -- master window operations
    , ("M-<Delete>", windows W.focusMaster)
    , ("M-S-<Delete>", windows W.swapMaster)


    --
    -- Multimedia
    --

    , ("<XF86AudioPlay>"       , spawn "mpc toggle")
    , ("<XF86AudioStop>"       , spawn "mpc stop"  )
    , ("<XF86AudioNext>"       , spawn "mpc next"  )
    , ("<XF86AudioPrev>"       , spawn "mpc prev"  )
    , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -10%")
    , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +10%")
    ]


--
-- Layouts
--

myLayouts = myTile ||| myMax ||| myFull
  where
    -- our layouts
    myTile     = renamed [Replace "dynamic tiling"] . avoidStruts . myGaps $ Tall nmaster delta ratio
    myMax      = renamed [Replace "maximised"     ] . avoidStruts . myGaps $ Full
    myFull     = renamed [Replace "fullscreen"    ] . noBorders            $ Full
    -- add a configurable amount of space around windows.
    myGaps     = spacingRaw False (Border 10 10 10 10) True (Border 10 10 10 10) True
    -- layout configuration
    nmaster    = 1      -- Default number of windows in the master pane
    ratio      = 2/3    -- Default proportion of screen occupied by master pane
    delta      = 3/100  -- Percent of screen to increment by when resizing panes


--
-- Statusbar
--

myXmobar = statusBarPropTo "_XMONAD_LOG_1" "xmobar" (pure myXmobarPP)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = grey0         " "
    , ppCurrent         = blue   . wrap " " ""
    , ppTitle           = grey2  . wrap (grey0 " <fn=1>[</fn> ") (grey0 " <fn=1>]</fn>") . shorten 32
    , ppVisible         = purple . wrap " " ""
    , ppHidden          = grey0  . wrap " " ""
    , ppHiddenNoWindows = grey0  . wrap " " ""
    , ppUrgent          = red    . wrap " " ""
    , ppLayout          = aqua   . wrap (grey0 " <fn=1>[</fn> ") (grey0 " <fn=1>]</fn> ")
    , ppOrder           = \[ws, l, _] -> [ws, l]
    }


--
-- ManageHook
--

(~?) :: Eq a => Query [a] -> [a] -> Query Bool
q ~? x = fmap (x `L.isInfixOf`) q

myManageHook = composeAll
    [ className =? "Thunar"     --> doFloat
    , className =? "Ristretto"  --> doFloat
    , className =? "Tk"         --> doFloat -- gui development (python)
    , className =? "TkFDialog"  --> doFloat -- ^^
    , className ~? "App"        --> doFloat -- gui development (java)
    ]
