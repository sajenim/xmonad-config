import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.Navigation2D
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeys)

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
    } `additionalKeys` myKeybindings


--
-- Configuration
--

myTerminal           = "wezterm"
myModMask            = mod4Mask
myBorderWidth        = 5
myNormalBorderColor  = "#32302f"
myFocusedBorderColor = "#32302f"
myWorkspaces         = ["code", "chat", "web", "games", "misc"]
myLauncher           = "rofi -modi run,calc -show run"
myScrot              = "scrot -s '%Y%m%d_%H%M%S.png' -e 'mv $f ~/Pictures/scrots/'"
volumeDown           = "pactl set-sink-volume @DEFAULT_SINK@ -10%"
volumeUp             = "pactl set-sink-volume @DEFAULT_SINK@ +10%"


--
-- Keybindings
--

myKeybindings =
    --
    -- Launching and killing programs
    --

    [ ((myModMask,               xK_Return), spawn myTerminal)
    , ((myModMask,               xK_Tab   ), spawn myLauncher)
    , ((myModMask,               xK_s     ), spawn myScrot   ) 
    , ((myModMask,               xK_Escape), kill            )
    , ((myModMask .|. shiftMask, xK_q     ), io exitSuccess  )


    --
    -- Multimedia
    --

    , ((noModMask, xF86XK_AudioPlay       ), spawn "mpc toggle")
    , ((noModMask, xF86XK_AudioStop       ), spawn "mpc stop"  )
    , ((noModMask, xF86XK_AudioNext       ), spawn "mpc next"  )
    , ((noModMask, xF86XK_AudioPrev       ), spawn "mpc prev"  )
    , ((noModMask, xF86XK_AudioLowerVolume), spawn volumeDown  )
    , ((noModMask, xF86XK_AudioRaiseVolume), spawn volumeUp    )


    --
    -- Navigation
    --

    -- directional navigation of windows
    , ((myModMask,               xK_Right), windowGo   R False)
    , ((myModMask,               xK_Left ), windowGo   L False)
    , ((myModMask,               xK_Up   ), windowGo   U False)
    , ((myModMask,               xK_Down ), windowGo   D False)

    -- swap adjacent windows
    , ((myModMask .|. shiftMask, xK_Right), windowSwap R False)
    , ((myModMask .|. shiftMask, xK_Left ), windowSwap L False)
    , ((myModMask .|. shiftMask, xK_Up   ), windowSwap U False)
    , ((myModMask .|. shiftMask, xK_Down ), windowSwap D False)

    -- workspaces
    , ((myModMask, xK_Page_Up  ), moveTo Prev hiddenWS)
    , ((myModMask, xK_Page_Down), moveTo Next hiddenWS)

    -- layouts
    , ((myModMask,               xK_t), sendMessage $ JumpToLayout "dynamic tiling"        )
    , ((myModMask,               xK_b), sendMessage $ JumpToLayout "binary space partition")
    , ((myModMask,               xK_m), sendMessage $ JumpToLayout "maximised"             )
    , ((myModMask,               xK_f), sendMessage $ JumpToLayout "fullscreen"            )
    , ((myModMask .|. shiftMask, xK_t), withFocused $ windows . W.sink                     )


    --
    -- DynamicTiling
    --

    -- move/swap focus of master
    , ((myModMask,               xK_BackSpace), windows W.focusMaster)
    , ((myModMask .|. shiftMask, xK_BackSpace), windows W.swapMaster )

    -- shrink/expand the master area
    , ((myModMask, xK_Home), sendMessage Shrink)
    , ((myModMask, xK_End ), sendMessage Expand)

    -- number of windows in the master area
    , ((myModMask .|. shiftMask, xK_Home), sendMessage (IncMasterN 1)   )
    , ((myModMask .|. shiftMask, xK_End ), sendMessage (IncMasterN (-1)))


    --
    -- BinarySpacePartition
    --

    -- expand windows
    , ((myModMask .|. mod1Mask, xK_Right), sendMessage $ ExpandTowardsBy R 0.01)
    , ((myModMask .|. mod1Mask, xK_Left ), sendMessage $ ExpandTowardsBy L 0.01)
    , ((myModMask .|. mod1Mask, xK_Down ), sendMessage $ ExpandTowardsBy D 0.01)
    , ((myModMask .|. mod1Mask, xK_Up   ), sendMessage $ ExpandTowardsBy U 0.01)

    -- shrink windows
    , ((myModMask .|. mod1Mask .|. controlMask, xK_Right), sendMessage $ ShrinkFromBy R 0.01)
    , ((myModMask .|. mod1Mask .|. controlMask, xK_Left ), sendMessage $ ShrinkFromBy L 0.01)
    , ((myModMask .|. mod1Mask .|. controlMask, xK_Down ), sendMessage $ ShrinkFromBy D 0.01)
    , ((myModMask .|. mod1Mask .|. controlMask, xK_Up   ), sendMessage $ ShrinkFromBy U 0.01)

    -- layout manipulation
    , ((myModMask .|. mod1Mask, xK_Return   ), sendMessage Rotate           )
    , ((myModMask .|. mod1Mask, xK_BackSpace), sendMessage Swap             )
    , ((myModMask .|. mod1Mask, xK_Home     ), sendMessage $ SplitShift Prev)
    , ((myModMask .|. mod1Mask, xK_End      ), sendMessage $ SplitShift Next)
    ]


--
-- Layouts
--

myLayouts = myTile ||| myBsp ||| myMax ||| myFull
  where
    -- our layouts
    myTile     = renamed [Replace "dynamic tiling"        ] . avoidStruts . myGaps $ Tall nmaster delta ratio
    myBsp      = renamed [Replace "binary space partition"] . avoidStruts . myGaps $ emptyBSP
    myMax      = renamed [Replace "maximised"             ] . avoidStruts . myGaps $ Full
    myFull     = renamed [Replace "fullscreen"            ] . noBorders            $ Full
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

myManageHook = composeAll
    [ className =? "Thunar"    --> doFloat
    , className =? "Ristretto" --> doFloat
    , className =? "Tk"        --> doFloat -- python gui development
    , className =? "TkFDialog" --> doFloat -- ^^
    ]
