--
-- xmonad config file.
--

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.DragPane
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns
import XMonad.Layout.OneBig
import XMonad.Layout.WindowNavigation
import XMonad.Prompt
import XMonad.Prompt.Ssh
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- Use windows key for mod
myModMask = mod4Mask

-- Terminal emulator
myTerminal = "urxvtc"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Convinces JAVA Swing applications that I am using Project Looking Glass
myStartupHook = setWMName "LG3D"

-- my workspaces
ws_gtd            = "α"
ws_web            = "β"
ws_dev            = "γ"
ws_webdev         = "δ"
ws_communications = "ε"
ws_network        = "ζ"
ws_top            = "η"
ws_raster         = "θ"
ws_vector         = "ι"
ws_pentesting     = "ω"

myWorkspaces = [ ws_gtd, ws_web, ws_dev, ws_webdev, ws_communications, ws_network, ws_top, ws_raster, ws_vector, ws_pentesting ]

-- special application rules
myManageHook = composeAll
    [ title     =? "mutt"               --> doShift ws_communications
    , title     =? "wyrd"               --> doShift ws_gtd
    , title     =? "task"               --> doShift ws_gtd
    , title     =? "htop"               --> doShift ws_top
    , className =? "Gimp"               --> doShift ws_raster
    , className =? "inkscape"           --> doShift ws_vector
    , className =? "Chromium"           --> doShift ws_web
    , className =? "Firefox"            --> doShift ws_web
    , className =? "VirtualBox"         --> doFloat
    , className =? "Xmessage"           --> doFloat
    , className =? "XCalc"              --> doFloat
    , className =? "Cinelerra"          --> doFloat
    , className =? "CinePaint"          --> doFloat
    , className =? "Synfigstudio"       --> doFloat
    , className =? "fontforge"          --> doFloat
    , className =? "xsane"              --> doFloat
    , className =? "lbe-ui-BrowserApp"  --> doFloat
    , className =? "Cssh"               --> doFloat
    , className =? "Vncviewer"          --> doFloat
    , className =? "Ssvnc"              --> doFloat
    , className =? "weka-gui-GUIChooser" --> doFloat
    , className =? "Firefox" <&&> resource =? "Dialog" --> doFloat
    , className =? "Xmessage"           --> doIgnore
    ]

-- layout
myLayout = tiled ||| Full ||| one ||| three
  where
    tiled = named "Default" (ResizableTall 1 (1/100) (1/2) [])
    three = named "Buff" (ThreeColMid 1 (3/100) (1/2))
    one   = named "One Big" (OneBig (11/16) (11/16))

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/chris/.xmonad/xmobarrc"
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ def
    { borderWidth = 2
    , modMask = myModMask
    , terminal = myTerminal
    , focusFollowsMouse = myFocusFollowsMouse
    , workspaces = myWorkspaces
    , handleEventHook = fullscreenEventHook
    , manageHook = manageDocks <+> myManageHook <+> manageHook def
    , layoutHook = avoidStruts $ smartBorders $ myLayout
    , logHook = do
            dynamicLogWithPP $ xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                } 
    , startupHook = myStartupHook
    }
    `additionalKeys`
    [ ((myModMask .|. shiftMask,    xK_z      ),  spawn "xscreensaver-command -lock")
    , ((myModMask,                  xK_Print  ),  spawn "scrot -b -s")
    , ((0,                          xK_Print  ),  spawn "scrot")

    -- Keys for Prompts
    , ((myModMask .|. controlMask,  xK_s      ),  sshPrompt def)

    -- Keys for ResizableTall layout
    , ((myModMask,                  xK_a),      sendMessage MirrorShrink)
    , ((myModMask,                  xK_z),      sendMessage MirrorExpand)

    -- Keys for WindowNavigation
    , ((myModMask,                  xK_Right  ),  sendMessage $ Go R)
    , ((myModMask,                  xK_Left   ),  sendMessage $ Go L)
    , ((myModMask,                  xK_Up     ),  sendMessage $ Go U)
    , ((myModMask,                  xK_Down   ),  sendMessage $ Go D)
    , ((myModMask .|. shiftMask,    xK_Right  ),  sendMessage $ Move R)
    , ((myModMask .|. shiftMask,    xK_Left   ),  sendMessage $ Move L)
    , ((myModMask .|. shiftMask,    xK_Up     ),  sendMessage $ Move U)
    , ((myModMask .|. shiftMask,    xK_Down   ),  sendMessage $ Move D)

    -- Keys for toggling struts
    , ((myModMask,                  xK_b      ),  sendMessage ToggleStruts)

    -- Keys for toggling keyboard layout
    , ((myModMask,                  xK_Escape ),  spawn "layout_switch.sh")

    -- Keys for managing urgencies
    , ((myModMask,                xK_BackSpace),  focusUrgent)
    , ((myModMask .|. shiftMask,  xK_BackSpace),  clearUrgents)

    ]
