--
-- xmonad config file.
--

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.SetWMName
import XMonad.Layout.Accordion
import XMonad.Layout.Combo
import XMonad.Layout.DragPane
import XMonad.Layout.Gaps
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.ResizableTile
import XMonad.Layout.StackTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns
import XMonad.Layout.WindowNavigation
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- Use windows key for mod
myModMask = mod4Mask

-- Terminal emulator
myTerminal = "urxvtc"

-- Fixes JAVA Swing applications with taking and losing focus.
myLogHook = takeTopFocus

-- Convinces JAVA Swing applications that I am using Project Looking Glass
myStartupHook = setWMName "LG3D"

-- my workspaces
ws_network        = "α"
ws_web            = "β"
ws_dev            = "γ"
ws_webdev         = "δ"
ws_communications = "ε"
ws_gtd            = "ζ"
ws_top            = "η"

ws_pentesting     = "ω"

myWorkspaces = [ ws_network, ws_web, ws_dev, ws_webdev, ws_communications, ws_gtd, ws_top ] 
               ++ 
               ["θ", "ι"] 
               ++ 
               [ ws_pentesting ]

-- special application rules
myManageHook = composeAll
    [ title     =? "mutt"               --> doShift ws_communications
    , title     =? "wyrd"               --> doShift ws_gtd
    , title     =? "task"               --> doShift ws_gtd
    , title     =? "htop"               --> doShift ws_top
    , className =? "Chromium"           --> doShift ws_web
    , className =? "Firefox"            --> doShift ws_web
    , className =? "jetbrains-idea-ce"  --> doShift ws_dev
    , className =? "VirtualBox"         --> doFloat
    , className =? "Xmessage"           --> doFloat
    , className =? "XCalc"              --> doFloat
    , className =? "draftsight.bin"     --> doFloat
    ]

-- layout
myLayout = tiled ||| noBorders Full ||| wmii ||| three
  where
    tiled = named "Default" (ResizableTall 1 (1/100) (1/2) [])
    wmii = windowNavigation (named "Wmii" (combineTwo (dragPane Vertical 0.01 0.5) (Accordion) (Accordion)))
    three = named "Buff" (ThreeColMid 1 (3/100) (1/2))

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/chris/.xmonad/xmobarrc"
  xmonad $ defaultConfig 
    { borderWidth = 2
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ myLayout
    , logHook = do
            dynamicLogWithPP $ xmobarPP
                { ppOutput = hPutStrLn xmproc
                , ppTitle = xmobarColor "green" "" . shorten 50
                } 
            myLogHook
    , startupHook = myStartupHook
    }
    `additionalKeys`
    [ ((myModMask .|. shiftMask,  xK_z),      spawn "xscreensaver-command -lock")
    , ((myModMask,                xK_Print),  spawn "scrot -b -s")
    , ((0,                        xK_Print),  spawn "scrot")

    -- Keys for ResizableTall layout
    , ((myModMask,                xK_a),      sendMessage MirrorShrink)
    , ((myModMask,                xK_z),      sendMessage MirrorExpand)

    -- Keys for WindowNavigation
    , ((myModMask,                xK_Right),  sendMessage $ Go R)
    , ((myModMask,                xK_Left ),  sendMessage $ Go L)
    , ((myModMask,                xK_Up   ),  sendMessage $ Go U)
    , ((myModMask,                xK_Down ),  sendMessage $ Go D)
    , ((myModMask .|. shiftMask,  xK_Right),  sendMessage $ Move R)
    , ((myModMask .|. shiftMask,  xK_Left ),  sendMessage $ Move L)
    , ((myModMask .|. shiftMask,  xK_Up   ),  sendMessage $ Move U)
    , ((myModMask .|. shiftMask,  xK_Down ),  sendMessage $ Move D)

    -- Keys for toggling struts
    , ((myModMask,                xK_b    ),  sendMessage ToggleStruts)

    ]
