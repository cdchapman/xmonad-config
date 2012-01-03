--
-- xmonad config file.
--


import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ICCCMFocus
import XMonad.Layout.NoBorders
import XMonad.Layout.Gaps
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import System.IO

-- Use windows key for mod
myModMask = mod4Mask

-- Terminal emulator
myTerminal = "urxvtc"

-- Fixes JAVA Swing applications with taking and losing focus.
myLogHook = takeTopFocus

-- Fixes JAVA Swing applications even more
myStartupHook = do
    setWMName "LG3D"
    spawn "urxvtc -e mutt"
    spawn "urxvtc -e htop"
    spawn "firefox"

-- my workspaces
ws_network        = "1:net"
ws_web            = "2:web"
ws_dev            = "3:dev"
ws_communications = "4:com"

myWorkspaces = [ ws_network, ws_web, ws_dev, ws_communications ] ++ map show [5..9]

-- special application rules
myManageHook = composeAll
    [ title     =? "mutt"               --> doShift ws_communications
    , className =? "Chromium"           --> doShift ws_web
    , className =? "Firefox"            --> doShift ws_web
    , className =? "jetbrains-idea-ce"  --> doShift ws_dev
    ]

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar /home/chris/.xmonad/xmobarrc"
  xmonad $ defaultConfig 
    { borderWidth = 2
    , modMask = myModMask
    , terminal = myTerminal
    , workspaces = myWorkspaces
    , manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
    , layoutHook = avoidStruts $ layoutHook defaultConfig
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
    , ((controlMask,              xK_Print),  spawn "sleep 0.2; scrot -s")
    , ((0,                        xK_Print),  spawn "scrot")
    ]
