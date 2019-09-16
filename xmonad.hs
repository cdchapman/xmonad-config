{-# LANGUAGE PostfixOperators #-}

--
-- My xmonad config file.
--

-- Imports
import           Data.Ratio ((%))
import           System.IO

import           Solarized as Sol

import           XMonad
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.SetWMName
import           XMonad.Hooks.UrgencyHook
import           XMonad.Layout.ComboP
import           XMonad.Layout.DragPane
import           XMonad.Layout.Gaps
import           XMonad.Layout.Grid
import           XMonad.Layout.IM
import           XMonad.Layout.Named
import           XMonad.Layout.NoBorders
import           XMonad.Layout.OneBig
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.TwoPane
import           XMonad.Layout.WindowNavigation
import           XMonad.Layout.WorkspaceDir
import           XMonad.Prompt
import           XMonad.Prompt.Ssh
import           XMonad.Prompt.Pass
import qualified XMonad.StackSet as W
import           XMonad.Util.EZConfig(additionalKeys)
import           XMonad.Util.Run(spawnPipe, hPutStrLn)

main = do
  xmproc <- spawnPipe "/usr/bin/xmobar $HOME/.xmonad/xmobarrc"
  xmonad $ docks $ withUrgencyHook NoUrgencyHook $ def
    { borderWidth = 2
    , normalBorderColor = solarizedBase03
    , focusedBorderColor = solarizedYellow
    , modMask = myModMask
    , terminal = myTerminal
    , focusFollowsMouse = myFocusFollowsMouse
    , workspaces = myWorkspaces
    , handleEventHook = fullscreenEventHook
    , manageHook = manageDocks <+> myManageHook <+> manageHook def
    , layoutHook = avoidStruts $ smartBorders $ myLayout
    , logHook = myLogHook xmproc
    , startupHook = myStartupHook
    }
    `additionalKeys` myAdditionalKeys

-- Use windows key for mod
myModMask = mod4Mask

-- Terminal emulator
myTerminal = "kitty --single-instance"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Convinces JAVA Swing applications that I am using Project Looking Glass
myStartupHook = setWMName "LG3D"

-- my workspaces
ws_gtd            = "α" -- Alpha
ws_web            = "β" -- Beta
ws_dev            = "γ" -- Gamma
ws_webdev         = "δ" -- Delta
ws_communications = "ε" -- Epsilon
ws_network        = "ζ" -- Zeta
ws_top            = "η" -- Eta
ws_raster         = "θ" -- Theta
ws_vector         = "ι" -- Iota
ws_kappa          = "κ" -- Kappa
ws_lambda         = "λ" -- Lambda
ws_mu             = "μ" -- Mu
ws_nu             = "ν" -- Nu
ws_xi             = "ξ" -- Xi
ws_omicron        = "ο" -- Omicron
ws_pi             = "π" -- Pi
ws_rho            = "ρ" -- Rho
ws_sigma          = "σ" -- Sigma
ws_tau            = "τ" -- Tau
ws_upsilon        = "υ" -- Upsilon
ws_phi            = "φ" -- Phi
ws_chi            = "χ" -- Chi
ws_psi            = "ψ" -- Psi
ws_pentesting     = "ω" -- Omega

myWorkspaces = [ ws_gtd
               , ws_web
               , ws_dev
               , ws_webdev
               , ws_communications
               , ws_network
               , ws_top
               , ws_raster
               , ws_vector
               , ws_pentesting
               ]

-- special application rules
myManageHook = composeAll
    -- shift applications to specific workspaces
    [ title     =? "mutt"                                  --> doShift ws_communications
    , title     =? "wyrd"                                  --> doShift ws_gtd
    , title     =? "task"                                  --> doShift ws_gtd
    , title     =? "htop"                                  --> doShift ws_top
    , className =? "Gimp"                                  --> doShift ws_raster
    , className =? "inkscape"                              --> doShift ws_vector
    , className =? "Chromium"                              --> doShift ws_web
    , className =? "firefox"                               --> doShift ws_web
    , className =? "qutebrowser"                           --> doShift ws_web

    -- float application windows
    , className =? "VirtualBox"                            --> doFloat
    , className =? "Xmessage"                              --> doFloat
    , className =? "XCalc"                                 --> doFloat
    , className =? "Cinelerra"                             --> doFloat
    , className =? "CinePaint"                             --> doFloat
    , className =? "Synfigstudio"                          --> doFloat
    , className =? "fontforge"                             --> doFloat
    , className =? "xsane"                                 --> doFloat
    , className =? "lbe-ui-BrowserApp"                     --> doFloat
    , className =? "Cssh"                                  --> doFloat
    , className =? "Vncviewer"                             --> doFloat
    , className =? "Ssvnc"                                 --> doFloat
    , className =? "weka-gui-GUIChooser"                   --> doFloat
    , className =? "firefox" <&&> resource =? "Dialog"     --> doFloat
    , className =? "Thunderbird" <&&> role =? "Msgcompose" --> doFloat
    , className =? "R_x11"                                 --> doFloat

    -- apply application window to a function
    , className =? "Pidgin"                                --> doF (W.shift ws_top)
    , className =? "Pidgin" <&&> role =? "conversation"    --> doF (W.swapDown)
--  , className =? "zoom"                                  --> doF (W.shift ws_top)

    -- ignore applications
    , className =? "Xmessage"                              --> doIgnore
    ]

    where
      role = stringProperty "WM_WINDOW_ROLE"

-- layout
myLayout = onWorkspaces [ ws_dev, ws_webdev ] dev $ onWorkspace ws_top imLayout $ standardLayouts
  where
    tiled = named "Default" (ResizableTall 1 (1/100) (1/2) [])
    dev = named "Dev" (workspaceDir "~/devel" (OneBig (11/16) (11/16)))

    -- define the list of standardLayouts
    standardLayouts = tiled ||| Full ||| dev ||| Grid

    -- define a layout for IM
    imLayout = withIM (1%6) (pidginRoster) standardLayouts
    -- imLayout = combineTwoP (TwoPane (3/100) (1/2))
    --           (withIM (1%4) (pidginRoster) standardLayouts)
    --           (withIM (1%4) (zoomRoster) standardLayouts)
    --           (ClassName "Pidgin")

    pidginRoster = (ClassName "Pidgin") `And` (Role "buddy_list")
    zoomRoster = (ClassName "zoom") `And` (Title "Zoom - Pro Account")

myXPConfig = defaultXPConfig
    { position = Bottom
    , bgColor = "#000000"
    , fgColor = "#FFFFFF"
--    , bgHLight = Sol.base03
--    , fgHLight = Sol.yellow
    , borderColor = "#000000"
    , height = 12
    , font = "xft:Hasklig:pixelsize=10:antialias=true:hinting=true"
    }

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ def
    { ppCurrent = xmobarColor solarizedBase3 "" . wrap "[" "]"
    , ppTitle   = xmobarColor solarizedBase2 "" . shorten 100
    , ppUrgent  = xmobarColor solarizedRed solarizedBase3 . wrap ">" "<"
    , ppOutput  = hPutStrLn h
    , ppSep     = " | "
    }

myAdditionalKeys =
    -- Keys for locking the screen
    [ ((myModMask .|. shiftMask,    xK_z      ) ,  spawn "xscreensaver-command -lock")

    -- Keys for screen shots
    , ((myModMask,                  xK_Print  ) ,  spawn "scrot -b -s")
    , ((0,                          xK_Print  ) ,  spawn "scrot")

    -- Keys for Prompts
    , ((myModMask .|. controlMask,  xK_s      ) ,  sshPrompt def)
    , ((myModMask .|. shiftMask,    xK_x      ) ,  changeDir def)

    -- Keys for ResizableTall layout
    , ((myModMask,                  xK_a      ) ,  sendMessage MirrorShrink)
    , ((myModMask,                  xK_z      ) ,  sendMessage MirrorExpand)

    -- Keys for WindowNavigation
    , ((myModMask,                  xK_Right  ) ,  sendMessage $ Go R)
    , ((myModMask,                  xK_Left   ) ,  sendMessage $ Go L)
    , ((myModMask,                  xK_Up     ) ,  sendMessage $ Go U)
    , ((myModMask,                  xK_Down   ) ,  sendMessage $ Go D)
    , ((myModMask .|. shiftMask,    xK_Right  ) ,  sendMessage $ Move R)
    , ((myModMask .|. shiftMask,    xK_Left   ) ,  sendMessage $ Move L)
    , ((myModMask .|. shiftMask,    xK_Up     ) ,  sendMessage $ Move U)
    , ((myModMask .|. shiftMask,    xK_Down   ) ,  sendMessage $ Move D)

    -- Keys for toggling struts
    , ((myModMask,                  xK_b      ) ,  sendMessage ToggleStruts)

    -- Keys for toggling keyboard layout
    , ((myModMask,                  xK_Escape ) ,  spawn "$HOME/.local/bin/layout_switch.sh")

    -- Keys for managing urgencies
    , ((myModMask,                  xK_BackSpace) ,  focusUrgent)
    , ((myModMask .|. shiftMask,    xK_BackSpace) ,  clearUrgents)

    -- Keys for managing passwords
    , ((myModMask,                  xK_grave              ) , passPrompt myXPConfig)
    , ((myModMask .|. controlMask,  xK_grave              ) , passGeneratePrompt myXPConfig)
    , ((myModMask .|. controlMask  .|. shiftMask, xK_grave) , passRemovePrompt myXPConfig)

    ]
