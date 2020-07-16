import XMonad
import XMonad.Config.Desktop

import XMonad.Actions.DwmPromote
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWS
import XMonad.Actions.ShowText
import XMonad.Actions.RotSlaves
import qualified XMonad.Actions.FlexibleResize as Flex --resize floating from any corner

import Data.Maybe

import XMonad.Hooks.FadeInactive
import XMonad.Hooks.FadeWindows
import XMonad.Hooks.DynamicLog                    -- for xmobar and starusbars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doCenterFloat, doFullFloat)
-- import XMonad.Hooks.SetWMName                       -- for java swing glitch
import XMonad.Hooks.EwmhDesktops                     -- for xserver glitch

import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.NoFrillsDecoration              -- title decoration
import XMonad.Layout.Spacing
import XMonad.Layout.LimitWindows
import XMonad.Layout.Named
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Util.Loggers

import XMonad.Util.EZConfig (additionalKeysP)       -- key bind with normal notation
import XMonad.Util.Run (spawnPipe)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
            where fadeAmount = 0.80

yellow  = "#b58900"
orange  = "#cb4b16"
red     = "#dc322f"
magenta = "#d33682"
violet  = "#6c71c4"
blue    = "#268bd2"
cyan    = "#2aa198"
green   = "#859900"
darkBlue = "#00002F"
brown = "#4D4B49"
black = "#666666"
white = "#bbff11"

myFont      = "xft:Ubuntu Mono Regualr:size=8:bold:antialias=true"
myFont2     = "xft:Ubuntu Mono Regualr:size=28:bold:antialias=true"
----------------------
-- MY CONFIGURATION --
myTerminal           = "termonad"
myModMask            = mod4Mask -- Win key or Super_L
---- colors
myNormalBorderColor  = black
myFocusedBorderColor = white
myTopBarActive       = orange
myTopBarInactive     = brown
---- sizes
myTopBarHeight       = 3
myBorderWidth        = 2 
myGaps               = 4
myTopGap             = 3
---- topbar decoration
--addTopBar =  noFrillsDeco shrinkText topBarTheme
topBarTheme = def
	      { activeColor           = myTopBarActive
              , inactiveColor         = myTopBarInactive
              , activeBorderColor     = myTopBarActive
              , inactiveBorderColor   = myTopBarInactive
              , activeTextColor       = myTopBarActive
              , inactiveTextColor     = myTopBarInactive
	      , decoHeight            = myTopBarHeight
	      }
------------------
-- KEY BINDINGS --
myKeys  =
        [ ("M-d",        spawn "rofi -show drun -show-icons")
        --, ("M-w",        spawn "firefox")
        , ("M-S-z",      spawn "xautolock -locknow")          -- manual screenlock
        -- print screen
        , ("C-<Print>",  spawn "sleep 0.2; scrot -s")         -- copy current window
        , ("<Print>",    spawn "scrot")                       -- copy current screen
        -- volume control
        , ("M-u",        spawn "amixer set Master toggle")    -- mute audio toggle
        , ("M-\\",       spawn "amixer -q sset Master 5%-")   -- volume down
        , ("M-]",        spawn "amixer -q sset Master 5%+")   -- volume up

        --, ("M-e",        spawn "emacsclient -c")
        , ("M-S-t",      sendMessage ToggleStruts)            -- need in .xmobarrc -> overrideRedirect = True

        , ("M-i",        dwmpromote)

       -- , ("M-<Tab>",    cycleRecentWS [theModKey] xK_Tab xK_grave)
        , ("M-w",        flashText flashTextTheme (2/10) "-->" >> moveTo Next NonEmptyWS)
        --, ("M-e",        flashText flashTextTheme (2/10) "<--" >> moveTo Prev NonEmptyWS)
        , ("M-e",        moveTo Prev NonEmptyWS >> logCurrent >>= flashText flashTextTheme (2/10) . fromMaybe "")
        , ("M-<Tab>",    toggleWS' ["NSP"])

        , ("M-o",        rotSlavesUp)
        
        --, ("M-<button1>", \w -> focus w >> Flex.mouseResizeWindow w)
        
        -- layouts
        , ("M-f",        sendMessage (Toggle NBFULL))
        ]
-- automatically detects the configured Mod key, some actions require it as argument
theModKey = if myModMask == mod4Mask  -- if mod key is Super
               then xK_Super_L        -- use Super
               else xK_Alt_L          -- else use Left Alt (Xmonad default)

flashTextTheme = def
                 { st_font = myFont2
                 , st_bg = darkBlue
                 , st_fg = "#ffffff"
                 }
-------------------------
-- STARTUP APLICATIONS --
myStartupHook = do
        spawn "xsetroot -cursor_name left_ptr"                                -- sets and makes visible the mouse cursor from the start
        spawn "xrandr --output Virtual1 --mode 1360x768 --dpi 96"             -- set screen resolution and dpi
        spawn "nitrogen --restore"                                            -- wallpaper
        spawn "trayer --edge top --align right --width 4 --padding 0 --expand true --SetDockType true --SetPartialStrut false --transparent true --alpha 0 --tint 0x000000 --height 15 --distancefrom right --margin 0"
        spawn "picom"                                                      -- compositor
        spawn "clipit"
        spawn "nm-applet"
        spawn "volumeicon"
----------------------
-- FLOATING WINDOWS --
myManageHook = composeAll
               [ className =? "Gimp"          --> doFloat
               , className =? "Nemo"          --> doFloat
               , className =? "Nitrogen"      --> doCenterFloat
               --, isFullscreen --> doFullFloat
               ]
-------------
-- LAYOUTS --
myLayouts = mkToggle (single NBFULL) $ avoidStruts $ smartBorders
            (tiled ||| monocle)
            where tiled   = named "Tiled"   ( mySpacing $ limitWindows 5 $ Tall 1 (3/100) (1/2) )
                  monocle = named "Monocle" ( mySpacing $ Full )
---- gaps
mySpacing = spacingRaw False (Border g g g g) True (Border g g g g) True
            where g  = myGaps
                  tg = myTopGap
------------------------
-- MAIN CONFIGURATION --
main = do

    xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc"  -- run xmobar with .xmobarrc

    xmonad $ ewmh desktopConfig
        { terminal           = myTerminal
        , modMask            = myModMask
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , startupHook        = myStartupHook

        , manageHook  = manageDocks 
			<+> myManageHook		-- include myManageHook
			<+> manageHook defaultConfig

        , layoutHook  = myLayouts -- avoidStruts 

        --, handleEventHook = handleEventHook def <+> fadeWindowsEventHook <+> fullscreenEventHook <+> docksEventHook <+> ewmhDesktopsEventHook
        , handleEventHook = handleEventHook def
                            <+> fullscreenEventHook
                            <+> docksEventHook
                            <+> ewmhDesktopsEventHook
                            <+> handleTimerEvent

        --, logHook     = myLogHook <+> fadeWindowsLogHook myFadeHook
        , logHook     = myLogHook
                        <+> ewmhDesktopsLogHook		
			<+> dynamicLogWithPP xmobarPP
	                                     { ppOutput     = hPutStrLn xmproc
	                                     , ppTitle      = xmobarColor "green" "" . shorten 50
	                                     } 
        }

        `additionalKeysP` myKeys
