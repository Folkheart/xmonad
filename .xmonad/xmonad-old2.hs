import XMonad
import XMonad.Config.Desktop

import XMonad.Actions.DwmPromote
import XMonad.Actions.CycleWS
import XMonad.Actions.RotSlaves

import XMonad.Hooks.DynamicLog                                 -- for xmobar and starusbars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (doCenterFloat)
-- import XMonad.Hooks.SetWMName                               -- for java swing glitch
import XMonad.Hooks.EwmhDesktops                               -- for xserver glitch

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Spacing
import XMonad.Layout.LimitWindows
import XMonad.Layout.Named
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
-- import XMonad.Util.Loggers

import XMonad.Util.EZConfig (additionalKeysP)                  -- key bind with normal notation
import XMonad.Util.Run (spawnPipe)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import System.IO

yellow    = "#b58900"
orange    = "#cb4b16"
red       = "#dc322f"
magenta   = "#d33682"
violet    = "#6c71c4"
blue      = "#268bd2"
cyan      = "#2aa198"
green     = "#859900"
darkBlue  = "#00002F"
brown     = "#4D4B49"
black     = "#666666"
white     = "#ff0000"
----------------------
-- MY CONFIGURATION --
myTerminal           = "termonad"
myModMask            = mod4Mask                               -- Win key or Super_L
---- colors
myNormalBorderColor  = black
myFocusedBorderColor = white
---- sizes
myBorderWidth        = 2
myGaps               = 3
------------------
-- KEY BINDINGS --
myKeys  =
        [ ("M-d",        spawn "rofi -show drun -show-icons")
        , ("M-S-z",      spawn "xautolock -locknow")          -- manual screenlock
        -- print screen
        , ("C-<Print>",  spawn "sleep 0.2; scrot -s")         -- copy current window
        , ("<Print>",    spawn "scrot")                       -- copy current screen
        -- volume control
        , ("M-u",        spawn "amixer set Master toggle")    -- mute audio toggle
        , ("M-\\",       spawn "amixer -q sset Master 5%-")   -- volume down
        , ("M-]",        spawn "amixer -q sset Master 5%+")   -- volume up

        , ("M-S-t",      sendMessage ToggleStruts)            -- need in .xmobarrc -> overrideRedirect = True

        , ("M-i",        dwmpromote)

        , ("M-w",        moveTo Next NonEmptyWS)
        , ("M-e",        moveTo Prev NonEmptyWS)
        , ("M-<Tab>",    toggleWS' ["NSP"])
        , ("M-o",        rotSlavesUp)
        -- layouts
        , ("M-f",        sendMessage (Toggle NBFULL))
        ]
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
------------------------
-- MAIN CONFIGURATION --
main = do

    xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc"  -- run xmobar with .xmobarrc

    xmonad $ def-- ewmh desktopConfig
        { terminal           = myTerminal
        , modMask            = myModMask
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , startupHook        = myStartupHook

        , manageHook  = manageDocks 
                        <+> myManageHook                -- include myManageHook
                        <+> manageHook def

        , layoutHook  = myLayouts -- avoidStruts 

        , handleEventHook = handleEventHook def
                            <+> fullscreenEventHook
                            <+> docksEventHook
                            -- <+> ewmhDesktopsEventHook

        , logHook     = --ewmhDesktopsLogHook
                        dynamicLogWithPP xmobarPP
                                             { ppOutput     = hPutStrLn xmproc
                                             , ppTitle      = xmobarColor "green" "" . shorten 50
                                             }
        }

        `additionalKeysP` myKeys
