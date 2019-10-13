import XMonad

-- import XMonad.Config.Desktop

import XMonad.Hooks.DynamicLog                    -- for xmobar and starusbars
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers (isFullscreen, doCenterFloat, doFullFloat)
-- import XMonad.Hooks.SetWMName                       -- for java swing glitch
-- import XMonad.Hooks.EwmhDesktops                     -- for xserver glitch

import XMonad.Layout.NoBorders (noBorders, smartBorders)
import XMonad.Layout.Tabbed

import XMonad.Util.EZConfig (additionalKeysP)       -- key bind with normal notation
import XMonad.Util.Run (spawnPipe)
-- import XMonad.Util.SpawnOnce                      -- startup applications

-- import qualified XMonad.StackSet as W
-- import qualified Data.Map        as M

import System.IO

------------------------
-- main configuration --
------------------------ 
main = do

    xmproc <- spawnPipe "xmobar ~/.xmonad/.xmobarrc"  -- run xmobar with .xmobarrc

    -- xmonad $ desktopConfig
    xmonad $ defaultConfig
        { terminal           = myTerminal
        , modMask            = myModMask
        , borderWidth        = myBorderWidth
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , startupHook        = myStartupHook

        , manageHook  = manageDocks <+> myManageHook <+> manageHook defaultConfig

        , layoutHook  = smartBorders $ avoidStruts $ layoutHook defaultConfig
                        -- simpleTabbed
                        -- ||| layoutHook defaultConfig

        , handleEventHook = handleEventHook defaultConfig <+> docksEventHook

        , logHook     = dynamicLogWithPP $ xmobarPP
                        { ppOutput     = hPutStrLn xmproc
                        , ppTitle      = xmobarColor "green" "" . shorten 50
                        }
        }

        `additionalKeysP` myKeys

----------------------
-- my configuration --
----------------------
myTerminal           = "st"
myModMask            = mod4Mask -- Win key or Super_L
myBorderWidth        = 1 
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#ddffoo"
myFloatBorderColor   = "#000000"

-------------------------
-- startup aplications --
-------------------------
myStartupHook = do
        spawn "xrandr -s 1360x768"                                            -- resolution
        spawn "xrandr --dpi 117"                                              -- set screen dpi
        -- spawn "xrandr --output Virtual1 --scale 1.25x1.25"                     -- change original dpi scale
        spawn "xsetroot -cursor_name left_ptr"                                -- sets and makes visible the mouse cursor from the start
        spawn "nitrogen --restore"                                            -- wallpaper
        spawn "compton --config ~/.config/compton.conf"                       -- compositor
        -- spawnOnce "emacs --daemon"
        spawn "xautolock -time 10 -locker '~/.xmonad/screenlock.sh'"          -- screenlock timer
        spawn "setxkbmap -option ctrl:nocaps && xcape -e 'Control_L=Escape'"  -- Caps Lock as Ctrl and Esc
        -- setWMName "LG3D" -- for java swing glitch
------------------
-- key bindings --
------------------
myKeys  =
        [ ("M-d",        spawn "rofi -show drun -show-icons")
        , ("M-w",        spawn "firefox")
        , ("M-S-z",      spawn "xautolock -locknow")          -- manual screenlock
        -- print screen
        , ("C-<Print>",  spawn "sleep 0.2; scrot -s")         -- copy current window
        , ("<Print>",    spawn "scrot")                       -- copy current screen
        -- volume control
        , ("M-u",        spawn "amixer set Master toggle")    -- mute audio toggle
        , ("M-i",        spawn "amixer -q sset Master 5%-")   -- volume down
        , ("M-o",        spawn "amixer -q sset Master 5%+")   -- volume up

        , ("M-e",        spawn "emacsclient -c")
        , ("M-S-t",      sendMessage ToggleStruts)            -- need in .xmobarrc -> overrideRedirect = True
        -- , ((mod4Mask,               xK_t),     spawn "emacsclient -c -a emacs -q --eval 'emacs-terminal'")
        ]

----------------------
-- floating windows --
----------------------
myManageHook = composeAll
               [ className =? "Gimp"          --> doFloat
               , className =? "Nemo"          --> doFloat
               , className =? "Nitrogen"      --> doCenterFloat
               ]
