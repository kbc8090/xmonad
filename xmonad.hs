import System.IO (Handle, hPutStrLn)
import System.Exit
import XMonad
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
--import XMonad.Hooks.Minimize
import XMonad.Hooks.ManageHelpers(doFullFloat, doCenterFloat, isFullscreen, isDialog)
import XMonad.Config.Desktop
--import XMonad.Config.Azerty
import XMonad.Util.Run(spawnPipe)
import XMonad.Actions.SpawnOn
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.MouseResize
import qualified Codec.Binary.UTF8.String as UTF8
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO


import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
--import XMonad.Layout.Magnifier
--import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
--import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
-- import XMonad.Layout.LayoutCombinators hiding ( (|||) )
--import XMonad hiding ( (|||) )  -- don't use the normal ||| operator
--import XMonad.Layout.LayoutCombinators   -- use the one from LayoutCombinators instead
import XMonad.Layout.Spacing
--import XMonad.Layout.Gaps
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
--import XMonad.Layout.Fullscreen (fullscreenFull)
--import XMonad.Layout.Cross(simpleCross)
--import XMonad.Layout.Spiral(spiral)
--import XMonad.Layout.Grid
import XMonad.Layout.ShowWName
--import XMonad.Layout.Simplest
import XMonad.Layout.Renamed
--import XMonad.Layout.SimplestFloat
--import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
--import XMonad.Layout.IndependentScreens
--import XMonad.Layout.Minimize
--import XMonad.Layout.CenteredMaster(centerMaster)
--import XMonad.Layout.SubLayouts
--import XMonad.Layout.WindowNavigation
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

import Graphics.X11.ExtraTypes.XF86
import qualified System.IO
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.ByteString as B

import Data.Maybe (maybeToList)
import Graphics.X11.ExtraTypes.XF86

import Control.Monad ((>=>), join, liftM, when)

--mod4Mask= super key
--mod1Mask= alt key
--controlMask= ctrl key
--shiftMask= shift key

myModMask                     = mod4Mask
mydefaults = def {
          normalBorderColor   = "#383c45"
        , focusedBorderColor  = "#6cbd93"
        , focusFollowsMouse   = True
        , mouseBindings       = myMouseBindings
        , workspaces          = myWorkspaces
        , keys                = myKeys
        , modMask             = myModMask
        , borderWidth         = 2
        , layoutHook          = myLayoutHook
        , startupHook         = myStartupHook >> addEWMHFullscreen
        , manageHook          = myManageHook
        , handleEventHook     = docksEventHook <+> fullscreenEventHook
}

-- Autostart
myStartupHook = do
    spawn "$HOME/.xmonad/scripts/autostart.sh"
    setWMName "LG3D"

encodeCChar = map fromIntegral . B.unpack

myTitleColor = "#c91a1a" -- color of window title
myTitleLength = 80 -- truncate window title to this length
myCurrentWSColor = "#82aaff" -- color of active workspace
myVisibleWSColor = "#000000" -- color of inactive workspace
myUrgentWSColor = "#c91a1a" -- color of workspace with 'urgent' window
myHiddenNoWindowsWSColor = "white"

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

addNETSupported :: Atom -> X ()
addNETSupported x   = withDisplay $ \dpy -> do
    r               <- asks theRoot
    a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
    a               <- getAtom "ATOM"
    liftIO $ do
       sup <- (join . maybeToList) <$> getWindowProperty32 dpy a_NET_SUPPORTED r
       when (fromIntegral x `notElem` sup) $
         changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen   = do
	 wms <- getAtom "_NET_WM_STATE"
	 wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
	 mapM_ addNETSupported [wms, wfs]

--myLayoutHook = spacing 6 $ gaps [(U,26), (D,4), (R,4), (L,4)]
--myLayoutHook = spacingRaw False (Border 8 8 8 8) True (Border 8 8 8 8) True
--myLayoutHook = spacingRaw False (Border 0 0 0 0) True (Border 4 4 4 4) True $ gaps [(U,27), (D,4), (R,4), (L,4)]
--               $ avoidStruts
--               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT)
--					-- $ smartBorders
--              $ tiled ||| Grid ||| spiral (7/8) ||| ThreeColMid 1 (3/100) (1/2) ||| Full
--                    where
--                    tiled   = Tall nmaster delta ratio
--                    nmaster = 1
--                    delta   = 3/100
--                    ratio   = 1/2

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
mySpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tiled     = renamed [Replace "tiled"]
           -- $ windowNavigation
--           $ addTabs shrinkText myTabTheme
--           $ subLayout [] (Simplest)
           $ limitWindows 12
           $ mySpacing 3
           $ ResizableTall 1 (3/100) (1/2) []
-- magnify  = renamed [Replace "magnify"]
--           $ windowNavigation
--           $ addTabs shrinkText myTabTheme
--           $ subLayout [] (smartBorders Simplest)
--           $ magnifier
--           $ limitWindows 12
--           $ mySpacing 8
--           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           -- $ windowNavigation
--			  $ subLayout [] (smartBorders Simplest)
           $ limitWindows 20 Full
--tabs     = renamed [Replace "tabs"]
           -- I cannot add spacing to this layout because it will
           -- add spacing between window and tabs which looks bad.
--			  $ subLayout [] (noBorders Simplest)
--           $ tabbed shrinkText myTabTheme

myTabTheme = def { fontName            = "xft:JetBrainsMono Nerd Font:bold:size=10:antialias=true:hinting=true"
                 , activeColor         = "#82aaff"
                 , inactiveColor       = "#313846"
                 , activeBorderColor   = "#1b1e2b"
                 , inactiveBorderColor = "#1b1e2b"
                 , activeTextColor     = "#1b1e2b"
                 , inactiveTextColor   = "#d0d0d0"
                 }

-- The layout hook
myLayoutHook = avoidStruts $ mouseResize $ windowArrange
               $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
             where
               -- I've commented out the layouts I don't use.
               myDefaultLayout =     tiled
                                -- ||| magnify
                                 ||| smartBorders monocle
                                 -- ||| floats
                                 -- ||| smartBorders tabs
                                 -- ||| grid
                                 -- ||| spirals
                                 -- ||| threeCol
                                 -- ||| threeRow



--WORKSPACES
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]
        
myWorkspaces :: [String]   
myWorkspaces = clickable . (map xmobarEscape) 
               $ [" dev ", " www ", " sys ", " doc ", " irc ", " music ", " vid "]
  where                                                                      
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ ">" ++ ws ++ "</action>" |
                      (i,ws) <- zip [1..7] l,                                        
                      let n = i ] 

-- window manipulations
myManageHook = composeAll . concat $
    [ [isDialog --> doCenterFloat]
    , [className =? c --> doCenterFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [resource =? i --> doIgnore | i <- myIgnores]
--    , [className =? c --> doShift (myWorkspaces !! 0) <+> viewShift (myWorkspaces !! 0)        | c <- my1Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 1) <+> viewShift (myWorkspaces !! 1)        | c <- my2Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 2) <+> viewShift (myWorkspaces !! 2)        | c <- my3Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 3) <+> viewShift (myWorkspaces !! 3)        | c <- my4Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 4) <+> viewShift (myWorkspaces !! 4)        | c <- my5Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 5) <+> viewShift (myWorkspaces !! 5)        | c <- my6Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 6) <+> viewShift (myWorkspaces !! 6)        | c <- my7Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 7) <+> viewShift (myWorkspaces !! 7)        | c <- my8Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 8) <+> viewShift (myWorkspaces !! 8)        | c <- my9Shifts]
--    , [className =? c --> doShift (myWorkspaces !! 9) <+> viewShift (myWorkspaces !! 9)        | c <- my10Shifts]
       ]
    where
--    viewShift    = doF . liftM2 (.) W.greedyView W.shift
    myCFloats = ["Arandr", "Arcolinux-tweak-tool.py", "Arcolinux-welcome-app.py", "Galculator", "Oblogout", "feh", "mpv", "vlc", "lxappearance", "Nitrogen", "nomacs", "Viewnior", "Xfce4-appfinder", "Audacious", "Deadbeef", "Io.github.celluloid_player.Celluloid", "mpv", "BleachBit", "Sxiv"]
    myTFloats = ["Downloads", "Save As...", "Customize Look and Feel"]
    myRFloats = []
    myIgnores = ["desktop_window", "panel"]
    -- my1Shifts = ["Chromium", "Vivaldi-stable", "Firefox"]
    -- my2Shifts = []
    -- my3Shifts = ["Inkscape"]
    -- my4Shifts = []
    -- my5Shifts = ["Gimp", "feh"]
    -- my6Shifts = ["vlc", "mpv"]
    -- my7Shifts = ["Virtualbox"]
    -- my8Shifts = ["Thunar"]
    -- my9Shifts = []
    -- my10Shifts = ["discord"]

-- keys config

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- SUPER + FUNCTION KEYS

  [ ((modMask, xK_e), spawn $ "atom" )
  , ((modMask, xK_d ), spawn $ "dmenu_run -i -fn 'Ubuntu Mono:style=Bold:size=11' -h '22' -nb '#1b1e2b' -nf 'white' -sb '#82aaff' -sf '#1b1e2b'")
  , ((modMask, xK_c), spawn $ "conky-toggle" )
  , ((modMask, xK_f), sendMessage FirstLayout)
--  , ((modMask .|. controlMask, xK_space), sendMessage (T.Toggle "monocle"))
--  , ((modMask, xK_f), sendMessage $ JumpToLayout "monocle")
  , ((modMask, xK_semicolon), sendMessage Expand )
  , ((modMask, xK_h), sendMessage Shrink )
--  , ((modMask, xK_m), (MT.Toggle NBFULL) >> sendMessage ToggleStruts) 
  , ((modMask, xK_q), kill )
  , ((modMask, xK_r), spawn $ "rofi-theme-selector" )
  , ((modMask, xK_v), spawn $ "pavucontrol" )
  , ((modMask, xK_w), spawn $ "vivaldi-stable" )
  , ((modMask, xK_y), spawn $ "polybar-msg cmd toggle" )
  , ((modMask, xK_x), spawn $ "oblogout" )
  , ((modMask, xK_Escape), spawn $ "xkill" )
  , ((modMask, xK_Return), spawn $ "urxvt -e fish" )
  , ((modMask, xK_F1), spawn $ "chromium --force-dark-mode" )
  , ((modMask, xK_F2), spawn $ "firefox" )
  , ((modMask, xK_F3), spawn $ "inkscape" )
  , ((modMask, xK_F4), spawn $ "gimp" )
  , ((modMask, xK_F6), spawn $ "vlc --video-on-top" )
  , ((modMask, xK_F7), spawn $ "virtualbox" )
  , ((modMask, xK_F8), spawn $ "thunar" )
  , ((modMask, xK_F9), spawn $ "evolution" )
  , ((modMask, xK_F10), spawn $ "spotify" )
  , ((modMask, xK_F11), spawn $ "rofi -show run -fullscreen" )
  , ((modMask, xK_F12), spawn $ "rofi -show run" )
  , ((modMask, xK_i), decWindowSpacing 4)
  , ((modMask, xK_o), incWindowSpacing 4)
  , ((modMask .|. shiftMask , xK_i), decScreenSpacing 4)
  , ((modMask .|. shiftMask , xK_o), incScreenSpacing 4)

  -- FUNCTION KEYS
--  , ((0, xK_F5), spawn $ "xfce4-appfinder" )

  -- SUPER + SHIFT KEYS

  , ((modMask .|. shiftMask , xK_Return ), spawn $ "thunar")
  , ((modMask .|. shiftMask , xK_r ), spawn $ "xmonad --recompile && sleep 3 && xmonad --restart")
  , ((modMask .|. shiftMask , xK_q ), kill)
  , ((modMask .|. shiftMask , xK_x ), io (exitWith ExitSuccess))

  -- CONTROL + ALT KEYS

  , ((controlMask .|. mod1Mask , xK_Next ), spawn $ "conky-rotate -n")
  , ((controlMask .|. mod1Mask , xK_Prior ), spawn $ "conky-rotate -p")
  , ((controlMask .|. mod1Mask , xK_a ), spawn $ "xfce4-appfinder")
  , ((controlMask .|. mod1Mask , xK_b ), spawn $ "thunar")
  , ((controlMask .|. mod1Mask , xK_c ), spawn $ "catfish")
  , ((controlMask .|. mod1Mask , xK_e ), spawn $ "arcolinux-tweak-tool")
  , ((controlMask .|. mod1Mask , xK_f ), spawn $ "firefox")
  , ((controlMask .|. mod1Mask , xK_g ), spawn $ "chromium -no-default-browser-check")
  , ((controlMask .|. mod1Mask , xK_i ), spawn $ "nitrogen")
  , ((controlMask .|. mod1Mask , xK_m ), spawn $ "xfce4-settings-manager")
  , ((controlMask .|. mod1Mask , xK_o ), spawn $ "$HOME/.xmonad/scripts/compton-toggle.sh")
  , ((controlMask .|. mod1Mask , xK_p ), spawn $ "pamac-manager")
  , ((controlMask .|. mod1Mask , xK_r ), spawn $ "rofi-theme-selector")
  , ((controlMask .|. mod1Mask , xK_s ), spawn $ "spotify")
  , ((controlMask .|. mod1Mask , xK_t ), spawn $ "urxvt")
  , ((controlMask .|. mod1Mask , xK_u ), spawn $ "pavucontrol")
  , ((controlMask .|. mod1Mask , xK_v ), spawn $ "vivaldi-stable")
  , ((controlMask .|. mod1Mask , xK_Return ), spawn $ "urxvt")

  -- ALT + ... KEYS

  , ((mod1Mask, xK_r), spawn $ "xmonad --restart" )

  --VARIETY KEYS WITH PYWAL

  , ((mod1Mask .|. shiftMask , xK_f ), spawn $ "variety -f && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_n ), spawn $ "variety -n && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_p ), spawn $ "variety -p && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_t ), spawn $ "variety -t && wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")
  , ((mod1Mask .|. shiftMask , xK_u ), spawn $ "wal -i $(cat $HOME/.config/variety/wallpaper/wallpaper.jpg.txt)&")

  --CONTROL + SHIFT KEYS

  , ((controlMask .|. shiftMask , xK_Escape ), spawn $ "xfce4-taskmanager")

  --SCREENSHOTS

  , ((0, xK_Print), spawn $ "scrot 'ArcoLinux-%Y-%m-%d-%s_screenshot_$wx$h.jpg' -e 'mv $f $$(xdg-user-dir PICTURES)'")
  , ((controlMask, xK_Print), spawn $ "xfce4-screenshooter" )
  , ((controlMask .|. shiftMask , xK_Print ), spawn $ "gnome-screenshot -i")


  --MULTIMEDIA KEYS

  -- Mute volume
  , ((0, xF86XK_AudioMute), spawn $ "amixer -q set Master toggle")

  -- Decrease volume
  , ((modMask, xK_KP_Subtract), spawn $ "amixer -q set Master 5%-")

  -- Increase volume
  , ((modMask, xK_KP_Add), spawn $ "amixer -q set Master 5%+")

  -- Increase brightness
  , ((0, xF86XK_MonBrightnessUp),  spawn $ "xbacklight -inc 5")

  -- Decrease brightness
  , ((0, xF86XK_MonBrightnessDown), spawn $ "xbacklight -dec 5")

--  , ((0, xF86XK_AudioPlay), spawn $ "mpc toggle")
--  , ((0, xF86XK_AudioNext), spawn $ "mpc next")
--  , ((0, xF86XK_AudioPrev), spawn $ "mpc prev")
--  , ((0, xF86XK_AudioStop), spawn $ "mpc stop")

  , ((0, xF86XK_AudioPlay), spawn $ "playerctl play-pause")
  , ((0, xF86XK_AudioNext), spawn $ "playerctl next")
  , ((0, xF86XK_AudioPrev), spawn $ "playerctl previous")
  , ((0, xF86XK_AudioStop), spawn $ "playerctl stop")


  --------------------------------------------------------------------
  --  XMONAD LAYOUT KEYS

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space), sendMessage NextLayout)

  --Focus selected desktop
  , ((mod1Mask, xK_Tab), nextWS)

  --Focus selected desktop
  , ((modMask, xK_Tab), moveTo Next NonEmptyWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Left ), prevWS)

  --Focus selected desktop
  , ((controlMask .|. mod1Mask , xK_Right ), nextWS)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space), setLayout $ XMonad.layoutHook conf)

  -- Move focus to the next window.
  , ((modMask, xK_j), windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k), windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask .|. shiftMask, xK_m), windows W.focusMaster  )

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j), windows W.swapDown  )

  -- Swap the focused window with the next window.
  , ((controlMask .|. modMask, xK_Down), windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k), windows W.swapUp    )

  -- Swap the focused window with the previous window.
  , ((controlMask .|. modMask, xK_Up), windows W.swapUp  )

  -- Shrink the master area.
  , ((controlMask .|. shiftMask , xK_h), sendMessage Shrink)

  -- Expand the master area.
  , ((controlMask .|. shiftMask , xK_l), sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Left), sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((controlMask .|. modMask, xK_Right), sendMessage (IncMasterN (-1)))

  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)

  --Keyboard layouts
  --qwerty users use this line
   | (i, k) <- zip (XMonad.workspaces conf) [xK_1,xK_2,xK_3,xK_4,xK_5,xK_6,xK_7]

  --French Azerty users use this line
  -- | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_minus, xK_egrave, xK_underscore, xK_ccedilla , xK_agrave]

  --Belgian Azerty users use this line
  -- | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_section, xK_egrave, xK_exclam, xK_ccedilla, xK_agrave]

      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)
      , (\i -> W.greedyView i . W.shift i, shiftMask)]]



myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, 1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, 2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, 3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster))

    ]

--XMOBAR
main = do

        xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc" -- xmobar monitor 1
        xmonad $ ewmh $ mydefaults {
        logHook =  dynamicLogWithPP $ def {
        ppOutput = \x -> System.IO.hPutStrLn xmproc0 x
        , ppCurrent = xmobarColor "#b7e07c" "#3c4457" . wrap "" "" -- Current workspace in xmobar
        , ppVisible = xmobarColor "#b7e07c" ""                -- Visible but not current workspace
        , ppHidden = xmobarColor "#F07178" "" . wrap "" ""   -- Hidden workspaces in xmobar
        , ppHiddenNoWindows = xmobarColor "#82AAFF" ""        -- Hidden workspaces (no windows)
        , ppTitle = xmobarColor "#ffb26b" "" . shorten 100     -- Title of active window in xmobar
        , ppSep =  "<fc=#c792ea> | </fc>"                     -- Separators in xmobar
        , ppUrgent = xmobarColor "#C45500" "" . wrap "!" "!"  -- Urgent workspace
		  , ppExtras  = [windowCount]
        , ppWsSep = ""
        , ppOrder = \(ws:l:t:ex) -> [ws,l]++ex++[t]
 }
}
