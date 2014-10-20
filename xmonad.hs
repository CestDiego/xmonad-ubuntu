import           XMonad
import           XMonad.Hooks.SetWMName

--Dmenu
import           XMonad.Util.Dmenu

-- Utilities
import           XMonad.Util.EZConfig (additionalKeysP, additionalMouseBindings)
import           XMonad.Util.NamedScratchpad (NamedScratchpad(NS), namedScratchpadManageHook, namedScratchpadAction, customFloating)
import           XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import           Graphics.X11.ExtraTypes.XF86

-- Dbus For Spotify
-- import DBus
-- import DBus.Client

-- Layouts Modifires
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Circle
import           XMonad.Layout.Fullscreen

import           XMonad.Layout.PerWorkspace (onWorkspace)
import           XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import           XMonad.Layout.WorkspaceDir
import           XMonad.Layout.Spacing (spacing)
import           XMonad.Layout.Minimize
import           XMonad.Layout.Maximize
import           XMonad.Layout.BoringWindows (boringWindows)
import           XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import           XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import           XMonad.Layout.Reflect (reflectVert, reflectHoriz, REFLECTX(..), REFLECTY(..))
import           XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), Toggle(..), (??))
import           XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))

-- System Stuff
import           System.IO
import           System.Environment -- get Environment Variables

-- WorkSpaces Switch
import XMonad.Actions.CycleWS			-- nextWS, prevWS

-- Actions
import           XMonad.Actions.SpawnOn
import           XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import           XMonad.Actions.CopyWindow (kill1, copyToAll, killAllOtherCopies, runOrCopy)
import           XMonad.Actions.GridSelect (GSConfig(..), goToSelected, bringSelected, colorRangeFromClassName, buildDefaultGSConfig)

import Data.List ---Clickable workspaces
import qualified Data.Map as M
import           Data.Ratio ((%))
import           XMonad.Actions.Plane
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ICCCMFocus
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.UrgencyHook
import qualified XMonad.StackSet as W

-- Layouts
import           XMonad.Layout.ZoomRow (zoomRow, zoomIn, zoomOut, zoomReset, ZoomMessage(ZoomFullToggle))
import           XMonad.Layout.Grid
-- import XMonad.Layout.GridVariants (Grid(Grid))
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.SimplestFloat
import           XMonad.Layout.IM
import           XMonad.Layout.OneBig
import           XMonad.Layout.ThreeColumns

-- Layout Modifiers

import           XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))


-- Prompts
import           XMonad.Prompt (defaultXPConfig, XPConfig(..), XPPosition(Top), Direction1D(..))
{-
  Xmonad configuration variables. These settings control some of the
  simpler parts of xmonad's behavior and are straightforward to tweak.
-}

myModMask            = mod4Mask       -- changes the mod key to "super"
myBorderWidth        = 1              -- width of border around windows
myFocusedBorderColor = "#746C48"
myNormalBorderColor  = "#1f1f1f"
myTerminal           = "urxvt"   -- which terminal software to use

myFont          = "xft:Monaco"
myColorBG       = "#1f1f1f"
myColorWhite    = "#eddcd3"
myColorRed      = "#cd546c"
myColorBrown    = "#989584"

myHomeDir       = getEnv "HOME"

-- Prompts colors
myPromptConfig =
    defaultXPConfig { font                  = myFont
                    , bgColor               = myColorBG
                    , fgColor               = myColorRed
                    , bgHLight              = myColorBG
                    , fgHLight              = myColorBrown
                    , borderColor           = myColorBG
                    , promptBorderWidth     = myBorderWidth
                    , height                = 20
                    , position              = Top
                    , historySize           = 0
                    }

-- Grid selector colors
myGridConfig = colorRangeFromClassName
    (0x18,0x15,0x12) -- lowest inactive bg
    (0x18,0x15,0x12) -- highest inactive bg
    (0x18,0x15,0x12) -- active bg
    (0x98,0x95,0x84) -- inactive fg
    (0xcd,0x54,0x6c) -- active fg

myGSConfig colorizer  = (buildDefaultGSConfig myGridConfig)
    { gs_cellheight   = 65
    , gs_cellwidth    = 120
    , gs_cellpadding  = 10
    , gs_font         = myFont
    }

{-
  Workspace configuration. Here you can change the names of your
  workspaces. 
-}

-- myWorkspaces = [ "i"
-- 		,"ii"	
-- 		,"iii"	
-- 		,"iv"	
-- 		,"v"
-- 		,"vi"
-- 		,"vii"
-- 		,"viii"
-- 		,"ix"
--                 ]       
myWorkspaces = clickable $ ["i"
		,"ii"	
		,"iii"	
		,"iv"	
		,"v"
		,"vi"
		,"vii"
		,"viii"
		,"ix"]	
	where clickable l = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
				(i,ws) <- zip [1..] l, let n = i ]
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
---SCRATCHPADS
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
                   -- Remember to install the urxvt script that checks if there is a daemon running
myScratchpads =
              -- [ NS "terminal" "urxvtc -name terminal -e tmux attach"     (resource =? "terminal") myPosition
              [ NS "terminal" "urxvt -name terminal"                                                 (resource =? "terminal") myTermPosition
              , NS "music" "urxvt -name playlist -e ncmpcpp"                             (resource =? "music")    myPositionBiggerSE
              -- , NS "clock" "urxvt -name clock -e ncmpcpp -s clock"                                   (resource =? "clock")    myPositionBiggerSW
              -- , NS "clock" "urxvt -name browser -e ncmpcpp -s browser"                                   (resource =? "browser")    myPositionBiggerSE
              -- , NS "media-library" "urxvt -name media-library -e ncmpcpp -s media-library"           (resource =? "media-library")    myPositionBiggerSE
              , NS "alsa" "urxvt -name alsa -e alsamixer"                                            (resource =? "alsa")    myPositionBiggerSE
              , NS "rtorrent" "urxvt -name rtorrent -e rtorrent"                                     (resource =? "rtorrent") myPosition
              ] where
                myPosition = customFloating $ W.RationalRect (1/3) (1/3) (1/3) (1/3)
                myTermPosition = customFloating $ W.RationalRect (1/3) (1/5) (2/3) (1/3)
                myPositionBiggerSW = customFloating $ W.RationalRect (1/2) (1/2) (1/2) (1/2)  
                myPositionBiggerSE = customFloating $ W.RationalRect (1/5) (1/5) (1/3) (1/3)


{-
  Layout configuration. In this section we identify which xmonad
  layouts we want to use. I have defined a list of default
  layouts which are applied on every workspace, as well as
  special layouts which get applied to specific workspaces.

  Note that all layouts are wrapped within "avoidStruts". What this does
  is make the layouts avoid the status bar area at the top of the screen.
  Without this, they would overlap the bar. You can toggle this behavior
  by hitting "super-b" (bound to ToggleStruts in the keyboard bindings
  in the next section).
-}

-- Define group of default layouts used on most screens, in the
-- order they will appear.
-- "smartBorders" modifier makes it so the borders on windows only
-- appear if there is more than one visible window.
-- "avoidStruts" modifier makes it so that the layout provides
-- space for the status bar at the top of the screen.
defaultLayouts = smartBorders(avoidStruts(
  -- ResizableTall layout has a large master window on the left,
  -- and remaining windows tile on the right. By default each area
  -- takes up half the screen, but you can resize using "super-h" and
  -- "super-l".
  ResizableTall 1 (3/100) (1/2) []

  -- Mirrored variation of ResizableTall. In this layout, the large
  -- master window is at the top, and remaining windows tile at the
  -- bottom of the screen. Can be resized as described above.
  ||| Mirror (ResizableTall 1 (3/100) (1/2) [])

  -- Full layout makes every window full screen. When you toggle the
  -- active window, it will bring the active window to the front.
  ||| noBorders Full

  -- ThreeColMid layout puts the large master window in the center
  -- of the screen. As configured below, by default it takes of 3/4 of
  -- the available space. Remaining windows tile to both the left and
  -- right of the master window. You can resize using "super-h" and
  -- "super-l".
  -- ||| ThreeColMid 1 (3/100) (3/4)

  -- Circle layout places the master window in the center of the screen.
  -- Remaining windows appear in a circle around it
  -- ||| Circle

  -- Grid layout tries to equally distribute windows in the available
  -- space, increasing the number of columns and rows as necessary.
  -- Master window is at top left.
  ||| Grid))


-- Here we define some layouts which will be assigned to specific
-- workspaces based on the functionality of that workspace.

-- The chat layout uses the "IM" layout. We have a roster which takes
-- up 1/8 of the screen vertically, and the remaining space contains
-- chat windows which are tiled using the grid layout. The roster is
-- identified using the myIMRosterTitle variable, and by default is
-- configured for Pidgin, so if you're using something else you
-- will want to modify that variable.
chatLayout = avoidStruts(withIM (1%7) (Title "CestDIego") Grid)

-- The GIMP layout uses the ThreeColMid layout. The traditional GIMP
-- floating panels approach is a bit of a challenge to handle with xmonad;
-- I find the best solution is to make the image you are working on the
-- master area, and then use this ThreeColMid layout to make the panels
-- tile to the left and right of the image. If you use GIMP 2.8, you
-- can use single-window mode and avoid this issue.
gimpLayout = smartBorders(avoidStruts(ThreeColMid 1 (3/100) (3/4)))

-- Here we combine our default layouts with our specific, workspace-locked
-- layouts.
myLayouts =   onWorkspace (myWorkspaces !! 6) chatLayout
            $ onWorkspace (myWorkspaces !! 0) (avoidStruts (fullScreen))
            -- $ onWorkspace (myWorkspaces !! 7) (avoidStruts (simplestFloat))
            $ onWorkspace (myWorkspaces !! 2) (avoidStruts (tiledSpace ||| fullTile) ||| fullScreen)
            $ onWorkspace (myWorkspaces !! 3) (avoidStruts (tiledSpace ||| fullTile) ||| fullScreen)
            $ onWorkspace (myWorkspaces !! 8) gimpLayout
            $ defaultLayouts
            where
                myMusic            =   limitWindows 4  $ spacing 36 $ Mirror $ mkToggle (single MIRROR) $ mkToggle (single REFLECTX) $ mkToggle (single REFLECTY) $ OneBig (2/3) (2/3)
		tiled  		= spacing 5 $ ResizableTall nmaster delta ratio [] 

                fullScreen 	= noBorders(fullscreenFull Full)
                tiledSpace  	= limitWindows 4 $ spacing 34 $ ResizableTall nmaster delta ratio [] 
                fullTile        = ResizableTall nmaster delta ratio [] 
                bigMonitor  	= spacing 5 $ ThreeColMid nmaster delta ratio 

                -- Default number of windows in master pane
                nmaster = 1
                -- Percent of the screen to increment when resizing
                delta 	= 5/100
                -- Default proportion of the screen taken up by main pane
                ratio 	= toRational (2/(1 + sqrt 5 :: Double)) 



{-
  Management hooks. You can use management hooks to enforce certain
  behaviors when specific programs or windows are launched. This is
  useful if you want certain windows to not be managed by xmonad,
  or sent to a specific workspace, or otherwise handled in a special
  way.

  Each entry within the list of hooks defines a way to identify a
  window (before the arrow), and then how that window should be treated
  (after the arrow).

  To figure out to identify your window, you will need to use a
  command-line tool called "xprop". When you run xprop, your cursor
  will temporarily change to crosshairs; click on the window you
  want to identify. In the output that is printed in your terminal,
  look for a couple of things:
    - WM_CLASS(STRING): values in this list of strings can be compared
      to "className" to match windows.
    - WM_NAME(STRING): this value can be compared to "resource" to match
      windows.

  The className values tend to be generic, and might match any window or
  dialog owned by a particular program. The resource values tend to be
  more specific, and will be different for every dialog. Sometimes you
  might want to compare both className and resource, to make sure you
  are matching only a particular window which belongs to a specific
  program.

  Once you've pinpointed the window you want to manipulate, here are
  a few examples of things you might do with that window:
    - doIgnore: this tells xmonad to completely ignore the window. It will
      not be tiled or floated. Useful for things like launchers and
      trays.
    - doFloat: this tells xmonad to float the window rather than tiling
      it. Handy for things that pop up, take some input, and then go away,
      such as dialogs, calculators, and so on.
    - doF (W.shift "Workspace"): this tells xmonad that when this program
      is launched it should be sent to a specific workspace. Useful
      for keeping specific tasks on specific workspaces. In the example
      below I have specific workspaces for chat, development, and
      editing images.
-}

myManagementHooks :: [ManageHook]
myManagementHooks = [
  -- , (className =? "Komodo IDE" <&&> resource =? "Komodo_find2") --> doFloat
   (className =? "Gimp") --> doF (W.shift "ix")
  ]
  ++
  [ resource =? r  --> doShift (myWorkspaces !! 5) | r <- myFolderApps]
  ++
  [ resource =? r  --> doIgnore | r <- myIgnoredApps]
  ++
  [ resource =? r --> doShift (myWorkspaces !! 6) | r <- myChatApps]
  ++
  [ resource =? r --> doShift (myWorkspaces !! 4) | r <- myArmyApps]
  ++
  [ resource =? r --> doFloat                | r <- myFloatApps]
  where
    myChatApps       = ["hackspace.slack.com"]
    myIgnoredApps    = ["dzen2"]
    myFolderApps     = ["nautilus"]
    myArmyApps       = ["army-browser"
                        ,"army-clock"
                        ,"army-playlist"
                        ,"army-alsamixer"]

    javaApps         = "sun-awt-X11-XFramePeer"
    myFloatApps      = [ javaApps
                       , "simplescreenrecorder"
                       , "zeal"
                       , "player"
                       , "genymotion"
                       , "skype"
                       , "Monitor"
                       , "crx_hkhggnncdpfibdhinjiegagmopldibha" -- This is THe Google Calendar Stuff, change it if it changes
                       , "crx_knipolnnllmklapflnccelgolnpehhpl" -- THis is fucking Google Hangouts
                       , "Eclipse"
                       , "Android SDK Manager"
                       , "GMATPrep.exe"
                       , "variety"]



{-
  Here we actually stitch together all the configuration settings
  and run xmonad. We also spawn an instance of xmobar and pipe
  content into it via the logHook.
-}

--------------------------------------------------------------------------------------------------------------------
-- DZEN LOG RULES for workspace names, layout image, current program title
--------------------------------------------------------------------------------------------------------------------
myLogHook h = dynamicLogWithPP ( defaultPP
	{
		  ppCurrent		= dzenColor color15 background .	pad
		, ppVisible		= dzenColor color14 background . 	pad
                -- display other workspaces which contain windows as a brighter grey
		, ppHidden		= dzenColor color7 background . 	pad
		, ppHiddenNoWindows	= dzenColor color0 background .	pad
		, ppWsSep		= ""
		, ppSep			= "    "
		, ppLayout		= wrap "^ca(1,xdotool key super+space)" "^ca()" . dzenColor color2 background .
				(\x -> case x of
					"Full"				->	"^i(/home/io/.xmonad/dzen2/layout_full.xbm)"
					"Spacing 5 ResizableTall"	->	"^i(/home/io/.xmonad/dzen2/layout_tall.xbm)"
					"ResizableTall"			->	"^i(/home/io/.xmonad/dzen2/layout_tall.xbm)"
					"SimplestFloat"			->	"^i(/home/io/.xmonad/dzen2/mouse_01.xbm)"
					"Circle"			->	"^i(/home/io/.xmonad/dzen2/full.xbm)"
					_				->	"^i(/home/io/.xmonad/dzen2/grid.xbm)"
				) 
		-- , ppTitle	=  wrap "^ca(1,xdotool key alt+shift+x)^fg(#D23D3D)^fn(fkp)x ^fn()" "^ca()" . dzenColor foreground background . shorten 60 . pad
		-- , ppTitle	=  wrap "^ca(1,xdotool key super+C)" "^ca()" . dzenColor color15 background . shorten 100 . pad
		, ppTitle	=  dzenColor color15 background . shorten 100 . pad
		, ppOrder	=  \(ws:l:t:_) -> [ws,l, t]
		, ppOutput	=   hPutStrLn h
	} )
--------------------------------------------------------------------------------------------------------------------
-- Spawn pipes and menus on boot, set default settings
--------------------------------------------------------------------------------------------------------------------
myXmonadBar = "dzen2 -x '0' -y '0' "
              ++ " -h '24' -w '1000' "
              ++ " -ta 'l' "
              ++ " -fg '" ++ foreground
              ++ "' -bg '" ++ background
              ++ "' -fn " ++ myFont

myStatusBar = "/home/io/.xmonad/status_bar '"
              ++
              foreground ++ "' '" ++
              background ++"' "++
              myFont



              
main = do
  -- xmproc <- spawnPipe "xmobar /home/io/.xmonad/xmobarrc"
  dzenLeftBar     <- spawnPipe myXmonadBar
  dzenRightBar    <- spawnPipe myStatusBar
  xmonad $ withUrgencyHook NoUrgencyHook $ defaultConfig {
    terminal = myTerminal
  , borderWidth = myBorderWidth
  , focusedBorderColor = myFocusedBorderColor
  , normalBorderColor  = myNormalBorderColor
  , layoutHook = smartBorders $ myLayouts
  , workspaces = myWorkspaces
  , modMask = myModMask
  , handleEventHook = fullscreenEventHook
  , manageHook = manageDocks <+> manageHook defaultConfig
    <+> namedScratchpadManageHook myScratchpads
    <+> composeAll myManagementHooks
  , startupHook = do
      setWMName "LG3D"
      spawn "/home/io/.xmonad/startup-hook"
  , logHook     = myLogHook dzenLeftBar -- >> fadeInactiveLogHook 0xdddddddd

  -- , logHook = takeTopFocus <+> dynamicLogWithPP xmobarPP {
  --     ppOutput = hPutStrLn xmproc
  --     , ppTitle = xmobarColor myTitleColor "" . shorten myTitleLength
  --     , ppCurrent = xmobarColor myCurrentWSColor "" . wrap myCurrentWSLeft myCurrentWSRight
  --     , ppVisible = xmobarColor myVisibleWSColor ""
  --       . wrap myVisibleWSLeft myVisibleWSRight
  --     , ppUrgent = xmobarColor myUrgentWSColor ""
  --       . wrap myUrgentWSLeft myUrgentWSRight
  --   }
  }
    `additionalKeysP` myKeyBindings

myKeyBindings =
  [
          ("M-b", sendMessage ToggleStruts)
        , ("M-p", spawn "exe=`dmenu_path | dmenu_run -fn xft:Monaco` && eval \"exec $exe\"")
        , ("<XF86AudioMute>",   spawn "amixer set Master toggle")
        , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute")
        , ("<XF86AudioRaiseVolume>", spawn "amixer set Mater 5%+ unmute")
        , ("<XF86MonBrightnessUp>", spawn "xbacklight +20")
        , ("<XF86MonBrightnessDown>", spawn "xbacklight -20")
        
        , ("M-q", spawn "killall dzen2; killall conky; killall tint2; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart" )
    -- , ((0, 0x1008FF12), spawn "amixer -q set Master toggle")
    -- , ((0, 0x1008FF11), spawn "amixer -q set Master 10%-")
    -- , ((0, 0x1008FF13), spawn "amixer -q set Master 10%+")
        -- , ((0, 0x1008FF13), spawn "amixer -q set Master 10%+")
    -- , ((myModMask .|. mod1Mask, xK_space), spawn "synapse")

          
        -- Spotify controls
        -- , ("<XF86Launch1>", spotifyCtrl dbusClient "PlayPause")
        -- , ("<XF86AudioPlay>", spotifyCtrl dbusClient "PlayPause")
        -- , ("<XF86AudioNext>", spotifyCtrl dbusClient "Next")
        -- , ("<XF86AudioPrev>", spotifyCtrl dbusClient "Previous")
        -- , ("<XF86AudioStop>", spotifyCtrl dbusClient "Stop")

          
        -- Keyboard Layout
        , ("M-S-d",             spawn "setxkbmap -layout us")
        , ("M-S-s",             spawn "setxkbmap -layout es")

        -- Workspace Navigation
        , ("M-`",               moveTo Prev NonEmptyWS)

        , ("M-C-M1-<Up>",       sendMessage Arrange)
        , ("M-C-M1-<Down>",     sendMessage DeArrange)
        , ("M-<Up>",            sendMessage (MoveUp 10))
        , ("M-<Down>",          sendMessage (MoveDown 10))
        , ("M-<Right>",         sendMessage (MoveRight 10))
        , ("M-<Left>",          sendMessage (MoveLeft 10))
        , ("M-S-<Up>",          sendMessage (IncreaseUp 10))
        , ("M-S-<Down>",        sendMessage (IncreaseDown 10))
        , ("M-S-<Right>",       sendMessage (IncreaseRight 10))
        , ("M-S-<Left>",        sendMessage (IncreaseLeft 10))
        , ("M-C-<Up>",          sendMessage (DecreaseUp 10))
        , ("M-C-<Down>",        sendMessage (DecreaseDown 10))
        , ("M-C-<Right>",       sendMessage (DecreaseRight 10))
        , ("M-C-<Left>",        sendMessage (DecreaseLeft 10))

    -- Music control via NCMCPP
        , ("M-M1-<Down>",       spawn "ncmpcpp toggle")
        , ("M-M1-<Left>",       spawn "ncmpcpp prev")
        , ("M-M1-<Right>",      spawn "ncmpcpp next")

    -- Layouts
        , ("M-h",               sendMessage Shrink)
        , ("M-l",               sendMessage Expand)
        , ("M-S-;",             sendMessage zoomReset)
        , ("M-;",               sendMessage ZoomFullToggle)

    -- Apps
        , ("M-M1-o",            runOrCopy "urxvt -name htop -e htop" (resource =? "htop"))
        , ("M-f",               raiseMaybe (runInTerm "-name ranger" "ranger") (resource =? "ranger"))
        , ("M-S-t",             raiseMaybe (runInTerm "-name newsbeuter" "newsbeuter") (resource =? "newsbeuter"))
        , ("M-C-j",             raiseMaybe (runInTerm "-name julia" "julia") (resource =? "julia"))
        , ("M-v",               raiseMaybe (runInTerm "-name weechat" "weechat-curses") (resource =? "weechat"))
        , ("<Print>",           spawn " sleep 0.2; scrot -e 'mv $f /home/io/Pictures/Screenshots/' & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")
        , ("M-<Print>",         spawn " sleep 0.2; scrot -s -e 'mv $f /home/io/Pictures/Screenshots/' & mplayer /usr/share/sounds/freedesktop/stereo/screen-capture.oga")

    -- Scratchpads
        , ("M-g",               namedScratchpadAction myScratchpads "terminal")
        , ("M-M1-b",            namedScratchpadAction myScratchpads "rtorrent")
        , ("M-n",               namedScratchpadAction myScratchpads "music")
        , ("M-S-a",             namedScratchpadAction myScratchpads "alsa")

    -- Prompts
        , ("M-[",               goToSelected $ myGSConfig myGridConfig)
        , ("M-S-]",             bringSelected $ myGSConfig myGridConfig)
        , ("M-M1-;",               changeDir myPromptConfig)
  ]


myBitmapsDir	= "dzen2/"

background= "#1f1f1f"
foreground= "#D6C3B6"
color0=  "#332d29"
color8=  "#817267"
color1=  "#8c644c"
color9=  "#9f7155"
color2=  "#746C48"
color10= "#9f7155"
color3=  "#bfba92"
color11= "#E0DAAC"
color4=  "#646a6d"
color12= "#777E82"
color5=  "#766782"
color13= "#897796"
color6=  "#4B5C5E"
color14= "#556D70"
color7=  "#504339"
color15= "#9a875f"
