-- Import Statements
import XMonad
import qualified Data.Map as M
--import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.Xlib
import System.IO
import System.Exit

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.Place

-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad
--import XMonad.Util

--layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.SimplestFloat
--actions
import XMonad.Actions.CycleWS


-- Data.Ratio due to the IM layout
import Data.Ratio ((%))

--------------------------------------------------------------------------------
--Define Terminal
myTerminal :: String
myTerminal = "urxvt"

--------------------------------------------------------------------------------
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["tmux","web","web2","code","code1","code2","im","term","term1","melp"]

--------------------------------------------------------------------------------
-- Make the bordercolor different here because well...  this is where it is defined.  BAM spice-weasel!
myNormalBorderColor = "#000000"
myFocusedBorderColor = "#009900"
myBorderWidth = 1

--------------------------------------------------------------------------------
myManageHook = composeAll . concat $
   [ [ className =? "Chromium" --> doShift "web" ]
   , [ className =? "Gimp" --> doFloat]
   , [ className =? "steam" --> doFloat]
   , [ className =? "Steam" --> doFloat]
   , [ (className =? "Firefox" <&&> resource =? "Dialog") --> doFloat]
   , [ appName =? "crx_nckgahadagoaajjgafhacjanaoiihapd" --> (placeHook chatPlacement <+> doFloat)]
   ]

chatPlacement :: Placement
chatPlacement = withGaps (0,20,20,0) (inBounds (smart (1,1)))


--------------------------------------------------------------------------------
--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP {ppOutput = hPutStrLn h}

--- Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
						, activeColor = "#000000"
						, activeBorderColor = "#A6C292"
						, activeTextColor = "#CEFFAC"
						, inactiveColor = "#000000"
						, inactiveBorderColor = "#7C7C7C"
						, inactiveTextColor = "#EEEEEE"
						}
--------------------------------------------------------------------------------
myLayoutHook	=  onWorkspace "im" imLayout 
				 $ onWorkspace "web" webL
				 $ onWorkspace "web2" webL
				 $ onWorkspace "melp" steam 
				 $ standardLayouts 
		where
		standardLayouts =	avoidStruts $ (tiled ||| Mirror tiled ||| Grid ||| ThreeCol 1 (3/100) (1/2) ||| Full)
 
		--Layouts
		tiled			= smartBorders (ResizableTall 1 (2/100) (1/2) [])
		reflectTiled	= (reflectHoriz tiled)
		tabLayout		= (tabbed shrinkText myTheme)
		full			= noBorders Full
		steam			= simplestFloat
 
		--Im Layout
		imLayout = withIM (1%10) (Role "roster") (standardLayouts)
		 
		--Web Layout
		webL = avoidStruts $ (tabLayout ||| tiled ||| reflectHoriz tiled ||| Grid ||| Full)
		 
--------------------------------------------------------------------------------
--myStartupHook :: X ()
--myStartupHook = do
--		spawnOnce "xmobar -x 1 ~/.xmobarrc2"
myStartupHook = return()

customPP :: PP
customPP = defaultPP {
			ppHidden = xmobarColor "#959595" ""
			, ppCurrent = xmobarColor "#859900" "" . wrap "[" "]" 
			, ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
			, ppLayout = xmobarColor "#2AA198" "" 
			, ppTitle = xmobarColor "#00FF00" "" . shorten 100
			, ppSep = "<fc=#0033FF> | </fc>"
		} 
 
--------------------------------------------------------------------------------
--Run XMonad with the defaults
main = do
  xmproc <- spawnPipe "xmobar -x 0 ~/.xmobarrc" 
  xmonad	$ defaultConfig { 
	terminal = myTerminal
	,workspaces = myWorkspaces
	,manageHook = myManageHook
	,normalBorderColor = myNormalBorderColor
	,focusedBorderColor = myFocusedBorderColor
	,borderWidth = myBorderWidth
	,logHook = myLogHook xmproc
	,layoutHook = myLayoutHook
	,keys = myKeys
	,startupHook = myStartupHook >> setWMName "LG3D"
  } 

myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
--------------------------------------------------------------------------------
newKeys conf@(XConfig {XMonad.modMask = mod4Mask}) = [    
	    ((mod4Mask, xK_p), spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'")  --Uses a colorscheme with dmenu
	    ,((mod4Mask, xK_f), spawn "urxvt -e xcalc")
	    ,((mod4Mask.|.shiftMask, xK_l), spawn "slock")
	    ,((mod4Mask.|.shiftMask, xK_s), spawn "sudo pm-suspend")
	    ,((mod4Mask, xK_Return), spawn "urxvt")
	    ,((mod4Mask, xK_m), spawn "chromium --app='https://mail.google.com'")
	    ,((mod4Mask, xK_0), nextWS)
	    ,((0, xK_Print), spawn "sleep 0.2; scrot -s")
	]
--------------------------------------------------------------------------------
