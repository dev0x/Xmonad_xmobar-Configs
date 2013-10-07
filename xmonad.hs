-- Import Statements
import XMonad
import qualified Data.Map as M
import XMonad.Util.EZConfig(additionalKeys)
import Graphics.X11.Xlib
import System.IO
import System.Exit

-- hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

-- utils
import XMonad.Util.SpawnOnce
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Scratchpad

--layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.Tabbed
import XMonad.Layout.PerWorkspace(onWorkspace)
import XMonad.Layout.Grid


-- Data.Ratio due to the IM layout
import Data.Ratio ((%))

--------------------------------------------------------------------------------
--Define Terminal
myTerminal :: String
myTerminal = "urxvt"

--------------------------------------------------------------------------------
--Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces = ["1:chat","2:web","3:web2","4:code","5:code","6:code","7:misc", "8:term", "9:term1"]

--------------------------------------------------------------------------------
-- Make the bordercolor different here because well...  this is where it is defined.  BAM spice-weasel!
myNormalBorderColor = "#808080"
myFocusedBorderColor = "#009900"

--------------------------------------------------------------------------------
--logHook
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP {ppOutput = hPutStrLn h}

--- Theme For Tabbed layout
myTheme = defaultTheme { decoHeight = 16
						, activeColor = "#a6c292"
						, activeBorderColor = "#a6c292"
						, activeTextColor = "#000000"
						, inactiveBorderColor = "#000000"
						}
--------------------------------------------------------------------------------
myLayoutHook	=  onWorkspace "1:chat" imLayout 
				 $ onWorkspace "2:web" webL
				 $ onWorkspace "3:web2" webL
				 $ standardLayouts 
		where
		standardLayouts =	avoidStruts $ (tiled ||| reflectTiled ||| Mirror tiled ||| Grid ||| Full)
 
		--Layouts
		tiled			= smartBorders (ResizableTall 1 (2/100) (1/2) [])
		reflectTiled	= (reflectHoriz tiled)
		tabLayout		= (tabbed shrinkText myTheme)
		full			= noBorders Full
 
		--Im Layout
		imLayout = avoidStruts 
			$ smartBorders 
			$ withIM ratio hangoutRoster 
			$ reflectHoriz 
			$ withIM hangoutRatio hangoutRoster (tiled ||| reflectTiled ||| Grid) where
				chatLayout  = Grid
		ratio = (1%9)
		hangoutRatio = (1%8)
		hangoutRoster = And (ClassName "hangouts") (Role "list")
		 
		--Web Layout
		webL = tabLayout  ||| tiled ||| reflectHoriz tiled |||  full 
		 
--------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
				spawnOnce "xmobar -x 1 ~/.xmobarrc2"

customPP :: PP
customPP = def{
			ppHidden = xmobarColor "#00FF00" ""
			, ppCurrent = xmobarColor "#859900" "" . wrap "[" "]" 
			, ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
			, ppLayout = xmobarColor "#2AA198" "" 
			, ppTitle = xmobarColor "#00FF00" "" . shorten 80
			, ppSep = "<fc=#0033FF> | </fc>"
		} 
 
--------------------------------------------------------------------------------
--Run XMonad with the defaults
main = do
  xmproc <- spawnPipe "xmobar" 
  xmonad	$ def { 
	terminal = myTerminal
	,workspaces = myWorkspaces
	,normalBorderColor = myNormalBorderColor
	,focusedBorderColor = myFocusedBorderColor
	,logHook = myLogHook xmproc
	,layoutHook = myLayoutHook
	,modMask = mod4Mask
	,keys = myKeys
	,startupHook = myStartupHook >> setWMName "LG3D"
  } 

myKeys x = M.union (M.fromList (newKeys x)) (keys defaultConfig x)
--------------------------------------------------------------------------------
newKeys conf@(XConfig {XMonad.modMask = modm}) = [    
    ((mod4Mask, xK_p), spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'")  --Uses a colorscheme with dmenu
    ,((mod4Mask, xK_f), spawn "urxvt -e xcalc")
    ,((mod4Mask, xK_Return), spawn "urxvt")
    --,((mod4Mask, xK_m), spawn "chromium --app='https://mail.google.com'")
    --,((mod4Mask .|. shiftMask, xK_s), spawn "sudo /usr/sbin/pm-suspend")
    --,((mod4Mask .|. shiftMask, xK_h), spawn "sudo /usr/sbin/pm-hibernate")
    ,((0, xK_Print), spawn "sleep 0.2; scrot -s")
	]
--------------------------------------------------------------------------------
     

