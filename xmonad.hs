-- Import Statements
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ICCCMFocus
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.NoBorders
import Graphics.X11.Xlib
import System.IO

--Define Terminal
myTerminal = "urxvt"
myWorkspaces = ["1:irc","2:web","3:vim","4:scala","5:eclipse","6:code","7:misc"]

myNormalBorderColor = "#808080"
myFocusedBorderColor = "#009900"
myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ customPP {ppOutput = hPutStrLn h}

myLayoutHook =  avoidStruts $ layoutHook defaultConfig 

myStartupHook = setWMName "LG3D" -- stupid java

customPP :: PP
customPP = defaultPP{
			ppHidden = xmobarColor "#00FF00" ""
			, ppCurrent = xmobarColor "#FF0000" "" . wrap "[" "]" 
			, ppUrgent = xmobarColor "#FF0000" "" . wrap "*" "*"
			, ppLayout = xmobarColor "#FF0000" "" 
			, ppTitle = xmobarColor "#00FF00" "" . shorten 80
			, ppSep = "<fc=#0033FF> | </fc>"
		} 
 
--Run XMonad with the defaults
main = do
  xmproc <- spawnPipe "xmobar" 
  xmonad	$ defaultConfig { 
	terminal = myTerminal
	,workspaces = myWorkspaces
    ,normalBorderColor = myNormalBorderColor
    ,focusedBorderColor = myFocusedBorderColor
	,logHook = takeTopFocus >> myLogHook xmproc
	,layoutHook = myLayoutHook
	,modMask = mod4Mask
    --,keys = myKeys
    ,startupHook = myStartupHook 
	} `additionalKeys`
    [
    ((mod4Mask, xK_p), spawn "dmenu_run -nb '#3F3F3F' -nf '#DCDCCC' -sb '#7F9F7F' -sf '#DCDCCC'")  --Uses a colorscheme with dmenu
    ,((mod4Mask, xK_f), spawn "urxvt -e xcalc")
    ,((mod4Mask, xK_Return), spawn "urxvt")
    --,((mod4Mask, xK_m), spawn "chromium --app='https://mail.google.com'")
    ,((mod4Mask .|. shiftMask, xK_s), spawn "sudo /usr/sbin/pm-suspend")
    ,((mod4Mask .|. shiftMask, xK_h), spawn "sudo /usr/sbin/pm-hibernate")
    ,((mod4Mask, xK_h), spawn "synergyc 192.168.0.187")
    ,((0, xK_Print), spawn "sleep 0.2; scrot -s")
    ] 

