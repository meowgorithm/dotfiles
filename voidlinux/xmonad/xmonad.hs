import System.IO
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.EZConfig ( additionalKeys )
import XMonad.Util.Run ( spawnPipe )
import XMonad.Actions.SpawnOn ( spawnHere )
import qualified Data.Map as Map

main :: IO ()
main =
    xmonad =<< xmobar myConfig

myConfig :: XConfig (Choose Tall (Choose (Mirror Tall) Full))
myConfig =
    def { terminal = "kitty"
        , modMask = mod4Mask
        , workspaces = myWorkspaces
        , normalBorderColor = "#262626"
        , focusedBorderColor = "#7951e0"
        , borderWidth = 4
        , keys = \c -> myKeys c `Map.union` keys def c
        }

myWorkspaces :: [String]
myWorkspaces =
    [ "一", "二", "三", "四", "五", "六", "七", "八", "九", "十" ]

myKeys :: (XConfig Layout -> Map.Map (ButtonMask, KeySym) (X ()))
myKeys conf@XConfig {XMonad.modMask = modMask} = Map.fromList
    [ ((mod4Mask, xK_p), spawn "rofi -show run")
    , ((mod4Mask .|. shiftMask, xK_s), spawnHere "fn=$HOME/screens/$(date +%m-%d-%Y-%H-%M.png); sel=$(slop -f \"-g %g\"); shotgun $sel $fn; Thunar $HOME/screens")
    ]
