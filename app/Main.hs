module Main where

import Control.Monad
import Data.Maybe
import Data.List

import XMonad hiding (Tall)
import XMonad.Config.Desktop
import XMonad.Hooks.ManageDocks
import XMonad.Layout.TwoPanePersistent
-- import XMonad.Layout.StackTile
import XMonad.Layout.Spacing
import XMonad.Layout.HintedTile
import XMonad.Layout.GridVariants as G
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import System.Exit
import XMonad.Layout.ResizableTile
-- import XMonad.Actions.WindowNavigation
import XMonad.Layout.NoBorders
import XMonad.Hooks.EwmhDesktops

stepSize = 1/5

main = xmonad $ ewmh desktopConfig
  { layoutHook = avoidStruts $ smartBorders $
      spacingRaw True (Border 24 8 8 8) True (Border 8 8 8 8) True $
      -- Tall { tallNMaster = 1, tallRatio = 1/2, tallRatioIncrement = stepSize }
      -- Mirror (StackTile 2 stepSize (8/10))
      -- TallGrid 1 2 (8/10) (16/10) stepSize
      HintedTile { nmaster = 2 , delta = stepSize , frac = 4/5 , alignment = TopLeft , orientation = Wide }
      ||| TwoPanePersistent {slaveWin = Nothing, dFrac = stepSize, mFrac = 3/5}
      -- ||| Full
    , modMask = mod4Mask
    , keys = myKeys
    , normalBorderColor = "#222222"
    , focusedBorderColor = "#FFB53A"
    , borderWidth = 3
    , startupHook = setFullscreenSupported
    , handleEventHook = handleEventHook desktopConfig <+> fullscreenEventHook
    , workspaces = []
  }

-- https://www.reddit.com/r/xmonad/comments/gc4b9i/what_is_the_best_way_to_make_xmonad_respect_true/fpbnsv9/?utm_source=reddit&utm_medium=web2x&context=3
setFullscreenSupported :: X ()
setFullscreenSupported = addSupported ["_NET_WM_STATE", "_NET_WM_STATE_FULLSCREEN"]

addSupported :: [String] -> X ()
addSupported props = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    newSupportedList <- mapM (fmap fromIntegral . getAtom) props
    io $ do
      supportedList <- fmap (join . maybeToList) $ getWindowProperty32 dpy a r
      changeProperty32 dpy r a aTOM propModeReplace (nub $ newSupportedList ++ supportedList)


myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_c      ), kill) -- %! Close the focused window

    , ((modMask,               xK_space  ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space  ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_l      ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Right  ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_Left   ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m      ), windows W.focusMaster  ) -- %! Move focus to the master window

    -- modifying the window order
    , ((modMask .|. shiftMask, xK_m      ), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_Right  ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_Left   ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_Up     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_Down   ), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((modMask,               xK_t      ), withFocused $ windows . W.sink) -- %! Push window back into tiling
    -- increase or decrease number of windows in the master area
    , ((modMask.|. shiftMask,  xK_Up     ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask.|. shiftMask,  xK_Down   ), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q      ), io exitSuccess) -- %! Quit xmonad
    , ((modMask              , xK_q      ), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else zenity --info --text=xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    ]
    -- ++
    -- -- mod-[1..9] %! Switch to workspace N
    -- -- mod-shift-[1..9] %! Move client to workspace N
    -- [((m .|. modMask, k), windows $ f i)
    --     | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    --     , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
    -- -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
