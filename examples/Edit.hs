import Control.Lens
import System.Process

import qualified DMenu

main :: IO ()
main = do
  let userFiles = [ "/home/m0rphism/.haskell-dmenu.conf"
                  , "/home/m0rphism/.conkyrc"
                  , "/home/m0rphism/.bash_profile"
                  , "/home/m0rphism/.bashrc"
                  , "/home/m0rphism/.ghci"
                  , "/home/m0rphism/.gitconfig"
                  , "/home/m0rphism/.gtkrc-2.0"
                  , "/home/m0rphism/.gtkrc-2.0.mine"
                  , "/home/m0rphism/.gvimrc"
                  , "/home/m0rphism/.haskeline"
                  , "/home/m0rphism/.inputrc"
                  , "/home/m0rphism/.spacemacs"
                  , "/home/m0rphism/.vimrc"
                  , "/home/m0rphism/.Xressources"
                  , "/home/m0rphism/.mpd/mpd.conf"
                  , "/home/m0rphism/.xmonad/xmonad.hs"
                  , "/home/m0rphism/.vifm/vifmrc"
                  , "/home/m0rphism/.config/mpv/mpv.conf"
                  , "/home/m0rphism/.config/mpv/input.conf"
                  , "/home/m0rphism/.config/mpv/input-image.conf"
                  , "/home/m0rphism/.config/profanity/profrc"
                  , "/home/m0rphism/.config/zathura/zathurarc"
                  , "/home/m0rphism/.nixpkgs/config.nix"
                  , "/home/m0rphism/.nixpkgs/mypackages.nix"
                  , "/home/m0rphism/Development/pkgs/packages.nix"
                  , "/home/m0rphism/Development/projects/projects.nix"
                  ]
  let rootFiles = [ "/etc/nixos/configuration.nix"
                  , "/etc/nixos/hardware-configuration.nix"
                  ]
  let userEditCmd = "gvim"
  let rootEditCmd = "gksudo gvim"

  let files = map Left userFiles ++ map Right rootFiles
  DMenu.runSelect (DMenu.prompt .= "edit") (either ("U  "++) ("R  "++)) files >>= \case
    Right (Left userFile :_) → callCommand $ userEditCmd ++ " " ++ userFile
    Right (Right rootFile :_) → callCommand $ rootEditCmd ++ " " ++ rootFile
    _                 → pure ()

-- usage :: String
-- usage = unlines
--   [ "Usage: dmenu-edit"
--   ]
