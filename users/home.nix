{ config, pkgs, lib, ... }:

{
    home = {
        username = "midori";
        homeDirectory = "/home/midori";
        stateVersion = "23.05";
        file = {
            ".icons/default".source = "${pkgs.capitaine-cursors}/share/icons/capitaine-cursors-white";
            ".local/bin" = {
                source = ../bin;
                recursive = true;
            };
        };
        packages = with pkgs; [
            clipcat
            nixops
            google-cloud-sdk
            steam-tui
            qemu
            psmisc
            librewolf
            ardour
            docker
            chromium
            firefox
            tmux  
            gnupg
            pinentry
            flatpak
            discord
            (retroarch.override {
                cores = with libretro; [
                    desmume
                    mgba
                    sameboy
                ];
            })
            gimp
            guvcview
            steamcmd
            prismlauncher
            capitaine-cursors
            lightly-boehs
            appimage-run
            pavucontrol
            vlc
            ffmpeg
            kdenlive
            p7zip
            scrot
            ryujinx
            htop-vim
            xclip
            xdotool
            rofi
            rofimoji
            rofi-bluetooth
            rofi-calc
            rofi-pulse-select
            rofi-systemd
            lutris
            vulkan-tools
            gamemode
            xsane
            obs-studio
            (wineWowPackages.full.override {
                wineRelease = "staging";
                mingwSupport = true;
            })

            youtube-tui
            wiki-tui
            qbittorrent

            #(steam-tui.overrideAttrs({
            #    src = pkgs.fetchFromGitHub {
            #        owner = "midorishibukawa";
            #        repo = "steam-tui";
            #        rev = "70cefd70dbf6389098d0c0984db1743e6aca2230";
            #        sha256 = "ei1HnEL38ZxkXFCliW3ZAt5CQuybgT47cav5u7dOcW4=";
            #    };
            #}))

            neofetch

            bun
            ccls
            clang
            cabal-install
            winetricks
            cabal2nix
            ghc
            stylish-haskell
            gnumake

            lua-language-server

            nil

            (picom.overrideAttrs({
                src = pkgs.fetchFromGitHub {
                    repo = "picom";
                    owner = "ibhagwan";
                    rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
                    sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
                };
            }))
        ] ++ (with pkgs.haskellPackages; [
            greenclip
            haskell-language-server
            xmobar
        ]) ++ (with pkgs.nodePackages; [
            live-server
            typescript-language-server
        ]) ++ (with pkgs.libsForQt5; [
            kdenlive
        ]);

        sessionVariables = {
            NIX_DATA_DIR = "$HOME/.local/etc/nix";
            XDG_BIN_HOME = "$HOME/.local/bin";
            XDG_CACHE_HOME = "$HOME/.local/cache";
            XDG_CONFIG_HOME = "$HOME/.local/config";
            XDG_DATA_HOME = "$HOME/.local/data";
            XDG_STATE_HOME = "$HOME/.local/state";
            STARSHIP_CONFIG = "$XDG_CONFIG_HOME/starship.toml";
            ZDOTDIR = "XDG_CONFIG_HOME/zsh";
            NIXOS_CONFIG_DIR = "$HOME/.local/config/nixos.config";
        };
    };
        
        gtk = {
            enable = true;
            theme = {
                name = "lightly";
                package = pkgs.lightly-boehs;
            };
            cursorTheme = {
                name = "capitaine";
                package = pkgs.capitaine-cursors;
            };
        };
    

    nixpkgs.config = {
        permittedInsecurePackages = [
            "electron-24.8.6"
            "python-2.7.18.6"
            "python2.7-certifi-2021.10.8"
            "python2.7-pyjwt-1.7.1"
            "openssl-1.1.1w"
        ];
        allowUnfree = true;
    };

    programs = {
        home-manager.enable = true;

        git = {
            enable = true;
            userName = "midorishibukawa";
            userEmail = "midori@shibukawa.io";
            extraConfig = {
                pull.rebase = true;
            };
        };

        kitty = {
            enable = true;
        };

        nixvim = {
            enable = true;
            colorscheme = "rose-pine";
            plugins = {
                cmp-buffer.enable = true;
                cmp-nvim-lsp.enable = true;
                cmp-nvim-lua.enable = true;
                cmp-path.enable = true;
                cmp_luasnip.enable = true;
                nvim-cmp.enable = true;
                telescope.enable = true;
                treesitter.enable = true;
            };

            extraPlugins = with pkgs.vimPlugins; [
                lsp-zero-nvim
                friendly-snippets
                rose-pine
                nvim-lspconfig
            ];

            extraConfigLua = ''
                ./config/nvim/lua/midori/init.lua
            '';
        };
        starship = {
            enable = true;
            enableZshIntegration = true;
            settings = lib.importTOML ./config/starship.toml;
        };
        zsh = {
            autocd = true;
            dotDir = ".local/config/zsh";
            enableAutosuggestions = true;
            enableCompletion = true;
            enable = true;

            shellAliases = {
                dev = "nix develop -c $SHELL";
                ls = "ls -a --color=auto";
                ll = "ls -lah --color=auto";
                ga = "git add";
                gcm = "git commit -m";
                gco = "git checkout";
                gi = "git init";
                gps = "git push";
                gpl = "git pull";
                gs = "git status";
                vi = "nvim .";
                v = "nvim";
            };

            zplug = {
                enable = true;
                plugins = [
                { name = "zsh-users/zsh-autosuggestions";           }
                { name = "zsh-users/zsh-syntax-highlighting";       }
                { name = "zsh-users/zsh-completions";               }
                { name = "zsh-users/zsh-history-substring-search";  }
                { name = "jeffreytse/zsh-vi-mode";                  }
                { name = "agkozak/zsh-z";                           }
                { name = "desyncr/auto-ls";                          }
                ];
            };
        };
    };

    xdg = {
        configFile = {
            "gtk-3.0" = {
                source = ./config/gtk-3.0;
                recursive = true; 
            };
            kitty = {
                source = ./config/kitty;
                recursive = true;
            };
            nvim = {
                source = ./config/nvim;
                recursive = true;
            };
            picom = {
                source = ./config/picom; 
                recursive = true;
            };
            rofi = {
                source = ./config/rofi;
                recursive = true; 
            };
            xmonad = {
                source = ./config/xmonad;
                recursive = true;
            };
            X11 = {
                source =./config/X11;
                recursive = true;
            };
        };
        configHome = "/home/midori/.local/config/";
    };

}
