{ config, pkgs, lib, ... }:

{
    home = {
        username = "midori";
        homeDirectory = "/home/midori";
        stateVersion = "23.05";
    
        packages = with pkgs; [
            chromium
            firefox
            tmux  
            gnupg
            pinentry

            appimage-run
            pavucontrol
            p7zip
            scrot
            xclip
            rofi
            lutris
            vulkan-tools
            (wineWowPackages.full.override {
                wineRelease = "staging";
                mingwSupport = true;
            })

            neofetch

            cabal-install
            cabal2nix
            ghc
            stylish-haskell

            lua-language-server

            nil

            haskellPackages.haskell-language-server
            haskellPackages.xmobar

            (picom.overrideAttrs({
                src = pkgs.fetchFromGitHub {
                    repo = "picom";
                        owner = "ibhagwan";
                        rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
                        sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
                };
            }))
        ];

        sessionVariables = {
            NIX_DATA_DIR = "$HOME/.local/etc/nix";
            XDG_BIN_HOME = "$HOME/.local/bin";
            XDG_CACHE_HOME = "$HOME/.local/cache";
            XDG_CONFIG_HOME = "$HOME/.local/config";
            XDG_DATA_HOME = "$HOME/.local/data";
            XDG_STATE_HOME = "$HOME/.local/state";
            STARSHIP_CONFIG = "$XDG_CONFIG_HOME/starship.toml";
            ZDOTDIR = "XDG_CONFIG_HOME/zsh";
        };
    };

    nixpkgs.config.allowUnfree = true;

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
                ls = "ls -a --color=auto";
                ll = "ls -lah --color=auto";
                ga = "git add";
                gcm = "git commit -m";
                gco = "git checkout";
                gi = "git init";
                gps = "git push";
                gpl = "git pull";
                gs = "git status";
                update = "$XDG_CONFIG_HOME/nixos.config/bin/update";
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
                ];
            };
        };
    };

    xdg = {
        configFile = {
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
            xmonad = {
                source = ./config/xmonad;
                recursive = true;
            };
        };
        configHome = "/home/midori/.local/config/";
    };

}
