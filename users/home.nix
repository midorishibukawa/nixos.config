{ config, pkgs, lib, ... }:

{
  home = {
    username = "midori";
    homeDirectory = "/home/midori";
    stateVersion = "23.05";

    packages = with pkgs; [
      firefox
      tmux
      
      gnupg
      pinentry
      
      pavucontrol
      scrot
      
      lutris
      vulkan-tools
      wineWowPackages.staging
      
      cabal-install
      cabal2nix
      haskell-language-server
      stylish-haskell

      lua-language-server

      typescript-language-server
    ];
  };

  services.picom.enable = true;

  programs = {
    home-manager.enable = true;

    git = {
      enable = true;
      userName = "midori shibukawa";
      userEmail = "midori@shibukawa.io";
    };

    kitty = {
      enable = true;
      extraConfig = lib.fileContents ./config/kitty/kitty.conf;
    };

    neovim = {
      enable = true;
      extraConfig = lib.fileContents ./config/nvim/init.lua;

      plugins = with pkgs.vimPlugins; [
        lsp-zero-nvim
	nvim-lspconfig
	nvim-cmp
	cmp-buffer
	cmp-path
	cmp_luasnip
	cmp-nvim-lsp
	cmp-nvim-lua
	friendly-snippets
	(nvim-treesitter.withPlugins (p: with p; [ c css haskell html java javascript lua nix ocaml rust typescript ]))
	rose-pine
        telescope-nvim
      ];
    };
  };
}
