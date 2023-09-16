{ config, pkgs, ... }:

{

  home = {
    username = "midori";
    homeDirectory = "/home/midori";
    stateVersion = "23.05";

    packages = with pkgs; [
      firefox
      gnupg
      kitty
      lutris
      pavucontrol
      pinentry
      tmux
      vulkan-tools
      wineWowPackages.staging
    ];
  };

  programs = {
    home-manager.enable = true;

    git = {
      enable = true;
      userName = "midori shibukawa";
      userEmail = "midori@shibukawa.io";
    };
  };
}
