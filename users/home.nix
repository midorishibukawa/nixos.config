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
      vulkan-tools
      wine
      winetricks
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
