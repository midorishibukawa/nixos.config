{ config, lib, pkgs, modulesPath, ... }:

{
  imports = [ 
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  boot = {
    extraModulePackages = [ ];
    kernelModules = [ "kvm-intel" ];
    
    initrd = {
        availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "usb_storage" "sd_mod" ];
        kernelModules = [ ];
      secrets."/crypto_keyfile.bin" = null;
      luks.devices = {
        "luks-279aad32-6cac-4931-9f69-75cfd67d517d".device = "/dev/disk/by-uuid/279aad32-6cac-4931-9f69-75cfd67d517d";
        "luks-b618c3bf-eb2d-4eee-a974-856d52962886" = {
          device = "/dev/disk/by-uuid/b618c3bf-eb2d-4eee-a974-856d52962886";
	  keyFile = "/crypto_keyfile.bin";
        };
      };
    };

    loader = {
      efi.canTouchEfiVariables = true;
      systemd-boot.enable = true;
    };
  };
  
  fileSystems = {
    "/" = { 
      device = "/dev/disk/by-uuid/1024088a-7b89-49a3-b84d-c9d22516e103";
      fsType = "ext4";
    };

    "/boot" = { 
      device = "/dev/disk/by-uuid/B5E2-D37F";
      fsType = "vfat";
    };
  };


  hardware = {
    bluetooth.enable = true;
    cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

    nvidia = {
      modesetting.enable = true;
      nvidiaSettings = true;
      open = false;
      package = config.boot.kernelPackages.nvidiaPackages.stable;
      powerManagement.enable = true;
      prime = {
        # offload.enable = true;
	# intelBusId = "";
	# nvidiaBusId = "";
      };
    };

    opengl = {
      driSupport = true;
      driSupport32Bit = true;
      enable = true;
    };

    pulseaudio = {
      enable = false;
      support32Bit = true;
    };
  };
  
  networking = {
    hostName = "midori-nixos";
    networkmanager.enable = true;
    useDHCP = lib.mkDefault true;
  };
  
  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";
  
  swapDevices = [ 
    { device = "/dev/disk/by-uuid/89e98719-eaea-4074-9fd0-bd67a806d7b2"; }
  ];

}
