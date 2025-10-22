# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];
  boot.loader.grub.enable = true;
  boot.loader.grub.device = "/dev/sda";
  boot.loader.grub.useOSProber = true;
  networking.hostName = "nicklasdev";
  networking.networkmanager.enable = true;
  time.timeZone = "Europe/Stockholm";
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.extraLocaleSettings = {
    LC_ADDRESS = "sv_SE.UTF-8";
    LC_IDENTIFICATION = "sv_SE.UTF-8";
    LC_MEASUREMENT = "sv_SE.UTF-8";
    LC_MONETARY = "sv_SE.UTF-8";
    LC_NAME = "sv_SE.UTF-8";
    LC_NUMERIC = "sv_SE.UTF-8";
    LC_PAPER = "sv_SE.UTF-8";
    LC_TELEPHONE = "sv_SE.UTF-8";
    LC_TIME = "sv_SE.UTF-8";
  };
  services.xserver.enable = true;
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
  #services.xserver.displayManager.default
  #services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome.enable = true;
  services.xserver.xkb = {
    layout = "se";
    variant = "";
  };
  console.keyMap = "sv-latin1";
#  services.printing.enable = true;
  services.printing = {
    enable = true;
    drivers = with pkgs; [
      gutenprint   # generic drivers
      cups-bjnp    # Canon network backend
      hplip        # doesn’t hurt, covers some PCL printers
    ];
  };
  hardware.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
  };
  users.users.nicklas = {
    isNormalUser = true;
    description = "nicklas";
    hashedPassword = "$6$e6GlX5UE4LWJc151$gk3Lj/nGKkqRlBcXuT/eacIQ48EpvYsGlTd1dxcDCtkG0QQPzia2FbVCAHMmDkwies60r4O64MfR1ZpoOIM7e.";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
       firefox
    ];
  };
  # Enables printers
  services.avahi = {
    enable = true;
    nssmdns4 = true;
    openFirewall = true;
  };
  programs.firefox.enable = true;
  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    bc
    dmenu
    gedit
    git
    screen
    tree
    vim
    vlc
    wget	
    xmobar
  ];
  services.openssh.enable = true;
  system.stateVersion = "24.11"; # Did you read the comment?
}
