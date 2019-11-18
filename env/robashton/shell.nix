let
  ashtonPackages =
    builtins.fetchGit {
      name = "ashton-packages";
      url = https://github.com/robashton/nixpkgs-ashton/;
      rev = "727186c0968777c5c06cc648c61796481d1efa1b";
    };

  nixPackages =
    import <nixpkgs> {
      overlays = [
        (import ashtonPackages)
      ];
    };

in

with nixPackages;

stdenv.mkDerivation {
  name = "robashton.pure_unit";
  buildInputs = [
    pkgs.devPackages.erlang-22-0-1.erlang
    pkgs.devPackages.erlang-22-0-1.rebar3-11
    pkgs.devPackages.purerl-0-13-2
    pkgs.psc-package
    pkgs.stdenv
    pkgs.cmake
    
    pkgs.pcre.dev # for nginx, so...
    pkgs.openssl # also for nginx
    pkgs.zlib # also for nginx and also for media

    # for id3as_media
    pkgs.bzip2
    pkgs.alsaLib
    pkgs.ladspa-sdk
    pkgs.freetype
    pkgs.lzma

    libva-full
    libva-utils
    intel-media-sdk
    intel-gpu-tools
    libdrm

    # Crappy UI stuff
#    elmPackages.elm
#    elmPackages.elm-upgrade
#    elmPackages.elm-format
#    nodejs

   
    # Used by scripts
    killall

  ];
}

