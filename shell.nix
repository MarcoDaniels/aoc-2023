let
  pkgs = import (fetchTarball {
    name = "nixpkgs-23.05-darwin-2023-10-05";
    url = "https://github.com/NixOS/nixpkgs/archive/1e9c7c0203be.tar.gz";
    sha256 = "10qbybc9k3dj1xap9n0i3z7pc3svzwhclgsyfzzsf8cfh8l518pn";
  }) { };

  format = pkgs.writeScriptBin "format" ''
    ${pkgs.nodePackages.purty}/bin/purty --write src/Main.purs
  '';

  purescript = pkgs.stdenv.mkDerivation rec {
    pname = "purescript";
    version = "0.15.13";
    src = pkgs.fetchurl {
      url =
        "https://github.com/${pname}/${pname}/releases/download/v${version}/macos-arm64.tar.gz";
      sha256 = "sha256-+oIKEm8i37YT5OzxMtcG7ha2toc8yJ+31y9bDPZLsHE=";
    };
    buildInputs = [ pkgs.zlib pkgs.gmp ];
    libPath = pkgs.lib.makeLibraryPath buildInputs;
    dontStrip = true;
    installPhase = ''
      mkdir -p $out/bin
      PURS="$out/bin/purs"

      install -D -m555 -T purs $PURS

      mkdir -p $out/share/bash-completion/completions
      $PURS --bash-completion-script $PURS > $out/share/bash-completion/completions/purs-completion.bash
    '';
  };

in pkgs.mkShell rec {
  buildInputs = with pkgs; [
    spago
    esbuild
    nodejs
    nodePackages.purty
    ## scripts
    format
    purescript
  ];
}
