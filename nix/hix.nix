{pkgs, ...}: {
  # name = "project-name";
  compiler-nix-name = "ghc8107"; # Version of GHC to use

  # crossPlatforms = p: pkgs.lib.optionals pkgs.stdenv.hostPlatform.isx86_64 ([
  #   p.mingwW64
  #   # p.ghcjs # TODO GHCJS support for GHC 9.2
  # ] ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isLinux [
  #   p.musl64
  # ]);

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "3.4.1";
  shell.tools.haskell-language-server = { version = "1.8.0.0"; index-state = "2022-12-17T00:00:00Z"; };

  shell.buildInputs = with pkgs; [zlib zlib.dev zlib.out];
}
