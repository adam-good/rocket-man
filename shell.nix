{ pkgs ? import <nixpkgs> {}} :
pkgs.mkShellNoCC {
    packages = with pkgs; [
        clojure
        clojure-lsp
    ];
}
