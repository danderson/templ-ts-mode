# templ-ts-mode

Emacs major mode for editing [Templ](https://templ.guide) files.

## Installing

You need to make the [Templ tree-sitter
parser](https://github.com/vrischmann/tree-sitter-templ) available to
your emacs installation, as well as the javascript parser. The latter
is usually distributed alongside emacs in your distro, if js-ts-mode
works then you're fine.

If you're a Nix/NixOS user, this repository is a flake that packages
both the major mode and the tree-sitter grammar as a convenience,
until nixpkgs starts carrying them upstream. I recommend using the
[community emacs
overlay](https://github.com/nix-community/emacs-overlay) to manage
your emacs configs, and you can peek at [my emacs nix
configuration](https://github.com/danderson/homelab/blob/main/home/emacs2.nix)
for an example of how to add the parser and major mode to your
installation.

## Usage

The package automatically sets `templ-ts-mode` for `.templ`
files. Currently, font-locking and indentation work, but Imenu and
other creature comforts aren't done yet.

templ-ts-mode does not define its own customization variables. Rather,
it reuses settings from the languages that make up a Templ file:

 - Set your preferred indentation for the Go-ish portions of the file
   with `go-ts-mode-indent-offset`.
 - Set your preferred javascript indentation for `script` components
   with `js-indent-level`.

### Known issues

Indentation is slightly buggy on the first line of `script`
components: the point must be at or before the first character on the
line for indentation to do anything. This is due to a bug in Emacs
29.1 relating to multi-language indentation handling and is already
fixed in git, so the bug will go away with the next Emacs update.

CSS font-locking is currently freelanced, rather than delegating to
css-mode. This is because Templ's CSS components are tricksy and can
include embedded chunks of Go, which makes it difficult to delegate
parsing and font-locking to css-ts-mode without causing chaos.

There may be corners of the Templ language that I missed in my
testing, so indentation and font-locking may be imperfect. Patches are
welcome.
