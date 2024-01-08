# templ-ts-mode

Emacs major mode for editing [Templ](https://templ.guide) files.

## Installing

templ-ts-mode is [available in
MELPA](https://melpa.org/#/templ-ts-mode). Install it however you
usually install your emacs packages from MELPA!

The mode does require two tree-sitter grammars: the Javascript
grammar, which should have shipped with your Emacs installation; and
the [Templ grammar](https://github.com/vrischmann/tree-sitter-templ),
which probably hasn't.

By default, if the Templ grammar is missing, templ-ts-mode will ask if
you want to download and build it using Emacs' built-in
`treesit-install-language-grammar` machinery. If you want to do this,
you need to have git and a C compiler installed. There are Customize
options to set the source repository for the grammar, and to either
always or never install the grammar if it's missing.

If you're a Nix/NixOS user, this repository is a flake that packages
both the major mode and the tree-sitter grammar as a convenience. I
recommend using the [community emacs
overlay](https://github.com/nix-community/emacs-overlay) to manage
your emacs configs, and you can peek at [my emacs nix
configuration](https://github.com/danderson/homelab/blob/main/home/emacs2.nix)
for an example of how to add the parser and major mode to your
installation.

## Usage

The package automatically sets `templ-ts-mode` for `.templ`
files. Currently, font-locking and indentation work, but Imenu and
other creature comforts aren't done yet.

templ-ts-mode does not define its own customization variables for
indentation. Instead, it piggybacks on the settings for the languages
that make up a Templ file:

 - Go code and the basic skeleton of Templ files are indented
   according to `go-ts-mode-indent-offset`.
 - Javascript code in `script` functions is indented according to
   `js-indent-level`.

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
