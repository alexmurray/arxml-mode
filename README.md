# Adaptive AUTOSAR arxml editing mode for Emacs

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](http://melpa.org/packages/arxml-mode-badge.svg)](http://melpa.org/#/arxml-mode)
[![Build Status](https://travis-ci.org/alexmurray/arxml-mode.svg?branch=master)](https://travis-ci.org/alexmurray/arxml-mode)

Provides an enhanced editing environment for AUTOSAR arxml files within Emacs, including:

* [nxml RNG](https://www.gnu.org/software/emacs/manual/html_node/nxml-mode/Introduction.html) integration
  for validity checking and XML tag completion
* [xref](https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref.html)
  integration for SHORT-NAME definitions and references
* Context sensitive reference completion
  via
  [completion-at-point](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html)
* [Flycheck](http://www.flycheck.org/) integration for enhanced validation
* [Company mode](https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-in-Buffers.html) integration
  for completions showing definitions of candidates and documentation
* Simple [yasnippet](http://joaotavora.github.io/yasnippet/) integration for
  frequently used constructs
* [all-the-icons.el](https://github.com/domtronn/all-the-icons.el) integration
  for pretty mode-line

Aims to provide features on-par with Vector's [vArxmlEditor](https://vector.com/vi_autosar_news_detail_en,1371902,,1699880,detail.html)

## Installation

### MELPA (coming soon...)

The preferred way to install `arxml-mode` is via
[MELPA](http://melpa.org) - then you can just <kbd>M-x package-install RET
arxml-mode RET</kbd>

To enable then simply add the following to your init file:

```emacs-lisp
(require 'arxml-mode)
```

### Manual

If you would like to install the package manually, download or clone it and
place within Emacs' `load-path`, then you can require it in your init file like
this:

```emacs-lisp
(require 'arxml-mode)
```

## License

Copyright © 2017 Alex Murray

Distributed under GNU GPL, version 3.
