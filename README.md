[![Build Status](https://travis-ci.org/akirak/counsel-org-clock.svg?branch=master)](https://travis-ci.org/akirak/counsel-org-clock)
[![MELPA](http://melpa.milkbox.net/packages/counsel-org-clock-badge.svg)](http://melpa.milkbox.net/#/counsel-org-clock)

# counsel-org-clock

Counsel Org Clock provides commands for displaying org clock entries via [Counsel (Ivy)](https://github.com/abo-abo/swiper) interface.

## Features

This package contains the following two commands:

- When you are clocking in a task, `counsel-org-clock-context` displays the task, its ancestors, and its descendants via Ivy. When not clocking in, this function behaves the same as `counsel-org-clock-history`. 
- `counsel-org-clock-history` displays entries in `org-clock-history` variable via Ivy.
  - With a prefix argument, `counsel-org-clock-history` rebuilds the history from clock entries in `org-agenda-files` before displaying it.
- `counsel-org-clock-goto` command is a replacement for `org-clock-goto` which lets you navigate to the active clock and through the clock history.

## Screenshots

`counsel-org-clock-context`:

![counsel-org-clock-context](https://akirak.keybase.pub/Screenshots/counsel-org-clock/counsel-org-clock-context.png)

`counsel-org-clock-history`:

![counsel-org-clock-history](https://akirak.keybase.pub/Screenshots/counsel-org-clock/counsel-org-clock-history.png)

Actions for the selected headline (`M-o`):

![actions](https://akirak.keybase.pub/Screenshots/counsel-org-clock/counsel-org-clock-commands.png)

## Prerequisites

- Emacs 24.3 (with Org)
- Ivy
- dash.el

## Installation

This package is available on MELPA as `counsel-org-clock`.

## Usage

### counsel-org-clock-context and counsel-org-clock-history

Run `counsel-org-clock-context` or `counsel-org-clock-history`. By default, these functions jump to a selected headline. You can change the default action by setting `counsel-org-clock-default-action` variable. 

You can also access a bunch of alternative actions from `M-o`, including:

- Narrow the buffer to the selected entry or show it in an indirect buffer
- Change the todo state
- Set tags
- Set a property
- Clock in/out
- Store a link

If you run `counsel-org-clock-history` with a prefix argument, it reads clock entries in `org-agenda-files` and rebuilds `org-clock-history` variable before displaying the history contents. 

### counsel-org-clock-goto

There is also `counsel-org-clock-goto` command. It behaves as follows:

- Without a prefix argument, this command lets you jump to the active clock. 
- With a universal prefix argument (`C-u`), it calls `counsel-org-clock-context`.
- With two universal prefix arguments (`C-u C-u`), it runs `counsel-org-clock-history`, which lets you browse your clock history. 
- With three universal prefix arguments (`C-u C-u C-u`), it runs `counsel-org-clock-history` with an argument. That is, it lets you browse the clock history after rebuilding it from `org-agenda-files`.

You can also customize what this command does when there is no active clock.
If you set `counsel-org-clock-goto-fallback-function` to a function, the command calls the function when there is no active clock.

I bind `M-g M-j` to this command:

``` emacs-lisp
(global-set-key (kbd "M-g M-j") #'counsel-org-clock-goto)
```

## Alternatives

- [org-mru-clock](https://github.com/unhammer/org-mru-clock)
  - [Comparison between counsel-org-clock and org-mru-clock by Marcin Borkowski](http://mbork.pl/2018-04-28_org-mru-clock)
    - Counsel Org Clock now rebuilds the history when you run `counsel-org-clock-history`/`counsel-org-clock-context` with a prefix argument!

## Thanks

- [mbork](https://github.com/mbork) ([#1](https://github.com/akirak/counsel-org-clock/pull/1))
- [rememberYou](https://github.com/rememberYou) ([#3](https://github.com/akirak/counsel-org-clock/pull/3))

## License

GPL v3
