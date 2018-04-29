[![Build Status](https://travis-ci.org/akirak/counsel-org-clock.svg?branch=master)](https://travis-ci.org/akirak/counsel-org-clock)
[![MELPA](http://melpa.milkbox.net/packages/counsel-org-clock-badge.svg)](http://melpa.milkbox.net/#/counsel-org-clock)

# counsel-org-clock

Counsel-org-clock provides commands for displaying org clock entries via [Counsel (Ivy)](https://github.com/abo-abo/swiper) interface.

## Features

There are currently the following two commands:

- When you are clocking in a task, `counsel-org-clock-context` displays the task, its ancestors, and its descendants via Ivy. When not clocking in, this function behaves the same as `counsel-org-clock-history`. 
- `counsel-org-clock-history` displays entries in `org-clock-history` variable via Ivy.
  - With a prefix argument, `counsel-org-clock-history` rebuilds the history from clock entries in `org-agenda-files`.

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

## Installation

This package is available on MELPA as `counsel-org-clock`.

## Usage

Run `counsel-org-clock-context` or `counsel-org-clock-history`. By default, these functions jump to a selected headline. You can change the default action by setting `counsel-org-clock-default-action` variable. 

You can also access a bunch of alternative actions from `M-o`, including:

- Narrow the buffer to the selected entry or show it in an indirect buffer
- Change the todo state
- Set tags
- Set a property
- Clock in/out
- Store a link

If you run `counsel-org-clock-history` with a prefix argument, it reads clock entries in `org-agenda-files` and rebuilds `org-clock-history` variable. 

## Alternatives

- [org-mru-clock](https://github.com/unhammer/org-mru-clock)
  - [Comparison between counsel-org-clock and org-mru-clock by Marcin Borkowski](http://mbork.pl/2018-04-28_org-mru-clock)

## Thanks

- [mbork](https://github.com/mbork) ([#1](https://github.com/akirak/counsel-org-clock/pull/1))

## License

GPL v3
