[![Build Status](https://travis-ci.org/akirak/counsel-org-clock.svg?branch=master)](https://travis-ci.org/akirak/counsel-org-clock)

# counsel-org-clock

Counsel-org-clock provides commands for displaying org clock entries via [Counsel (Ivy)](https://github.com/abo-abo/swiper) interface.

## Features

There are currently the following two commands:

- When you are clocking in a task, `counsel-org-clock-context` displays the task, its ancestors, and its descendants via Ivy. When not clocking in, this function behaves the same as `counsel-org-clock-history`. 
- `counsel-org-clock-history` displays entries in `org-clock-history` variable via Ivy.

## Screenshots

`counsel-org-clock-context`:

![counsel-org-clock-context](https://akirak.keybase.pub/Screenshots/counsel-org-clock/counsel-org-clock-context.png)

`counsel-org-clock-history`:

![counsel-org-clock-history](https://akirak.keybase.pub/Screenshots/counsel-org-clock/counsel-org-clock-history.png)

Actions for the selected headline (`M-o`):

![actions](https://akirak.keybase.pub/Screenshots/counsel-org-clock/counsel-org-clock-commands.png)

## Prerequisites

- Emacs 24.1 (with Org)
- Ivy

## Installation

This package is not yet available on MELPA. Use Quelpa or something to install the package from this repository.

## Usage

Run `counsel-org-clock-context` or `counsel-org-clock-history`. By default, these functions jump to a selected headline. You can change the default action by setting `counsel-org-clock-default-action` variable. 

You can also access a bunch of alternative actions from `M-o`, including:

- Narrow the buffer to the selected entry or show it in an indirect buffer
- Change the todo state
- Set tags
- Set a property
- Clock in/out
- Store a link

## Thanks

- [mbork](https://github.com/mbork) ([#1](https://github.com/akirak/counsel-org-clock/pull/1))

## License

GPL v3
