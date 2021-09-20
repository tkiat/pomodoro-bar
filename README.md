# pomodoro-bar

A feature-rich CLI-based Pomodoro clock with optional integration with external displays: polybar and xmobar. For a single Python script alternative, see [pomodoro-bar-py](https://github.com/tkiat/pomodoro-bar-py).

## Samples

```
CLI Display                                   Bar Display / Color

N/A                                           POMODORO / Yellow
[w]-b-w-b-w-b-w-l 25:00 - [s]tart or [q]uit   [1]START / Yellow
[w]-b-w-b-w-b-w-l 23:45 - CTRL+c to Pause     [1]23:45 / Green
[w]-b-w-b-w-b-w-l 23:34 - [s]tart or [q]uit   [1]PAUSE / Yellow
w-[b]-w-b-w-b-w-l 05:00 - [s]tart or [q]uit   [1]BREAK / Yellow
w-[b]-w-b-w-b-w-l 04:32 - CTRL+c to Skip      [1]04:32 / Yellow
w-b-[w]-b-w-b-w-l 12:34 - CTRL+c to Pause     [2]12:34 / Green
```

```bash
$ pomodoro-bar --record -w 25 -n 3
Number of 25-minute sessions from this week (top)
 Mon   Tue   Wed   Thu   Fri  Sat   Sun   Avg
 ---   ---   ---   ---   ---  ---   ---   ---
 8.0  10.0   8.0  10.0   6.0  8.0         8.3
10.0  12.0  10.0  16.0  10.0  8.0  10.0  10.9
14.0   6.0  10.0  12.0  10.0  8.0  10.0  10.0
```

## Features

- Pausable
- Configurable session lengths
- View summary in both raw and pretty formats
- Execute any system command when a session ends
- Integration with an external bar (currently polybar and xmobar) with alternate colors between work and break sessions

## Prerequisites

- Linux (I don't have OS X or Windows to test)
- Nix to install this program (optional but recommended)
- System command(s) to notify you when session ends like `xset` or `espeak` (optional but recommended)

## Installation

1. Using Nix
    ```bash
    $ git clone https://github.com/tkiat/pomodoro-bar.git
    $ cd pomodoro-bar/
    $ nix-build --attr exe
    $ nix-env -i ./result
    ```
1. Using Nix Flakes (experimental)
    ```bash
    $ nix build github:tkiat/pomodoro-bar --out-link pomodoro-bar-result
    $ nix-env -i ./pomodoro-bar-result
    ```

## Usage

### General

```bash
$ pomodoro-bar -h
$ pomodoro-bar -w 25 -b 5 -l 15 --cmdwork "xset dpms force off" --cmdbreak "xset dpms force off" --bartype xmobar
$ pomodoro-bar --record -w 25
```

## xmobar integration (optional)

### Make named pipes on start (optional but recommended)

You need to compile xmobar after making FIFOs to make it work. To prevent manual compilation, you need to create two named pipes before starting your window manager i.e. put something like this in ~/.xinitrc, ~/.xsessionrc, or equivalent.

```bash
$ pomodoro_bar_w=/tmp/.pomodoro-bar-w
$ pomodoro_bar_i=/tmp/.pomodoro-bar-i
$ rm -f $pomodoro_bar_w && mkfifo $pomodoro_bar_w
$ rm -f $pomodoro_bar_i && mkfifo $pomodoro_bar_i
```

This will create two named pipes that will act as a rendezvous for IPC between this application and xmobar.

### Link named pipes in your xmobar config

```haskell
Config {
  ...
  template = "<fc=green>%pomodoro-bar-w%</fc><fc=yellow>%pomodoro-bar-i%</fc>",
  commands = [
    Run PipeReader "P:/tmp/.pomodoro-bar-w"       "pomodoro-bar-w"
    Run PipeReader "OMODORO:/tmp/.pomodoro-bar-i" "pomodoro-bar-i",
  ]
  ...
}
```

You may recompile xmobar or restart the window manager after this setup.

## Polybar integration (optional)

### Make named pipes on start (optional but recommended)

Same as described in the xmobar section

#### Link named pipes in your polybar config

Declare two modules.

```ini
[module/working]
type = custom/script
exec = tail -F /tmp/.pomodoro-bar-w 2>/dev/null
exec-if = [ -p /tmp/.pomodoro-bar-w ]
tail = true
format-foreground = #00ff00

[module/resting]
type = custom/script
exec = echo POMODORO && tail -F /tmp/.pomodoro-bar-i 2>/dev/null
exec-if = [ -p /tmp/.pomodoro-bar-i ]
tail = true
format-foreground = #ffff00
```

And add these modules to your bar.

```ini
[bar/whatever]
modules-right = whatever working resting whatever
```

You may recompile polybar or restart the window manager after this setup.