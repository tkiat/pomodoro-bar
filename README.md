# pomodoro-bar

A CLI pomorodo clock based on Haskell and nix. I made this because I couldn't find any Pomodoro Timer with these exact features. Visit [pomodoro-bar-py](https://github.com/tkiat/pomodoro-bar-py) for the equivalent single Python script alternative.

## Features

- Pausable
- Configurable session lengths
- Report simple stats in both raw and pretty formats
- Choose any system command to execute when sessions end
- Integration with polybar and xmobar with alternate colors between work and break sessions

## Samples

```
CLI Display                                   Bar Display / Color

N/A                                           POMODORO / Yellow
[w]-b-w-b-w-b-w-l 25:00 - [s]tart or [q]uit   [1]START / Yellow
[w]-b-w-b-w-b-w-l 23:45 - CTRL+c to Pause     [1]23:45 / Green
[w]-b-w-b-w-b-w-l 23:34 - [s]tart or [q]uit   [P]23:34 / Yellow
w-[b]-w-b-w-b-w-l 05:00 - [s]tart or [q]uit   [2]BREAK / Yellow
w-[b]-w-b-w-b-w-l 04:32 - CTRL+c to Skip      [2]04:32 / Yellow
w-b-[w]-b-w-b-w-l 12:34 - CTRL+c to Pause     [3]12:34 / Green
```

## Prerequisites

- Linux (I have't tested on OSX, I don't think it works on Windows)
- nix to install this app (optional but recommended)
- System command(s) to notify you when session ends like `xset` or `espeak` (optional but recommended)

## Installation

### Using nix

```bash
cd pomodoro-bar/
nix-build --attr exe
nix-env -i ./result
```

## Usage

### General

```bash
pomodoro-bar -h
echo "Example command"
pomodoro-bar -w 25 -b 5 -l 15 --cmdwork "xset dpms force off" --cmdbreak "xset dpms force off" --xmobar
echo "View record"
pomodoro-bar --record -w 25
```

## xmobar integration (optional)

### Make named pipes on start (optional but recommended)

You need to compile xmobar after making FIFOs to make it work. To prevent manual compilation, you need to create two named pipes before starting your window manager. In general, you can put something like this in ~/.xinitrc before executing your window manager.

```bash
pomodoro_bar_w=/tmp/.pomodoro-bar-w
pomodoro_bar_i=/tmp/.pomodoro-bar-i
rm -f $pomodoro_bar_w && mkfifo $pomodoro_bar_w
rm -f $pomodoro_bar_i && mkfifo $pomodoro_bar_i
```

This will create two named pipes that will act as a rendezvous for IPC between this application and xmobar.

### Link named pipes in your xmobar config

```haskell
Config {
  template = "<fc=green>%pomodoro-bar-w%</fc><fc=yellow>%pomodoro-bar-i%</fc>",
  commands = [
    Run PipeReader "P:/tmp/.pomodoro-bar-w"       "pomodoro-bar-w"
    Run PipeReader "OMODORO:/tmp/.pomodoro-bar-i" "pomodoro-bar-i",
  ]
}
```

After this initial setup, you can restart the window manager or simply recompile xmobar.

## Polybar integration (optional)

### Make named pipes on start (optional but recommended)

Same as xmobar section

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

After this initial setup, you can restart the window manager or simply recompile polybar.
