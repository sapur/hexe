# hexe

`hexe` is a simple terminal-based hex editor.

![Screenshot](screenshot.png)

**Features:**

 * Unlimited undo and redo
 * Set marks at any offset and jump between marks
 * Freely adjustable column width

**Features hexe does NOT have:**

 * Changeable key mappings (almost supported)
 * Unlimited file size
 * Multiple buffers

## Usage

`hexe` is always invoked on a single file:

    hexe <filename>

The following command will print a table of all keys and functions:

    hexe --list-bindings

To bookmark locations from the command line, call `hexe` like this:

    hexe -M 0=entry -M 446=part1 -M 462=part2 -M 478=part3 -M 494=part4 -M 510=magic sector.bin

Refer to `hexe --help` for a list of all options.

`hexe` reads `.config/hexe/config` (on Linux) at startup. It may contain any 
commands, for example:

    column-width =8
    set-mode char-over
