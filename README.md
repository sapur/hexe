# hexe

`hexe` is a simple terminal-based hex editor.

![Screenshot](screenshot.png)

**Features:**

 * Unlimited undo and redo
 * Set bookmarks at any offset and jump between marks
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

You can also put this information (along with any other commands) in a file 
(`mbr.cmd`):

    mark 0   "bootloader entry point"
    mark 446 "partition 1"
    mark 462 "partition 2"
    mark 478 "partition 3"
    mark 494 "partition 4"
    mark 510 "magic 55AA"

And let `hexe` load it at startup:

    hexe --script mbr.cmd sector.bin

At startup, `hexe` reads `.config/hexe/config` automatically. It may contain 
any commands, for example:

    column-width =8
    set-mode char-over

Refer to `hexe --help` for a list of all command-line options.
