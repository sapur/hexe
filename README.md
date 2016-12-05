# hexe

`hexe` is a simple terminal-based hex editor.

![Screenshot](screenshot.png)

**Features:**

 * Unlimited undo and redo
 * Set marks at any offset and jump between marks
 * Freely adjustable column width

**Features hexe does NOT have:**

 * Unlimited file size
 * Multiple buffers
 * Changeable key mappings (not yet)

## Usage

`hexe` is always invoked on a single file:

    hexe --help
    hexe <filename>

The following command will print a table of all keys and functions:

    hexe --list-bindings
