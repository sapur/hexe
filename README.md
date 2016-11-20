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
 * Changeable key mappings

## Usage

    hexe --help
    hexe <filename>

## Key mapping

| Key                                                       | Function                                 |
| ---                                                       | ---                                      |
| **General**                                               |                                          |
| `<Esc>`, `q`                                              | quit                                     |
| `<Ctrl-s>`, `w`                                           | save                                     |
| **Navigation**                                            |                                          |
| `<Left>`, `<Right>`, `<Up>`, `<Down>`, `h`, `l`, `k`, `j` | navigate                                 |
| `<Home>`, `<End>`, `^`, `$`                               | move to beginning/end of display line    |
| `<Ctrl-Home>`, `<Ctrl-End>`, `g`, `G`                     | move to beginning/end of file            |
| `<PageUp>`, `<PageDown>`                                  | move page up/down                        |
| `<Ctrl-Up>`, `<Ctrl-Down>`                                | scroll line up/down
| `H`, `M`, `L`                                             | move to 10%/50%/90% of displayed region  |
| `<Ctrl-g>`                                                | jump to given offset                     |
| **Adjust view port**                                      |                                          |
| `<`, `>`                                                  | increase/decrease column width           |
| `t`, `<S-t>`                                              | switch to 16/256 color mode              |
| **Bookmarks**                                             |                                          |
| `<Space>`, `m`                                            | toggle bookmark                          |
| `<Ctrl-KLeft>`, `<Ctrl-KRight>`, `N`, `n`                 | jump to previous/next bookmark           |
| **Edit**                                                  |                                          |
| `<Del>`, `x`                                              | delete    byte                           |
| `u`,     `<Ctrl-r>`                                       | undo/redo                                |
| any key                                                   | overwrite byte                           |
| **Modes**                                                 |                                          |
| `<Tab>`                                                   | switch between byte and character column |
| `<Ins>`                                                   | switch between overwrite and insert mode |
| `<Esc>`                                                   | return to overwrite/byte mode            |
