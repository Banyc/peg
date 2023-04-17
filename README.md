# PEG

## `peggrep`

Example file `demo_file`:

```text
THIS LINE IS THE 1ST UPPER CASE LINE IN THIS FILE.
this line is the 1st lower case line in this file.
This Line Has All Its First Character Of The Word With Upper Case.

Two lines above this line is empty.
And this is the last line.
```

### Match literals

```bash
$ peggrep "'this'" demo_file
this line is the 1st lower case line in this file.
Two lines above this line is empty.
And this is the last line.
```

- `'this'`: match the literal string "this"

### Match `this.*empty`

```bash
$ peggrep "'this' (!'empty' .)* 'empty'" demo_file
Two lines above this line is empty.
```

- `!'empty'`: true if the current position is not followed by "empty"
- `.`: consume one character unconditionally
- `(!'empty' .)*`:
  - def: match any number of characters that are not followed by "empty"
  - when the position is at the `e` of "empty", the `(!'empty' .)*` exits
- The final `'empty'`: match and consume the "empty"

To make life easier, we can make `(!'empty' .)* 'empty'` into a function:

1. Write a grammar file:
   ```py
   // grammar.peg
   until[str] <- (!str .)* str
               ;
   ```
1. Run with the grammar file:
   ```bash
   $ peggrep -g grammar.peg "'this' until['empty']" demo_file
   Two lines above this line is empty.
   ```

### Match digits

Steps:

1. Write a grammar file:
   ```py
   // grammar.peg
   digit <- '0' / '1' / '2' / '3' / '4' / '5' / '6' / '7' / '8' / '9'
          ;
   ```
1. Run with the grammar file:
   ```bash
   $ peggrep -g grammar.peg "digit+" demo_file
   THIS LINE IS THE 1ST UPPER CASE LINE IN THIS FILE.
   this line is the 1st lower case line in this file.
   ```

### Grammar file from environment variable

We don't want to write a grammar file every time we want to use it. We can use the environment variable `PEGGREP_GRAMMAR` to specify the grammar file:

```bash
$ export PEGGREP_GRAMMAR="/absolute/path/to/grammar.peg"
$ peggrep "'this' until['empty']" demo_file
Two lines above this line is empty.
```
