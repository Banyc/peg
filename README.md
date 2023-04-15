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

```text
$ peggrep "'this'" demo_file
this line is the 1st lower case line in this file.
Two lines above this line is empty.
And this is the last line.
```

- `'this'`: match the literal string "this"

### Match `lines.*empty`

```text
$ peggrep "'lines' (!'empty' .)*" demo_file
Two lines above this line is empty.
```

- `!'empty'`: true if the current position is not followed by "empty"
- `.`: consume one character unconditionally
- `(!'empty' .)*`:
  - def: match any number of characters that are not followed by "empty"
  - when the position is at the `e` of "empty", the `(!'empty' .)*` exits
    - after the exit, there are no more characters are required to match, so the whole pattern matches successfully
