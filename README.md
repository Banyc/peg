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

### Match `lines.*empty`

```text
$ peggrep "'lines' ('empty' / .)*" demo_file
Two lines above this line is empty.
```
