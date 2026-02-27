# muninn

Muninn is an Odin linter and formatter.

## Usage

```sh
muninn fmt path/to/file.odin
muninn fmt path/to/dir/

muninn fmt --dry-run path/to/file.odin

muninn fmt --check path/to/dir/

muninn fmt --diff path/to/file.odin
```

## Configuration

The formatter searches for `muninn.toml` starting from the formatted file's directory, walking up to the filesystem root.

```toml
[fmt]
indent_style = "tabs"
indent_width = 4
line_width = 100
newline_limit = 2
```

| Key | Default | Description |
|---|---|---|
| `indent_style` | `"spaces"` | `"tabs"` or `"spaces"` |
| `indent_width` | `4` | Number of spaces per indent level (when using spaces) |
| `line_width` | `100` | Target line width  |
| `newline_limit` | `2` | Maximum consecutive newlines allowed |


## Disable formatting

Disable formatting for a region of code with comments:

```odin
//muninn:fmt off
dont_format_this :: proc() {
    // ...
}
//muninn:fmt on
```

## License

This project is licensed under the terms of the [MIT](./LICENSE).
