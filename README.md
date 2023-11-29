# rename_duplicate_labels Extension For Quarto

For a given project render, rename any duplicate chunk labels (and references to these) across a file, and across files (e.g. chapters).

## Installing

```bash
quarto add NIFU-NO/rename_duplicate_labels
```

This will install the extension under the `_extensions` subdirectory.
If you're using version control, you will want to check in this directory.

## Using

```yaml
project:
  pre-render:
    - "_extensions/rename_duplicate_labels.R"
```

- can also be used manually of course.

## Example

Here is the source code for a minimal example: [example.qmd](example.qmd).

