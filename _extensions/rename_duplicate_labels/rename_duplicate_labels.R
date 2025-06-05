process_files <- function(files) {
  # Split the files string into individual file paths
  files <- stringi::stri_replace_all_fixed(files, "\\", "/")
  filepaths <- unlist(stringi::stri_split_fixed(files, pattern = "\n"))

  # Initialize a named vector to keep track of label occurrences across all files
  label_counts <- setNames(numeric(), character())

  # Process each file
  lapply(filepaths, function(filepath) {
    # Read in the file
    lines <- readLines(file.path(filepath), warn = FALSE)

    # Process each line in the file
    lines_processed <- sapply(lines, function(line) {
      if (stringi::stri_detect_regex(line, pattern = "^#\\| label: '|^#+[[:space:]]+[^\\{]+\\{#sec-[^\\}]+\\}")) {
        # Extract the label
        label <- stringi::stri_replace_first_regex(line,
          pattern = "^#\\| label: '([^']+)'[[:space:]]*$|^#+[[:space:]]+[^\\{]+\\{(#sec-[^\\}]+)\\}",
          replacement = "$1$2"
        )
        if (is.na(label_counts[label])) label_counts[label] <<- 0
        # Increment the count for this label
        label_counts[label] <<- label_counts[label] + 1
        # If this label has been encountered before, append a digit to label
        if (label_counts[label] > 1) {
          print(paste0("Modifying: ", filepath))
          # Replace the last character (which is an apostrophe) with a digit and an apostrophe
          new_line <- stringi::stri_replace_last_regex(line,
            pattern = "(['\\}])",
            replacement = paste0(label_counts[label], "$1")
          )
        }
      }
      if (exists("new_line")) new_line else line
    }, USE.NAMES = FALSE)

    # Write the modified lines back to the file
    if (is.character(lines) && lines != lines_processed) writeLines(lines, filepath)
    filepath
  })

  message("Completed rename_duplicate_labels")
}


# Get the list of files from the environment variable
files <- Sys.getenv("QUARTO_PROJECT_INPUT_FILES")

# Call process_files with the list of files
if (length(files)) process_files(files)
