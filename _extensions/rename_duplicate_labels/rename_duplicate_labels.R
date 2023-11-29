process_files <- function(files) {
  # Split the files string into individual file paths
  files <-  stringi::stri_replace_all_fixed(files, "\\", "/")
  filepaths <- unlist(stringi::stri_split_fixed(files, pattern="\n"))
  
  # Initialize a named vector to keep track of label occurrences across all files
  label_counts <- setNames(numeric(), character())
  
  # Process each file
  lapply(filepaths, function(filepath) {
    # Read in the file
    lines <- readLines(file.path(filepath), warn = FALSE)
    
    # Process each line in the file
    new_lines <- sapply(lines, function(line) {
      if (stringi::stri_detect_regex(line, pattern = "^#\\| label: '|^#+[[:space:]]+[^\\{]+\\{#sec-[^\\}]+\\}")) {
        # Extract the label
        label <- stringi::stri_replace_first_regex(line, 
                                                   pattern = "^#\\| label: '([^']+)'[[:space:]]*$|^#+[[:space:]]+[^\\{]+\\{(#sec-[^\\}]+)\\}", 
                                                   replacement = "$1$2")
        if (is.na(label_counts[label])) label_counts[label] <<- 0
        # Increment the count for this label
        label_counts[label] <<- label_counts[label] + 1
        # If this label has been encountered before, append a digit
        if (label_counts[label] > 1) {
          # Replace the last character (which is an apostrophe) with a digit and an apostrophe
          line <- stringi::stri_replace_last_regex(line, 
                                                   pattern = "(['\\}])", 
                                                   replacement = paste0(label_counts[label], "$1"))
        }
      }
      return(line)
    }, USE.NAMES = FALSE)
    
    # Write the modified lines back to the file
    writeLines(new_lines, filepath)
  })
  
  message("Completed rename_duplicate_labels")
}


# Get the list of files from the environment variable
files <- Sys.getenv('QUARTO_PROJECT_INPUT_FILES')

# Call process_files with the list of files
process_files(files)
