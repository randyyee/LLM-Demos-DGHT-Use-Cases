# Load required libraries
library(stringr)

# Set the directory containing the subfolders
main_dir <- "C:/Users/pcx5/OneDrive - CDC/PEPFAR Docs/COP_ROP"

# Set the directory for the renamed copies
clean_dir <- file.path(main_dir, "Clean")


# Check if the "Clean" directory exists and delete its contents if it does
if (dir.exists(clean_dir)) {
  # List all files in the "Clean" directory
  clean_files <- list.files(clean_dir, full.names = TRUE)
  
  # Delete all files in the "Clean" directory
  file.remove(clean_files)
} else {
  # Create the "Clean" directory if it doesn't exist
  dir.create(clean_dir)
}


# Define a wordlist of words to remove from filenames
wordlist <- c("Compliant",
              "ROP23",
              "v2",
              "v3",
              "COP",
              "COP19",
              "ROP",
              "508",
              "2017",
              "2018",
              "2019",
              "2020",
              "2023",
              "2024",
              "Public",
              "public",
              "Feb",
              "Aug",
              "Sep",
              "01",
              "02",
              "08",
              "09",
              "15",
              "19",
              "11th",
              "11",
              "10th",
              "10",
              "2021",
              "13",
              "30",
              "20",
              "21",
              "22",
              "23",
              "24",
              "PLL",
              "Approval", 
              "SDS",
              "SID",
              "Strategic",
              "Directional",
              "directional",
              "Summary",
              "FINAL", "Final", "final", "with", 
              "Approval", "Memo", 
              "Signed",
              "signed",
              "Compliant", 
              "compliant",
              "COMPLAINT",
              "of", "Planning", "Plannig", "Letter",
              "revised",
              "English",
              "Redactions",
              "approved"
              )  # Add the words you want to remove

# Get a list of all subfolders
subfolders <- list.dirs(main_dir, full.names = TRUE, recursive = FALSE)

# Function to clean the filename
clean_filename <- function(filename, wordlist) {
  
  # Remove words from the wordlist
  for (word in wordlist) {
    # filename <- str_remove_all(filename, paste0("\\b", word, "\\b"))
    # filename <- str_remove_all(filename, paste0("\\B", word, "\\B"))
    # filename <- str_remove_all(filename, paste0("\\b", word, "\\b"))
    filename <- str_remove_all(filename, fixed(word))
  }
  
  # Remove underscores and dashes
  filename <- str_remove_all(filename, "[-_]")
  
  # Remove any leading or trailing whitespace and periods
  filename <- str_trim(filename)
  filename <- str_remove_all(filename, "^\\.|\\.$")
  
  filename <- toupper(filename)
  
  # Replace "Part1" with "PartA" and "Part2" with "PartB"
  filename <- str_replace_all(filename, "PART1", "_PartA")
  filename <- str_replace_all(filename, "PART2", "_PartB")
  
  filename <- str_replace_all(filename, "DEMOCRATICRETHECONGO", "DEMOCRATICREPUBLICTHECONGO")
  filename <- str_replace_all(filename, "DOMINICANRE", "DOMINICANREPUBLIC")
  
  filename <- str_replace_all(filename, "DRC", "DEMOCRATICREPUBLICTHECONGO")
  filename <- str_replace_all(filename, "DR", "DOMINICANREPUBLIC")
  
  filename <- str_replace_all(filename, "CDI", "COTEDIVOIRE")
  
  filename <- str_replace_all(filename, "\\.", "")
  
  
  # Remove numbers 3-9
  filename <- str_remove_all(filename, "[0-9]")
  
  return(filename)
}

# Loop through each subfolder
for (subfolder in subfolders) {
  # Get the subfolder name
  subfolder_name <- basename(subfolder)
  
  # Get a list of all PDF files in the current subfolder
  pdf_files <- list.files(subfolder, pattern = "\\.pdf$", full.names = TRUE)
  
  # Loop through each PDF file
  for (pdf_file in pdf_files) {
    # Extract the current filename without the directory path
    current_filename <- basename(pdf_file)
    
    # Remove ".pdf" to clean the filename
    filename_without_extension <- str_remove(current_filename, "\\.pdf$")
    
    # Clean the filename
    cleaned_filename <- clean_filename(filename_without_extension, wordlist)
    
    # Create the new filename by appending the subfolder name
    new_filename <- paste0(cleaned_filename, "-", subfolder_name, ".pdf")
    
    # Create the full path for the new file in the "Clean" directory
    new_filepath <- file.path(clean_dir, new_filename)
    
    # Copy and rename the file
    file.copy(pdf_file, new_filepath)
  }
}

cat("PDFs copied, cleaned, and renamed successfully!")
list.files(clean_dir)
