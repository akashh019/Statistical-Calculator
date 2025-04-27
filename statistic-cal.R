# Taking input
numbers_input <- readline(prompt = "Enter numbers separated by spaces: ")

# Check if input is empty
if (nchar(numbers_input) == 0) {
  cat("No numbers entered. Program exiting.\n")
} else {
  numbers <- as.numeric(unlist(strsplit(numbers_input, " ")))
  
  # Menu
  cat("\nWhat do you want to calculate?\n")
  cat("1. Mean\n")
  cat("2. Median\n")
  cat("3. Mode\n")
  cat("4. Standard Deviation\n")
  cat("5. Show all statistics\n")
  
  # User choice
  choice_input <- readline(prompt = "Enter your choice (1-5): ")
  
  if (nchar(choice_input) == 0) {
    cat("No choice entered. Program exiting.\n")
  } else {
    choice <- as.integer(choice_input)
    
    # Mode function
    get_mode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }
    
    # Show calculations based on choice
    if (choice == 1) {
      cat("Mean =", mean(numbers), "\n")
    } else if (choice == 2) {
      cat("Median =", median(numbers), "\n")
    } else if (choice == 3) {
      cat("Mode =", get_mode(numbers), "\n")
    } else if (choice == 4) {
      cat("Standard Deviation =", sd(numbers), "\n")
    } else if (choice == 5) {
      cat("Mean =", mean(numbers), "\n")
      cat("Median =", median(numbers), "\n")
      cat("Mode =", get_mode(numbers), "\n")
      cat("Standard Deviation =", sd(numbers), "\n")
      cat("Minimum =", min(numbers), "\n")
      cat("Maximum =", max(numbers), "\n")
      cat("Range =", range(numbers)[2] - range(numbers)[1], "\n")
    } else {
      cat("Invalid choice! Please run again.\n")
    }
  }
}
