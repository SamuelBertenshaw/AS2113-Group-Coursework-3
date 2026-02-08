############################
# Part A: Detecting Errors #
############################

check_policy_errors <- function(file_path, write_csv = TRUE, output_file = "policy_errors.csv") {
  
  
  policy_data <- read.csv(file_path, stringsAsFactors = FALSE)
  
  # Create empty error data frame
  errors <- data.frame(
    policy.number = character(),
    error = character(),
    stringsAsFactors = FALSE
  )
  
  # Allowed smoking statuses
  valid_smoking <- c("Smoker", "Non-smoker")
  
  # Loop through each policy
  for (i in 1:nrow(policy_data)) {
    
    pol <- policy_data[i, ]
    
    # Age check
    if (pol$age.at.inception < 65 || pol$age.at.inception > 75) {
      errors[nrow(errors) + 1, ] <- list(pol$policy.number, "Invalid age at inception")
    }
    
    # Term check
    if (pol$age.at.inception + pol$term > 90) {
      errors[nrow(errors) + 1, ] <- list(pol$policy.number, "Policy term exceeds age 90 limit")
    }
    
    # Inception year check
    if (pol$inception.year > 2026) {
      errors[nrow(errors) + 1, ] <- list(pol$policy.number, "Inception year after data creation date")
    }
    
    # Smoking status check
    if (!(pol$smoker.status %in% valid_smoking)) {
      errors[nrow(errors) + 1, ] <- list(pol$policy.number, "Invalid smoking status")
    }
    
    # Death in 2026 or later
    if (pol$exit == "Death" && pol$year.of.exit >= 2026) {
      errors[nrow(errors) + 1, ] <- list(pol$policy.number, "Death recorded in 2026 or later")
    }
    
    # Policy ended before full term
    if (pol$exit == "End") {
      expected_end <- pol$inception.year + pol$term - 1
      if (pol$year.of.exit != expected_end) {
        errors[nrow(errors) + 1, ] <- list(pol$policy.number, "Policy ended before full term completed")
      }
    }
    
    # Exit year present but exit type missing
    if (pol$exit == "" && !is.na(pol$year.of.exit)) {
      errors[nrow(errors) + 1, ] <- list(pol$policy.number, "Exit year present but exit type missing")
    }
    
  } 
  
  # Name columns of the csv file
  colnames(errors) <- c("policy_number", "error_description")
  
  # Writes the csv
  write.csv(errors, output_file, row.names = FALSE)
  
  # Return error data frame in the terminal for easy viewing
  return(errors)
}
####################################
# Part B: Policyholder Maintenance #
####################################

# Function to ensure names, sex and smoker is stored in a consistent format
.capitalise_first <- function(x) {
  if (is.na(x) || nchar(x) == 0) return(as.character(x))
  s <- tolower(as.character(x))
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# Function to check if a policy is active at the start of the year
.is_active_at_year <- function(row, year) {
  if (is.na(row$inception.year) || row$inception.year > year) return(FALSE)
  if (is.na(row$exit) || trimws(row$exit) == "") return(TRUE)
  if (!is.na(row$year.of.exit) && row$year.of.exit > year) return(TRUE)
  FALSE
}

# Function to add a new policyholder
add_policy <- function(policy.number,
                       first.name,
                       surname,
                       inception.year,
                       age.at.inception,
                       premium,
                       sum.assured,
                       term,
                       sex,
                       smoker.status,
                       file = "pholders.csv") {
  
  # Vector used to store error messages during validation
  errors <- character(0)
  
  # Validates policy numbers
  policy.number <- as.character(policy.number)
  if (is.na(policy.number) || trimws(policy.number) == "")
    errors <- c(errors, "policy.number must be a non-empty character value")
  
  # Validate numeric inputs
  if (is.na(inception.year)) errors <- c(errors, "inception.year must be numeric")
  
  if (is.na(age.at.inception) || age.at.inception %% 1 != 0)
    errors <- c(errors, "age.at.inception must be an integer")
  
  if (age.at.inception < 65 || age.at.inception > 75)
    errors <- c(errors, "age.at.inception must be between 65 and 75")
  
  if (is.na(term) || term %% 1 != 0 || term <= 0)
    errors <- c(errors, "term must be a positive integer")
  
  if (age.at.inception + term > 90)
    errors <- c(errors, "age.at.inception + term must be <= 90")
  
  if (is.na(premium) || premium < 0)
    errors <- c(errors, "premium must be >= 0")
  
  if (is.na(sum.assured) || sum.assured < 0)
    errors <- c(errors, "sum.assured must be >= 0")
  
  # Validate sex 
  sex_low <- tolower(trimws(sex))
  if (!sex_low %in% c("male", "female"))
    errors <- c(errors, "sex must be Male or Female")
  
  # Validate smoker status 
  smoker_low <- tolower(trimws(smoker.status))
  if (!smoker_low %in% c("smoker", "non-smoker", "ex-smoker"))
    errors <- c(errors, "smoker.status must be Smoker, Non-smoker or Ex-smoker")
  
  # If any validation failed, return errors and stop
  if (length(errors) > 0)
    return(list(success = FALSE, errors = errors))
  
  # Check that the CSV file exists
  if (!file.exists(file))
    return(list(success = FALSE, errors = c("pholders.csv not found")))
  
  # Read existing policyholder file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Ensure policy numbers in the file are character
  df$policy.number <- as.character(df$policy.number)
  
  # Check policy number uniqueness
  if (any(df$policy.number == policy.number))
    return(list(success = FALSE, errors = c("policy.number already exists")))
  
  # Create a new row with correctly formatted values
  new_row <- data.frame(
    policy.number = policy.number,
    first.name = .capitalise_first(first.name),
    surname = .capitalise_first(surname),
    inception.year = inception.year,
    age.at.inception = age.at.inception,
    premium = premium,
    sum.assured = sum.assured,
    term = term,
    sex = .capitalise_first(sex_low),
    smoker.status = .capitalise_first(smoker_low),
    exit = NA,
    year.of.exit = NA,
    stringsAsFactors = FALSE
  )
  
  # Append the new policyholder and overwrite the CSV
  df <- rbind(df, new_row)
  write.csv(df, file, row.names = FALSE)
  
  # Indicate success
  list(success = TRUE)
}

# Function to record policyholder deaths (must be recorded before withdrawals)
record_deaths <- function(policy_numbers, year, file = "pholders.csv") {
  
  # Read the existing policyholder file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Ensure policy numbers are treated as character identifiers
  df$policy.number <- as.character(df$policy.number)
  policy_numbers <- as.character(policy_numbers)
  
  # Objects used to track results of the operation
  skipped <- list()   # policies that could not be updated
  updated <- 0        # number of successfully recorded deaths
  
  # Loop through each policy number supplied by the user
  for (p in policy_numbers) {
    
    # Locate the policy in the data frame
    idx <- which(df$policy.number == p)
    
    # If the policy does not exist, skip it
    if (length(idx) == 0) {
      skipped[[p]] <- "policy not found"
      next
    }
    
    # Check that the policy is active at the start of the year
    if (!.is_active_at_year(df[idx, ], year)) {
      skipped[[p]] <- "not active at start of year"
      next
    }
    
    # Record death by updating exit fields
    df$exit[idx] <- "Died"
    df$year.of.exit[idx] <- year
    
    # Increment successful update counter
    updated <- updated + 1
  }
  
  # Save the updated data back to the CSV file
  write.csv(df, file, row.names = FALSE)
  
  # Return a summary of the operation
  list(success = TRUE, updated = updated, skipped = skipped)
}

# Function to record withdrawals
record_withdrawals <- function(policy_numbers, year, file = "pholders.csv") {
  
  # Read the existing policyholder file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Ensure policy numbers are character values
  df$policy.number <- as.character(df$policy.number)
  policy_numbers <- as.character(policy_numbers)
  
  # Objects used to track results
  skipped <- list()   # policies that could not be withdrawn
  updated <- 0        # number of successful withdrawals
  
  # Loop through each supplied policy number
  for (p in policy_numbers) {
    
    # Find the policy in the data frame
    idx <- which(df$policy.number == p)
    
    # Skip if policy number does not exist
    if (length(idx) == 0) {
      skipped[[p]] <- "policy not found"
      next
    }
    
    # Check policy is active at the start of the year
    if (!.is_active_at_year(df[idx, ], year)) {
      skipped[[p]] <- "not active at start of year"
      next
    }
    
    # Record withdrawal
    df$exit[idx] <- "Withdrawn"
    df$year.of.exit[idx] <- year
    
    # Increment count of successful updates
    updated <- updated + 1
  }
  
  # Save the updated file
  write.csv(df, file, row.names = FALSE)
  
  # Return summary information
  list(success = TRUE, updated = updated, skipped = skipped)
}

#  Function to settle matured policies at end of year 
settle_matured_policies <- function(year, file = "pholders.csv") {
  
  # Read the policyholder file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Ensure policy numbers are treated as characters
  df$policy.number <- as.character(df$policy.number)
  
  # Vector to store policy numbers that are settled
  settled <- character(0)
  
  # Loop through each policy in the data frame
  for (i in seq_len(nrow(df))) {
    
    # Calculate the final year of the policy term
    last_year <- df$inception.year[i] + df$term[i] - 1
    
    # Check if the policy matures in the given year
    # and is still active at the start of that year
    if (last_year == year && .is_active_at_year(df[i, ], year)) {
      
      # Record maturity of the policy
      df$exit[i] <- "End"
      df$year.of.exit[i] <- year
      
      # Store the policy number
      settled <- c(settled, df$policy.number[i])
    }
  }
  
  # Save the updated file
  write.csv(df, file, row.names = FALSE)
  
  # Return a summary of settled policies
  list(success = TRUE, settled = settled)
}


