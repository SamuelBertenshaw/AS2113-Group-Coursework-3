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
  
  errors <- character(0)
  
  # Validate policy number
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
  
  # Stop if validation fails
  if (length(errors) > 0)
    return(list(success = FALSE, errors = errors))
  
  # Check file exists
  if (!file.exists(file))
    return(list(success = FALSE, errors = c("pholders.csv not found")))
  
  # Read file
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  
  # Check uniqueness
  if (any(df$policy.number == policy.number))
    return(list(success = FALSE, errors = c("policy.number already exists")))
  
  # Create new row
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
  
  # Append and save
  df <- rbind(df, new_row)
  write.csv(df, file, row.names = FALSE)
  
  list(success = TRUE)
}

# Function to record policyholder deaths
record_deaths <- function(policy_numbers, year, file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  policy_numbers <- as.character(policy_numbers)
  
  skipped <- list()
  updated <- 0
  
  for (p in policy_numbers) {
    
    idx <- which(df$policy.number == p)
    
    # Policy not found
    if (length(idx) == 0) {
      skipped[[p]] <- "policy not found"
      next
    }
    
    # Prevent overwriting an existing exit
    if (!is.na(df$exit[idx]) && trimws(df$exit[idx]) != "") {
      skipped[[p]] <- "policy already exited"
      next
    }
    
    # Must be active at start of year
    if (!.is_active_at_year(df[idx, ], year)) {
      skipped[[p]] <- "not active at start of year"
      next
    }
    
    # Record death
    df$exit[idx] <- "Death"
    df$year.of.exit[idx] <- year
    updated <- updated + 1
  }
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, updated = updated, skipped = skipped)
}

# Function to record withdrawals
record_withdrawals <- function(policy_numbers, year, file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  policy_numbers <- as.character(policy_numbers)
  
  skipped <- list()
  updated <- 0
  
  for (p in policy_numbers) {
    
    idx <- which(df$policy.number == p)
    
    # Policy not found
    if (length(idx) == 0) {
      skipped[[p]] <- "policy not found"
      next
    }
    
    # Prevent overwriting an existing exit
    if (!is.na(df$exit[idx]) && trimws(df$exit[idx]) != "") {
      skipped[[p]] <- "policy already exited"
      next
    }
    
    # Must be active at start of year
    if (!.is_active_at_year(df[idx, ], year)) {
      skipped[[p]] <- "not active at start of year"
      next
    }
    
    # Record withdrawal
    df$exit[idx] <- "Withdrawl"
    df$year.of.exit[idx] <- year
    updated <- updated + 1
  }
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, updated = updated, skipped = skipped)
}

# Function to settle matured policies at end of year
settle_matured_policies <- function(year, file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  
  settled <- character(0)
  
  for (i in seq_len(nrow(df))) {
    
    # Skip policies that already exited
    if (!is.na(df$exit[i]) && trimws(df$exit[i]) != "")
      next
    
    last_year <- df$inception.year[i] + df$term[i] - 1
    
    # Settle if policy matures this year
    if (last_year == year && .is_active_at_year(df[i, ], year)) {
      df$exit[i] <- "End"
      df$year.of.exit[i] <- year
      settled <- c(settled, df$policy.number[i])
    }
  }
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, settled = settled)
}

#################################
# Part C: Summarising the Data  #
#################################

premium_summary <- function(year, file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  
  # Determine which policies are active at the start of the year
  active <- logical(nrow(df))
  for (i in seq_len(nrow(df))) {
    active[i] <- .is_active_at_year(df[i, ], year)
  }
  
  df_active <- df[active, ]
  
  result <- data.frame(
    category = character(),
    total.premium = numeric(),
    stringsAsFactors = FALSE
  )
  
  result[nrow(result) + 1, ] <-
    list("All policies", sum(df_active$premium))
  
  for (s in c("Male", "Female")) {
    result[nrow(result) + 1, ] <-
      list(paste("Sex:", s),
           sum(df_active$premium[df_active$sex == s]))
  }
  
  for (sm in c("Smoker", "Non-smoker", "Ex-smoker")) {
    result[nrow(result) + 1, ] <-
      list(paste("Smoker status:", sm),
           sum(df_active$premium[df_active$smoker.status == sm]))
  }
  
  for (s in c("Male", "Female")) {
    for (sm in c("Smoker", "Non-smoker", "Ex-smoker")) {
      result[nrow(result) + 1, ] <-
        list(paste(s, "-", sm),
             sum(df_active$premium[
               df_active$sex == s &
                 df_active$smoker.status == sm
             ]))
    }
  }
  
  result
}

death_benefit_summary <- function(year, file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  deaths <- df$exit == "Death" & df$year.of.exit == year
  df_deaths <- df[deaths, ]
  
  result <- data.frame(
    category = character(),
    total.death.benefit = numeric(),
    stringsAsFactors = FALSE
  )
  
  result[nrow(result) + 1, ] <-
    list("All policies", sum(df_deaths$sum.assured))
  
  for (s in c("Male", "Female")) {
    result[nrow(result) + 1, ] <-
      list(paste("Sex:", s),
           sum(df_deaths$sum.assured[df_deaths$sex == s]))
  }
  
  for (sm in c("Smoker", "Non-smoker", "Ex-smoker")) {
    result[nrow(result) + 1, ] <-
      list(paste("Smoker status:", sm),
           sum(df_deaths$sum.assured[df_deaths$smoker.status == sm]))
  }
  
  for (s in c("Male", "Female")) {
    for (sm in c("Smoker", "Non-smoker", "Ex-smoker")) {
      result[nrow(result) + 1, ] <-
        list(paste(s, "-", sm),
             sum(df_deaths$sum.assured[
               df_deaths$sex == s &
                 df_deaths$smoker.status == sm
             ]))
    }
  }
  
  result
}

policy_cashflows <- function(policy.number, year, interest.rate,
                             file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  policy.number <- as.character(policy.number)
  
  idx <- which(df$policy.number == policy.number)
  if (length(idx) == 0)
    return(list(success = FALSE, error = "Policy not found"))
  
  pol <- df[idx, ]
  
  final_year <- year
  if (!is.na(pol$year.of.exit) && pol$year.of.exit <= year)
    final_year <- pol$year.of.exit
  
  premium_years <- pol$inception.year:final_year
  
  nominal_premium <- length(premium_years) * pol$premium
  
  years_to_accumulate <- year - premium_years + 1
  pv_premium <- sum(pol$premium * (1 + interest.rate)^years_to_accumulate)
  
  nominal_benefit <- 0
  pv_benefit <- 0
  
  if (pol$exit == "Death" && pol$year.of.exit <= year) {
    nominal_benefit <- pol$sum.assured
    pv_benefit <- pol$sum.assured *
      (1 + interest.rate)^(year - pol$year.of.exit + 0.5)
  }
  
  data.frame(
    quantity = c("Nominal premium",
                 "Nominal benefit",
                 "Present value of premiums",
                 "Present value of benefits"),
    amount = c(nominal_premium,
               nominal_benefit,
               pv_premium,
               pv_benefit),
    stringsAsFactors = FALSE
  )
}

