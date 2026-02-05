############################
# Part A: Detecting Errors #
############################

# Read in raw data
error_checking <- read.csv("pholdersraw.csv", stringsAsFactors = FALSE)

# Create empty error log
errors <- data.frame(
  policy.number = character(),
  error = character(),
  stringsAsFactors = FALSE
)

# Allowed smoking statuses
valid_smoking <- c("Smoker", "Non-smoker")

for (i in 1:nrow(error_checking)) {
  
  pol <- error_checking[i, ]
  
  if (pol$age.at.inception < 65 || pol$age.at.inception > 75) {
    errors[nrow(errors) + 1, ] <-
      list(pol$policy.number, "Invalid age at inception")
  }
  
  if (pol$age.at.inception + pol$term > 90) {
    errors[nrow(errors) + 1, ] <-
      list(pol$policy.number, "Policy term exceeds age 90 limit")
  }
  
  if (pol$inception.year > 2026) {
    errors[nrow(errors) + 1, ] <-
      list(pol$policy.number, "Inception year after data creation date")
  }
  
  if (!(pol$smoker.status %in% valid_smoking)) {
    errors[nrow(errors) + 1, ] <-
      list(pol$policy.number, "Invalid smoking status")
  }
  
  if (pol$exit == "Death" && pol$year.of.exit >= 2026) {
    errors[nrow(errors) + 1, ] <-
      list(pol$policy.number, "Death recorded in 2026 or later")
  }
  
  if (pol$exit == "End") {
    expected_end <- pol$inception.year + pol$term - 1
    if (pol$year.of.exit != expected_end) {
      errors[nrow(errors) + 1, ] <-
        list(pol$policy.number, "Policy ended before full term completed")
    }
  }
  
  if (pol$exit == "" && !is.na(pol$year.of.exit)) {
    errors[nrow(errors) + 1, ] <-
      list(pol$policy.number, "Exit year present but exit type missing")
  }
}

# Name columns 
colnames(errors) <- c("policy_number", "error_description")

# Write error csv file
write.csv(errors, "policy_errors.csv", row.names = FALSE)

####################################
# Part B: policyholder maintenance #
####################################

# Capitalise first letter, rest lower-case
.capitalise_first <- function(x) {
  if (is.na(x) || nchar(x) == 0) return(as.character(x))
  s <- tolower(as.character(x))
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# Check whether a policy is active at the start of a given year
.is_active_at_year <- function(row, year) {
  if (is.na(row$inception.year) || row$inception.year > year) return(FALSE)
  if (is.na(row$exit) || trimws(row$exit) == "") return(TRUE)
  if (!is.na(row$year.of.exit) && row$year.of.exit > year) return(TRUE)
  FALSE
}

# -----------------------#
# Add a new policyholder #
# -----------------------#
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
  
  # Validate numeric fields
  if (is.na(policy.number)) errors <- c(errors, "policy.number must be numeric")
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
  
  # Validate sex (case-insensitive)
  sex_low <- tolower(trimws(sex))
  if (!sex_low %in% c("male", "female"))
    errors <- c(errors, "sex must be Male or Female")
  
  # Validate smoker.status (case-insensitive, strict choices)
  smoker_low <- tolower(trimws(smoker.status))
  if (!smoker_low %in% c("smoker", "non-smoker", "ex-smoker"))
    errors <- c(errors, "smoker.status must be Smoker, Non-smoker or Ex-smoker")
  
  # Stop if validation fails
  if (length(errors) > 0)
    return(list(success = FALSE, errors = errors))
  
  # Read file
  if (!file.exists(file))
    return(list(success = FALSE, errors = c("pholders.csv not found")))
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Check unique policy number
  if (any(df$policy.number == policy.number))
    return(list(success = FALSE, errors = c("policy.number already exists")))
  
  # Prepare new row
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
  
  # Append and overwrite
  df <- rbind(df, new_row)
  write.csv(df, file, row.names = FALSE)
  
  list(success = TRUE)
}

# --------------#
# Record deaths #
# --------------#
record_deaths <- function(policy_numbers, year, file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  skipped <- list()
  updated <- 0
  
  for (p in policy_numbers) {
    idx <- which(df$policy.number == p)
    if (length(idx) == 0) {
      skipped[[as.character(p)]] <- "policy not found"
      next
    }
    if (!.is_active_at_year(df[idx, ], year)) {
      skipped[[as.character(p)]] <- "not active at start of year"
      next
    }
    df$exit[idx] <- "Died"
    df$year.of.exit[idx] <- year
    updated <- updated + 1
  }
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, updated = updated, skipped = skipped)
}

# -------------------#
# Record withdrawals #
# -------------------#
record_withdrawals <- function(policy_numbers, year, file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  skipped <- list()
  updated <- 0
  
  for (p in policy_numbers) {
    idx <- which(df$policy.number == p)
    if (length(idx) == 0) {
      skipped[[as.character(p)]] <- "policy not found"
      next
    }
    if (!.is_active_at_year(df[idx, ], year)) {
      skipped[[as.character(p)]] <- "not active at start of year"
      next
    }
    df$exit[idx] <- "Withdrawn"
    df$year.of.exit[idx] <- year
    updated <- updated + 1
  }
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, updated = updated, skipped = skipped)
}

# ---------------------------------------#
# Settle matured policies at end of year # 
# ---------------------------------------#
settle_matured_policies <- function(year, file = "pholders.csv") {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  settled <- integer(0)
  
  for (i in seq_len(nrow(df))) {
    last_year <- df$inception.year[i] + df$term[i] - 1
    if (last_year == year && .is_active_at_year(df[i, ], year)) {
      df$exit[i] <- "End"
      df$year.of.exit[i] <- year
      settled <- c(settled, df$policy.number[i])
    }
  }
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, settled = settled)
}
