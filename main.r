
#### Functions used across multiple parts of the Coursework ####

# Capitalises first letter of a string, lowercases the rest
.capitalise_first <- function(x) {
  if (is.na(x) || nchar(x) == 0) return(as.character(x))
  s <- tolower(as.character(x))
  paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
}

# Returns TRUE if a policy was active at the start of the given year
.is_active_at_year <- function(row, year) {
  if (is.na(row$inception.year) || row$inception.year > year) return(FALSE)
  if (is.na(row$exit) || trimws(row$exit) == "") return(TRUE)
  if (!is.na(row$year.of.exit) && row$year.of.exit >= year) return(TRUE)
  FALSE
}



#### Part A: Detecting Errors ####

check_policy_errors <- function(raw_file, output_file = "policy_errors.csv") {
  
  policies <- read.csv(raw_file, stringsAsFactors = FALSE)
  
  data_year     <- 2026
  valid_smokers <- c("Non-smoker", "Smoker", "Ex-smoker")
  
  # Empty errors data frame
  policy_errors <- data.frame(
    policy.number     = character(0),
    error_description = character(0),
    stringsAsFactors  = FALSE
  )
  
  # Appends a row to policy_errors for each index in 'errors'
  append_errors <- function(df, errors, message) {
    if (length(errors) > 0) {
      new_errors <- data.frame(
        policy.number     = policies$policy.number[errors],
        error_description = message,
        stringsAsFactors  = FALSE
      )
      df <- rbind(df, new_errors)
    }
    df
  }
  
  # Age at inception must be between 65 and 75
  policy_errors <- append_errors(policy_errors,
                                 which(policies$age.at.inception < 65), "Age at inception too young (<65)")
  policy_errors <- append_errors(policy_errors,
                                 which(policies$age.at.inception > 75), "Age at inception too old (>75)")
  
  # Term cannot take the policyholder into their 90th year
  policy_errors <- append_errors(policy_errors,
                                 which(policies$age.at.inception + policies$term > 90),
                                 "Term too long (would cover 90th year or beyond)")
  
  # Invalid smoker status
  policy_errors <- append_errors(policy_errors,
                                 which(!policies$smoker.status %in% valid_smokers), "Invalid smoker status")
  
  # No exits can have occurred in 2026 (data is from 01/01/26)
  policy_errors <- append_errors(policy_errors,
                                 which(!is.na(policies$year.of.exit) & policies$year.of.exit == data_year),
                                 "Policy cannot have exited in 2026")
  
  # Exit year cannot be before inception year
  policy_errors <- append_errors(policy_errors,
                                 which(!is.na(policies$year.of.exit) &
                                         policies$year.of.exit < policies$inception.year),
                                 "Policy ends before it starts")
  
  # Inception year cannot be in the future
  policy_errors <- append_errors(policy_errors,
                                 which(policies$inception.year > data_year),
                                 "Policy inception in the future")
  
  # A policy marked 'End' must have run for its full term
  early_end <- which(
    !is.na(policies$exit) &
      policies$exit == "End" &
      !is.na(policies$year.of.exit) &
      (policies$year.of.exit - policies$inception.year + 1) < policies$term
  )
  policy_errors <- append_errors(policy_errors, early_end,
                                 "Policy ended before term completed")
  
  # Save errors to CSV 
  write.csv(policy_errors, output_file, row.names = FALSE)
  colnames(policy_errors) <- c("Policy Number", "Error Description")
  return(policy_errors)
}


#### Part B: Policyholder Maintenance ####

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
                       file) {
  
  errors <- character(0)
  
  # Policy number must be a non-empty string
  policy.number <- as.character(policy.number)
  if (is.na(policy.number) || trimws(policy.number) == "")
    errors <- c(errors, "policy.number must be a non-empty character value")
  
  if (is.na(inception.year))
    errors <- c(errors, "inception.year must be numeric")
  
  # Age must be a whole number in the permitted entry range
  if (is.na(age.at.inception) || age.at.inception %% 1 != 0)
    errors <- c(errors, "age.at.inception must be an integer")
  
  if (!is.na(age.at.inception) && (age.at.inception < 65 || age.at.inception > 75))
    errors <- c(errors, "age.at.inception must be between 65 and 75")
  
  # Term must be a positive whole number
  if (is.na(term) || term %% 1 != 0 || term <= 0)
    errors <- c(errors, "term must be a positive integer")
  
  # Policy must not run into the policyholder's 90th year
  if (!is.na(age.at.inception) && !is.na(term) && age.at.inception + term > 90)
    errors <- c(errors, "age.at.inception + term must be <= 90")
  
  if (is.na(premium) || premium < 0)
    errors <- c(errors, "premium must be >= 0")
  
  if (is.na(sum.assured) || sum.assured < 0)
    errors <- c(errors, "sum.assured must be >= 0")
  
  # Standardise sex before checking
  sex_low <- tolower(trimws(sex))
  if (!sex_low %in% c("male", "female"))
    errors <- c(errors, "sex must be Male or Female")
  
  # Standardise smoker status before checking
  smoker_low <- tolower(trimws(smoker.status))
  if (!smoker_low %in% c("smoker", "non-smoker", "ex-smoker"))
    errors <- c(errors, "smoker.status must be Smoker, Non-smoker or Ex-smoker")
  
  if (length(errors) > 0)
    return(list(success = FALSE, errors = errors))
  
  if (!file.exists(file))
    return(list(success = FALSE, errors = "File not found"))
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  
  # Reject duplicate policy numbers
  if (any(df$policy.number == policy.number))
    return(list(success = FALSE, errors = "policy.number already exists"))
  
  # Build new row; categorical fields stored in consistent Title Case
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
  
  write.csv(rbind(df, new_row), file, row.names = FALSE)
  list(success = TRUE)
}


record_deaths <- function(policy_numbers, year, file) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  policy_numbers <- as.character(policy_numbers)
  
  # For each requested policy number, find its row index in df (NA if not found)
  pol_row <- match(policy_numbers, df$policy.number)
  
  not_found <- is.na(pol_row)
  already_exit <- !not_found & !is.na(df$exit[ifelse(not_found, 1, pol_row)]) &
    trimws(df$exit[ifelse(not_found, 1, pol_row)]) != ""
  not_active <- !not_found & !already_exit &
    !vapply(pol_row[!not_found & !already_exit],
            function(i) .is_active_at_year(df[i, ], year),
            logical(1))
  
  # Build a skipped list from the three failure checks
  skipped <- as.list(c(
    setNames(rep("policy not found", sum(not_found)),
             policy_numbers[not_found]),
    setNames(rep("policy already exited", sum(already_exit)),
             policy_numbers[already_exit]),
    setNames(rep("not active at start of year", sum(not_active)),
             policy_numbers[!not_found & !already_exit][not_active])
  ))
  
  # Updates the correct rows
  valid_row_pol <- pol_row[!not_found & !already_exit][!not_active]
  df$exit[valid_row_pol] <- "Death"
  df$year.of.exit[valid_row_pol] <- year
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, updated = length(valid_row_pol), skipped = skipped)
}


record_withdrawals <- function(policy_numbers, year, file) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  policy_numbers <- as.character(policy_numbers)
  
  # For each requested policy number, find its row index 
  pol_row <- match(policy_numbers, df$policy.number)
  
  not_found <- is.na(pol_row)
  already_exit <- !not_found & !is.na(df$exit[ifelse(not_found, 1, pol_row)]) &
    trimws(df$exit[ifelse(not_found, 1, pol_row)]) != ""
  not_active <- !not_found & !already_exit &
    !vapply(pol_row[!not_found & !already_exit],
            function(i) .is_active_at_year(df[i, ], year),
            logical(1))
  
  # Again build a skipped list from the three failure checks
  skipped <- as.list(c(
    setNames(rep("policy not found", sum(not_found)),
             policy_numbers[not_found]),
    setNames(rep("policy already exited", sum(already_exit)),
             policy_numbers[already_exit]),
    setNames(rep("not active at start of year", sum(not_active)),
             policy_numbers[!not_found & !already_exit][not_active])
  ))
  
  # Update the correct rows in one vectorised assignment
  valid_pol_row <- pol_row[!not_found & !already_exit][!not_active]
  df$exit[valid_pol_row] <- "Withdrawl" # in the files on moodle Withdrawal is spelt Withdrawl so keeping consistent 
  df$year.of.exit[valid_pol_row] <- year
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, updated = length(valid_pol_row), skipped = skipped)
}


settle_matured_policies <- function(year, file) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  
  # Find policies whose final year of cover is exactly 'year'
  final_year <- df$inception.year + df$term - 1
  matured <- which(final_year == year)
  
  # Keep only those with no exit already recorded
  no_exit <- is.na(df$exit[matured]) | trimws(df$exit[matured]) == ""
  active <- vapply(matured[no_exit],
                    function(i) .is_active_at_year(df[i, ], year),
                    logical(1))
  
  settle_pol <- matured[no_exit][active]
  
  df$exit[settle_pol] <- "End"
  df$year.of.exit[settle_pol] <- year
  
  write.csv(df, file, row.names = FALSE)
  list(success = TRUE, settled = df$policy.number[settle_pol])
}


#### Part C: Summarising the Data ####


# Creates a summary table for any numeric column
.build_summary_table <- function(df, value_col, result_label) {
  
  sexes <- c("Male", "Female")
  smoker_levels <- c("Smoker", "Non-smoker", "Ex-smoker")
  
  all_total <- sum(df[[value_col]])
  
  # Totals split by sex
  sex_totals <- sapply(sexes,
                       function(s) sum(df[[value_col]][df$sex == s]))
  
  # Totals split by smoker status
  smoker_totals <- sapply(smoker_levels,
                          function(sm) sum(df[[value_col]][df$smoker.status == sm]))
  
  # Totals split by sex and smoker status
  cross_totals <- sapply(sexes,
                         function(s) {
                           sapply(smoker_levels,
                                  function(sm) sum(df[[value_col]][df$sex == s & df$smoker.status == sm]))
                         }
  )
  
  cross_vec <- as.vector(cross_totals)
  cross_labels <- as.vector(
    outer(smoker_levels, sexes, function(sm, s) paste(s, "-", sm))
  )
  
  categories <- c(
    "All policies",
    paste("Sex:", sexes),
    paste("Smoker status:", smoker_levels),
    cross_labels
  )
  
  result <- data.frame(
    category = categories,
    amount   = c(all_total, sex_totals, smoker_totals, cross_vec),
    stringsAsFactors = FALSE
  )
  # Names the tables corresponding to the function called
  names(result)[2] <- result_label
  result
}


premium_summary <- function(year, file) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  
  # Only chooses policies active at the start of the year
  active_flag <- vapply(seq_len(nrow(df)),
                        function(i) .is_active_at_year(df[i, ], year), logical(1))
  
  .build_summary_table(df[active_flag, ], "premium", "total.premium")
}


death_benefit_summary <- function(year, file) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Keep only policies where the exit was through Death in the given year
  deaths_flag <- !is.na(df$exit) &
    df$exit == "Death" &
    !is.na(df$year.of.exit) &
    df$year.of.exit == year
  
  .build_summary_table(df[deaths_flag, ], "sum.assured", "total.death.benefit")
}


policy_cashflows <- function(policy.number, year, interest.rate, file) {
  
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  policy.number <- as.character(policy.number)
  
  policy_row <- which(df$policy.number == policy.number)
  
  if (length(policy_row) == 0)
    return(list(success = FALSE, error = "Policy not found"))
  
  pol <- df[policy_row, ]
  
  # Premiums stop at the earlier of the given year and the exit year
  final_premium_year <- year
  if (!is.na(pol$year.of.exit) && pol$year.of.exit <= year)
    final_premium_year <- pol$year.of.exit
  
  premium_years <- pol$inception.year:final_premium_year
  
  nominal_premium <- length(premium_years) * pol$premium
  
  # Each premium paid at start of year t accumulates to end of 'year'
  years_accumulated <- year - premium_years + 1
  pv_premium <- sum(pol$premium * (1 + interest.rate)^years_accumulated)
  
  nominal_benefit <- 0
  pv_benefit <- 0
  
  if (!is.na(pol$exit) &&
      pol$exit == "Death" &&
      !is.na(pol$year.of.exit) &&
      pol$year.of.exit <= year) {
    
    nominal_benefit <- pol$sum.assured
    
    # Death mid-year means we need to account for an extra half year of interest
    pv_benefit <- pol$sum.assured * (1 + interest.rate)^(year - pol$year.of.exit + 0.5)
  }
  
  data.frame(
    quantity = c("Nominal premium", "Nominal benefit",
                 "Present value of premiums", "Present value of benefits"),
    amount = c(nominal_premium, nominal_benefit, pv_premium, pv_benefit),
    stringsAsFactors = FALSE
  )
}



#### Part D: Simulating Deaths ####


simulate_deaths <- function(sim_year, policy_file) {
  
  policies       <- read.csv(policy_file, stringsAsFactors = FALSE)
  mortality_table <- read.csv("mortrates.csv", stringsAsFactors = FALSE)
  smoker_rates   <- read.csv("srates.csv", stringsAsFactors = FALSE)
  
  # Keep only policies active in the simulation year
  active_policies <- policies[
    policies$inception.year <= sim_year &
      (is.na(policies$year.of.exit) | policies$year.of.exit >= sim_year), ]
  
  if (nrow(active_policies) == 0) return(data.frame())
  
  # Age in the simulation year (everyone born 1 Jan)
  active_policies$age       <- active_policies$age.at.inception +
    (sim_year - active_policies$inception.year)
  active_policies$full_name <- paste(active_policies$first.name,
                                     active_policies$surname)
  
  # Look up base mortality by age and sex
  age_match <- match(active_policies$age, mortality_table$Age)
  active_policies$base_mortality <- ifelse(
    active_policies$sex == "Male",
    mortality_table$Male[age_match],
    mortality_table$Female[age_match]
  )
  
  # Smoking adjustment: rows = sex (Male/Female), cols = smoker status
  smoke_adj <- matrix(c(
    smoker_rates$Non.smoker[smoker_rates$Gender == "Male"],
    smoker_rates$Smoker[smoker_rates$Gender == "Male"],
    smoker_rates$Ex.Smoker[smoker_rates$Gender == "Male"],
    smoker_rates$Non.smoker[smoker_rates$Gender == "Female"],
    smoker_rates$Smoker[smoker_rates$Gender == "Female"],
    smoker_rates$Ex.Smoker[smoker_rates$Gender == "Female"]
  ), nrow = 2, byrow = TRUE)
  
  sex_idx   <- ifelse(active_policies$sex == "Male", 1, 2)
  smoke_idx <- match(active_policies$smoker.status,
                     c("Non-smoker", "Smoker", "Ex-smoker"))
  
  # Apply smoking multiplier to base mortality
  active_policies$adjusted_mortality <- active_policies$base_mortality *
    smoke_adj[cbind(sex_idx, smoke_idx)]
  
  # Simulate deaths: 1 = died, 0 = survived
  active_policies$died <- rbinom(nrow(active_policies), 1,
                                 active_policies$adjusted_mortality)
  
  # Return table of deaths only
  death_records <- active_policies[active_policies$died == 1,
                                   c("policy.number", "full_name", "age", "smoker.status", "sum.assured")]
  colnames(death_records) <- c("Policy Number", "Name", "Age at Death",
                               "Smoking Status", "Sum Assured")
  death_records
}



