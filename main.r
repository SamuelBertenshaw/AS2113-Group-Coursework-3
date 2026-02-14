#### Part A: Detecting Errors ####


check_policy_errors <- function(raw_file, output_file = "policy_errors.csv") {
  
  # Load the raw policies
  policies <- read.csv(raw_file, stringsAsFactors = FALSE)
  
  data_year <- 2026  # Date of data
  valid_smokers <- c("Non-smoker", "Smoker", "Ex-smoker")
  mandatory_fields <- c("sex", "smoker.status", "age.at.inception", "inception.year", "term", "sum.assured")
  
  # Create empty dataframe to store errors
  policy_errors <- data.frame(policy.number = character(0), error_description = character(0), stringsAsFactors = FALSE)
  
  # Function to append errors to the data frame
  append_errors <- function(df, errors, message) {
    if(length(errors) > 0) {
      new_errors <- data.frame(policy.number = policies$policy.number[errors],
                               error_description = message,
                               stringsAsFactors = FALSE)
      df <- rbind(df, new_errors)
    }
    df
  }
  
  # Age at inception must be between 65 and 75
  policy_errors <- append_errors(policy_errors, which(policies$age.at.inception < 65), 
                                 "Age at inception too young (<65)")
  policy_errors <- append_errors(policy_errors, which(policies$age.at.inception > 75), 
                                 "Age at inception too old (>75)")
  
  # Term cannot cover someone in their 90th year
  policy_errors <- append_errors(policy_errors, which(policies$age.at.inception + policies$term > 89), 
                                 "Term too long (would cover 90th year or beyond)")
  
  # Invalid smoker status
  policy_errors <- append_errors(policy_errors, which(!policies$smoker.status %in% valid_smokers), 
                                 "Invalid smoker status")
  
  # Exit in 2026 (cannot happen)
  policy_errors <- append_errors(policy_errors, which(!is.na(policies$year.of.exit) & 
                                                        policies$year.of.exit == data_year),
                                 "Policy cannot have exited in 2026")
  
  # Policy ends before it starts
  policy_errors <- append_errors(policy_errors, which(!is.na(policies$year.of.exit) & 
                                                        policies$year.of.exit < policies$inception.year),
                                 "Policy ends before it starts")
  
  # 6. Policy inception in the future
  policy_errors <- append_errors(policy_errors, which(policies$inception.year > data_year),
                                 "Policy inception in the future")
  
  # 7. Policy ends before term completed
  early_end <- which(policies$exit == "End" & !is.na(policies$year.of.exit) &
                       (policies$year.of.exit - policies$inception.year + 1) < policies$term)
  policy_errors <- append_errors(policy_errors, early_end, "Policy ended before term completed")
  
  
  # Save to CSV
  write.csv(policy_errors, output_file, row.names = FALSE)
  colnames(policy_errors) <- c("Policy Number","Error Description")
  return(policy_errors)
}



#### Part B: Policyholder Maintenance ####


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


#### Part C: Summarising the Data  ####


# Function to summarise premiums earned in a given year
premium_summary <- function(year, file) {
  
  # Load policy data
  df <- read.csv(file, stringsAsFactors = FALSE)
  df$policy.number <- as.character(df$policy.number)
  
  # Identify policies that are active at the start of the given year
  active <- logical(nrow(df))
  for (i in seq_len(nrow(df))) {
    active[i] <- .is_active_at_year(df[i, ], year)
  }
  
  # Subset to active policies only
  df_active <- df[active, ]
  
  # Initialise results data frame
  result <- data.frame(
    category = character(),
    total.premium = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Total premium from all active policies
  result[nrow(result) + 1, ] <-
    list("All policies", sum(df_active$premium))
  
  # Premium split by sex
  for (s in c("Male", "Female")) {
    result[nrow(result) + 1, ] <-
      list(paste("Sex:", s),
           sum(df_active$premium[df_active$sex == s]))
  }
  
  # Premium split by smoker status
  for (sm in c("Smoker", "Non-smoker", "Ex-smoker")) {
    result[nrow(result) + 1, ] <-
      list(paste("Smoker status:", sm),
           sum(df_active$premium[df_active$smoker.status == sm]))
  }
  
  # Premium split by both sex and smoker status
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
  
  # Return summary table
  result
}

# Function to summarise death benefits paid out in a given year
death_benefit_summary <- function(year, file = "pholders.csv") {
  
  # Read the policyholder data from file
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Identify policies where the exit reason is death and 
  # the year of exit matches the year entered by the user
  deaths <- df$exit == "Death" & df$year.of.exit == year
  
  # Subset the data to only those policies
  df_deaths <- df[deaths, ]
  
  # Create an empty results data frame to store the summaries
  result <- data.frame(
    category = character(),
    total.death.benefit = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Total death benefit paid across all policies
  result[nrow(result) + 1, ] <-
    list("All policies", sum(df_deaths$sum.assured))
  
  # Death benefit split by sex
  for (s in c("Male", "Female")) {
    result[nrow(result) + 1, ] <-
      list(paste("Sex:", s),
           sum(df_deaths$sum.assured[df_deaths$sex == s]))
  }
  
  # Death benefit split by smoker status
  for (sm in c("Smoker", "Non-smoker", "Ex-smoker")) {
    result[nrow(result) + 1, ] <-
      list(paste("Smoker status:", sm),
           sum(df_deaths$sum.assured[df_deaths$smoker.status == sm]))
  }
  
  # Death benefit split by both sex and smoker status
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
  
  # Return the completed summary table
  result
}

# Function to summarise the premiums and potential death benefits 
# for a given policy number, both nominal and accumulated 
policy_cashflows <- function(policy.number, year, interest.rate,
                             file = "pholders.csv") {
  
  # Read the policyholder data
  df <- read.csv(file, stringsAsFactors = FALSE)
  
  # Ensure policy numbers are treated consistently as characters
  df$policy.number <- as.character(df$policy.number)
  policy.number <- as.character(policy.number)
  
  # Locate the requested policy
  idx <- which(df$policy.number == policy.number)
  
  # If the policy does not exist, return an error message
  if (length(idx) == 0)
    return(list(success = FALSE, error = "Policy not found"))
  
  # Extract the policy record
  pol <- df[idx, ]
  
  # Determine the last year premiums are paid
  final_year <- year
  if (!is.na(pol$year.of.exit) && pol$year.of.exit <= year)
    final_year <- pol$year.of.exit
  
  # Create a sequence of years in which premiums were paid
  premium_years <- pol$inception.year:final_year
  
  # Nominal total premium = annual premium x number of years premiums were paid
  nominal_premium <- length(premium_years) * pol$premium
  
  # Calculate how many years each premium accumulates interest
  years_to_accumulate <- year - premium_years + 1
  
  # Present value of premiums accumulated to the end of the given year
  pv_premium <- sum(pol$premium * (1 + interest.rate)^years_to_accumulate)
  
  # Initialise benefit values (default is zero)
  nominal_benefit <- 0
  pv_benefit <- 0
  
  # Calculates accumulated death benefit
  if (pol$exit == "Death" && pol$year.of.exit <= year) {
    nominal_benefit <- pol$sum.assured
    pv_benefit <- pol$sum.assured *
      (1 + interest.rate)^(year - pol$year.of.exit + 0.5)
  }
  
  # Return results as a small, clearly labelled data frame
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



#### Part D: Simulating Active Policies in a year ####

simulate_deaths <- function(sim_year, policy_file) {

  policies <- read.csv(policy_file, stringsAsFactors = FALSE)
  mortality_table <- read.csv("mortrates.csv", stringsAsFactors = FALSE)
  smoker_rates <- read.csv("srates.csv", stringsAsFactors = FALSE)
  
  # Filter active policies for the year
  active_policies <- policies[
    policies$inception.year <= sim_year & (is.na(policies$year.of.exit) | policies$year.of.exit >= sim_year), 
  ]
  
  if(nrow(active_policies) == 0) return(data.frame())  # No active policies
  
  # Compute age in the simulation year
  active_policies$age <- active_policies$age.at.inception + (sim_year - active_policies$inception.year)
  
  # Combine first and last name
  active_policies$full_name <- paste(active_policies$first.name, active_policies$surname)
  
  # Finding the corresponding mortality for each policyholder
  age_match <- match(active_policies$age, mortality_table$Age)
  active_policies$base_mortality <- ifelse(
    active_policies$sex == "Male",
    mortality_table$Male[age_match],
    mortality_table$Female[age_match]
  )
  
  # Smoking adjustment matrix
  smoke_adj <- matrix(c(
    smoker_rates$Non.smoker[smoker_rates$Gender=="Male"], smoker_rates$Smoker[smoker_rates$Gender=="Male"], smoker_rates$Ex.Smoker[smoker_rates$Gender=="Male"],
    smoker_rates$Non.smoker[smoker_rates$Gender=="Female"], smoker_rates$Smoker[smoker_rates$Gender=="Female"], smoker_rates$Ex.Smoker[smoker_rates$Gender=="Female"]
  ), nrow=2, byrow=TRUE)
  
  sex_index <- ifelse(active_policies$sex=="Male", 1, 2)
  smoke_index <- match(active_policies$smoker.status, c("Non-smoker","Smoker","Ex-smoker"))
  
  # Applying the smoking adjustment to the base mortality rates
  active_policies$adjusted_mortality <- active_policies$base_mortality * smoke_adj[cbind(sex_index, smoke_index)]
  
  # Simulate deaths in the given year
  active_policies$died <- rbinom(nrow(active_policies), 1, active_policies$adjusted_mortality)
  
  # Output table for policyholders who died in the year
  death_records <- active_policies[active_policies$died == 1, 
                                   c("policy.number", "full_name", "age", "smoker.status", "sum.assured")]
  colnames(death_records) <- c("Policy Number", "Name", "Age at Death", "Smoking Status", "Sum Assured")
  
  return(death_records)
}



