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

