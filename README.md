Code for Group Coursework 3

**Actuarial Policyholder Management & Simulation System**

This repository contains an R-based system developed for Actuarial Policyholder Management and Simulations. The code covers policy data validation, policyholder maintenance, financial summarisation, and mortality simulation. The system is structured into four main parts, with shared helper functions used throughout to ensure consistency and robustness.

**PROJECT STRUCTURE**

The system is organised into the following logical sections:

Part A: Detecting Errors
Part B: Policyholder Maintenance
Part C: Summarising the Data
Part D: Simulating Deaths

All code is written in base R, making extensive use of vectorised operations and apply-family functions where appropriate.

**ASSUMPTIONS**

• All policyholders are assumed to be born on 1 January
• Policies exiting during a year are considered active at the start of that year
• Data is assumed to be correct as of 01/01/2026
• Interest rates are annual effective rates
• Input CSV files follow the expected column structure

**SHARED HELPER FUNCTIONS**

capitalise_first(x)
Capitalises the first letter of a string and lowercases the remainder. This ensures consistent formatting of names, sex, and smoker status throughout the dataset.

is_active_at_year(row, year)
Returns TRUE if a policy was active at the start of the given year. The function correctly handles inception year, exit year, and policies that exit during the year (which are still considered active at the start of that year).

**PART A: DETECTING ERRORS**

check_policy_errors(raw_file, output_file)

This function validates a raw policy dataset and identifies common data issues. The following checks are performed:

• Age at inception must be between 65 and 75
• Policy term must not take the policyholder into their 90th year
• Smoker status must be valid
• No policy can have exited in the data year (2026)
• Exit year cannot be before inception year
• Inception year cannot be in the future
• Policies marked as “End” must have completed their full term

All detected errors are written to a CSV file and also returned as a data frame.

**PART B: POLICYHOLDER MAINTENANCE**

add_policy(policy.number, first.name, surname, inception.year, age.at.inception, premium, sum.assured, term, sex, smoker.status, file)
Adds a new policyholder to the dataset after performing full validation. The function ensures valid ages, terms, premiums, and sum assured values, prevents duplicate policy numbers, standardises categorical fields, and returns informative error messages if validation fails.

record_deaths(policy_numbers, year, file)
Records deaths for a set of policy numbers. The function checks that policies exist, have not already exited, and were active at the start of the given year. Vectorised logic is used and overwriting of existing exits is prevented.

record_withdrawals(policy_numbers, year, file)
Records withdrawals using the same validation logic as deaths. The spelling “Withdrawl” is retained to remain consistent with the supplied data files.

settle_matured_policies(year, file)
Automatically settles policies that mature at the end of the specified year, provided that the full policy term has been completed and no previous exit has been recorded.

**PART C: SUMMARISING THE DATA**

build_summary_table(df, value_col, result_label)
An internal helper function that creates summary tables for numeric values. Results are split into:

• All policies
• By sex
• By smoker status
• By sex and smoker status

This function is reused to ensure consistency across summaries.

premium_summary(year, file)
Calculates the total premium paid during a given year. Only policies active at the start of the year are included. Results are returned in a small data frame with the required breakdowns.

death_benefit_summary(year, file)
Calculates total death benefits paid in a given year. Only policies with exit reason “Death” and a matching year of exit are included. The breakdown structure matches that of the premium summary.

policy_cashflows(policy.number, year, interest.rate, file)
Calculates cashflows for an individual policy. Outputs include nominal premiums paid, nominal death benefits received, present value of premiums, and present value of benefits. Premiums are assumed to be paid at the start of each year, and death benefits are assumed to be paid mid-year.

**PART D: SIMULATING DEATHS**

simulate_deaths(sim_year, policy_file)
Simulates deaths among active policyholders in a given year using age-based mortality rates, sex-based mortality, and smoker-status adjustments. Deaths are simulated using Bernoulli trials. The function returns a table of simulated deaths, including policy number, name, age at death, smoking status, and sum assured.
