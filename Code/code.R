## ----setup, include=FALSE----------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = FALSE)

# load libraries
library(readxl)
library(tidyverse)
library(knitr)
library(kableExtra)
library(scales)
library(dplyr)
library(tidyr)
library(ggplot2)


## ----data_load---------------------------------------------------------------------------------------------------------------------------------
# define column names
student_cols <- c("On_campus", "Days_fall_spring", "Days_jterm", "Days_summer",
                  "Walk", "Drive_gas", "Drive_electric", "Bike_or_nonmotor", "Bus",
                  "Carpool", "Motorcycle_or_scooter", "Other", "Miles_to_campus")
employee_cols <- c("Days_commute_fall_spring", "Days_remote_fall_spring",
                   "Days_commute_summer_winter", "Days_remote_summer_winter", "Gas_100",
                   "Drive_gas", "Drive_electric", "Walk", "Bike_or_nonmotor", "Bus",
                   "Carpool", "Motorcycle_or_scooter", "Other", "Miles_to_campus")

# define vector of columns related to apportionment of transportation modes
modes_of_transp <- c("Drive_gas", "Drive_electric", "Walk", "Bike_or_nonmotor",
                     "Bus", "Carpool", "Motorcycle_or_scooter", "Other")

# read in student data - keep track of response indices for identifying problematic records
student_df <- read_excel(path = "Miami_Commuter_Analysis_FY25.xlsx",
                         sheet = "Student Calculations",
                         col_names = student_cols,
                         skip = 1) %>%
  mutate(`Response #` = 1:n()) %>%
  relocate(`Response #`) %>%
  relocate(Walk, .after = Drive_electric)

# read in employee data - keep track of response indices for identifying problematic records
employee_df <- read_excel(path = "Miami_Commuter_Analysis_FY25.xlsx",
                          sheet = "Employee Calculations",
                          col_names = employee_cols,
                          skip = 1) %>%
  mutate(`Response #` = 1:n()) %>%
  relocate(`Response #`)


## ----data_vald---------------------------------------------------------------------------------------------------------------------------------
# incomplete response - there were no incomplete responses for students
incomp_resp <- employee_df %>%
  filter(is.na(Gas_100)) %>%
  mutate(Issue = "Incomplete Response",
         Domain = "Employee") %>%
  select(Domain, `Response #`, Issue)

# invalid apportionment
invald_appmnt_student <- student_df %>%
  filter(rowSums(select(., all_of(modes_of_transp)), na.rm = TRUE) != 100) %>%
  mutate(Issue = "Invalid Apportionment",
         Domain = "Student") %>%
  select(Domain, `Response #`, Issue)

invald_appmnt_employee <- employee_df %>%
  filter(Gas_100 == "No") %>%
  filter(rowSums(select(., all_of(modes_of_transp)), na.rm = TRUE) != 100) %>%
  mutate(Issue = "Invalid Apportionment",
         Domain = "Employee") %>%
  select(Domain, `Response #`, Issue)

invald_appmnt <- rbind(invald_appmnt_student,
                       invald_appmnt_employee)

# contradictory apportionment
contrad_appmnt <- employee_df %>%
  filter(Gas_100 == "No" & Drive_gas == 100) %>%
  mutate(Issue = "Contradictory Apportionment",
         Domain = "Employee") %>%
  select(Domain, `Response #`, Issue)

# invalid week
invald_wk <- employee_df %>%
  filter((Days_commute_fall_spring + Days_remote_fall_spring > 5) |
          Days_commute_summer_winter + Days_remote_summer_winter > 5 |
          Days_commute_fall_spring + Days_commute_summer_winter == 0) %>%
  mutate(Issue = "Invalid Week",
         Domain = "Employee") %>%
  select(Domain, `Response #`, Issue)

# combine issues in single dataframe
issues_df <- rbind(incomp_resp,
                   invald_appmnt,
                   contrad_appmnt,
                   invald_wk) %>%
  arrange(desc(Domain), `Response #`) %>%
  mutate(Issue = factor(Issue,
                        levels = c("Incomplete Response", "Invalid Apportionment",
                                   "Contradictory Apportionment", "Invalid Week")),
         Domain = factor(Domain,
                         levels = c("Student", "Employee")))

# contingency table of issues
kable(table(issues_df$Issue, issues_df$Domain),
      caption = "Table 1: Counts of Survey Response Issues") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)


## ----incomp_resp-------------------------------------------------------------------------------------------------------------------------------
### remove incomplete responses ###
employee_clean <- employee_df %>%
  filter(!is.na(Gas_100))

# did not occur in student data


## ----invald_appmnt-----------------------------------------------------------------------------------------------------------------------------
### remove invalid apportionments ###
# for employee data
employee_clean <- employee_clean %>%
  filter(
    # keep if Gas_100 = "Yes" because otherwise the NAs will affect the next condition
    if_all(all_of(modes_of_transp), is.na) |
    # OR keep if sum == 100, including partially incomplete responses
    (rowSums(select(., all_of(modes_of_transp)), na.rm=TRUE) == 100))

# for student data
student_clean <- student_df %>%
  # keep if sum == 100, including partially incomplete responses
  filter(rowSums(select(., all_of(modes_of_transp)), na.rm=TRUE) == 100)


## ----contrad_appmnt----------------------------------------------------------------------------------------------------------------------------
### adjust contradictory apportionment ###
employee_clean <- employee_clean %>%
  mutate(Gas_100 = ifelse((Gas_100 == "No" & as.numeric(Drive_gas) == 100), "Yes", Gas_100)) %>%
  mutate(Gas_100 = ifelse(`Response #` == 126, "No", Gas_100)) # manually fix response #126

# not relevant for student data


## ----invald_wk---------------------------------------------------------------------------------------------------------------------------------
### remove remote work information ###
employee_clean <- employee_clean %>%
  select(-Days_remote_fall_spring, -Days_remote_summer_winter) %>%
  filter(Days_commute_fall_spring + Days_commute_summer_winter != 0)

# not relevant to student data


## ----100_impute--------------------------------------------------------------------------------------------------------------------------------
### impute Drive_gas to be 100 and all others to be 0 for Gas_100 == "Yes"
employee_clean <- employee_clean %>%
  mutate(Drive_gas             = ifelse(Gas_100 == "Yes", 100, Drive_gas),
         Drive_electric        = ifelse(Gas_100 == "Yes", 0, Drive_electric),
         Walk                  = ifelse(Gas_100 == "Yes", 0, Walk),
         Bike_or_nonmotor      = ifelse(Gas_100 == "Yes", 0, Bike_or_nonmotor),
         Bus                   = ifelse(Gas_100 == "Yes", 0, Bus),
         Carpool               = ifelse(Gas_100 == "Yes", 0, Carpool),
         Motorcycle_or_scooter = ifelse(Gas_100 == "Yes", 0, Motorcycle_or_scooter),
         Other                 = ifelse(Gas_100 == "Yes", 0, Other))

# not relevant to student data


## ----zero_impute-------------------------------------------------------------------------------------------------------------------------------
### impute zeros for remaining NAs
# for employee data
employee_clean <- employee_clean %>%
  mutate(across(all_of(modes_of_transp), ~ replace_na(.x, 0)))

# for student data
student_clean <- student_clean %>%
  mutate(across(all_of(modes_of_transp), ~ replace_na(.x, 0))) %>%
  mutate(Days_jterm = if_else(is.na(Days_jterm), 0, Days_jterm),
         Days_summer = if_else(is.na(Days_summer), 0, Days_summer))


## ----fig.fullwidth=TRUE, fig.height=2.5--------------------------------------------------------------------------------------------------------
# build a clean distance frame with group labels
dist_df <- bind_rows(
  employee_clean %>%
    transmute(Domain = "Employee",
              Miles_to_campus = as.numeric(Miles_to_campus)),
  student_clean %>%
    transmute(Domain = "Student",
              Miles_to_campus = as.numeric(Miles_to_campus))
) %>%
  filter(!is.na(Miles_to_campus))

# Boxplot by group
ggplot(dist_df, aes(x = Domain, y = Miles_to_campus, fill = Domain)) +
  geom_boxplot(width = 0.6, outlier.alpha = 0.5, outliers = FALSE) +
  geom_jitter(alpha = 0.2, width = 0.1) +
  coord_flip() +
  labs(
    caption = "Figure 1: One remaining employee reported an implausible distance from campus.",
    x = NULL,
    y = "Miles to Campus"
  ) +
  theme_classic() +
  theme(plot.caption = element_text(size = 10, hjust = 0),
        legend.position = "none")


## ----------------------------------------------------------------------------------------------------------------------------------------------
### remove implausible distances
# for employee data
employee_clean <- employee_clean %>%
  filter(Miles_to_campus <= 100)

# for student data
student_clean <- student_clean %>%
  filter(Miles_to_campus <= 100)


## ----count_table-------------------------------------------------------------------------------------------------------------------------------
# summarize student data
student_sum <- student_clean %>%
  summarize(`n (original)` = nrow(student_df),
            `n (adjusted)` = n(),
            `N` = 943,
            `Response Rate (original)` = `n (original)`/`N`*100,
            `Response Rate (adjusted)` = `n (adjusted)`/`N`*100,
            Drive_gas = mean(Drive_gas),
            Drive_electric = mean(Drive_electric),
            Walk = mean(Walk),
            Bike_or_nonmotor = mean(Bike_or_nonmotor),
            Bus = mean(Bus),
            Carpool = mean(Carpool),
            Motorcycle_or_scooter = mean(Motorcycle_or_scooter),
            Other = mean(Other))

# summarize employee data
employees_sum <- employee_clean %>%
  summarize(`n (original)` = nrow(employee_df),
            `n (adjusted)` = n(),
            `N` = 2802,
            `Response Rate (original)` = `n (original)`/`N`*100,
            `Response Rate (adjusted)` = `n (adjusted)`/`N`*100,
            Drive_gas = mean(Drive_gas),
            Drive_electric = mean(Drive_electric),
            Walk = mean(Walk),
            Bike_or_nonmotor = mean(Bike_or_nonmotor),
            Bus = mean(Bus),
            Carpool = mean(Carpool),
            Motorcycle_or_scooter = mean(Motorcycle_or_scooter),
            Other = mean(Other))

# combine dataframes and provide row names
sum <- as.data.frame(rbind(student_sum, employees_sum)) %>%
  mutate(across(4:13, ~ paste0(round(.x, 2), "%")))
rownames(sum) <- c("Student", "Employee")

# display table
kable(select(sum, 1:5), digits = 2, align = "r",
      caption = "Table 2: Summary of Cleaned Data")


## ----------------------------------------------------------------------------------------------------------------------------------------------
# display table
kable(select(sum, 6:13), digits = 2, align = "r",
      caption = "Table 3: Average Apportionment of Transportation Modes")


## ----constants---------------------------------------------------------------------------------------------------------------------------------
### DEFINE OUR CONSTANTS/KNOWN VALUES
# sample sizes
n1 <- nrow(student_clean)
n2 <- nrow(employee_clean)
n <- n1 + n2

# population sizes
N1 <- 943
N2 <- 2802
N <- N1 + N2

# bootstrap sample sizes
k1 <- round(N1 / n1)
k2 <- round(N2 / n2)

# number of weeks per term
weeks <- c(Fall = 15, Spring = 15, JTerm = 4, Summer = 12)

# miles per gallon
mpg_const <- 26

# metric tons CO2 per gallon (EPA tailpipe factor)
co2_per_gal <- 0.0089


## ----annual_info-------------------------------------------------------------------------------------------------------------------------------
# add annual information - student
student_annual <- student_clean %>%
  mutate(annual_days = Days_fall_spring * weeks["Fall"] +
                       Days_fall_spring * weeks["Spring"] +
                       Days_jterm * weeks["JTerm"] + 
                       Days_summer * weeks["Summer"],
         annual_miles = 2 * Miles_to_campus * annual_days,
         annual_gas_miles = annual_miles * Drive_gas/100,
         annual_gallons = annual_gas_miles / mpg_const,
         annual_CO2 = annual_gallons * co2_per_gal)

# add annual information - employee
employee_annual <- employee_clean %>%
  mutate(annual_days = Days_commute_fall_spring * weeks["Fall"] +
                       Days_commute_fall_spring * weeks["Spring"] +
                       Days_commute_summer_winter * weeks["JTerm"] + 
                       Days_commute_summer_winter * weeks["Summer"],
         annual_miles = 2 * Miles_to_campus * annual_days,
         annual_gas_miles = annual_miles * Drive_gas/100,
         annual_gallons = annual_gas_miles / mpg_const,
         annual_CO2 = annual_gallons * co2_per_gal)


## ----bootstrap_w_poststratification------------------------------------------------------------------------------------------------------------
# define "bootstrap populations"
student_bs_pop <- student_annual[rep(1:nrow(student_annual), each = k1), ]
employee_bs_pop <- employee_annual[rep(1:nrow(employee_annual), each = k2), ]

### ALGORITHM TO PERFORM ESTIMATION ###
estimate <- function(var) {
  # set seed for reproducibility
  set.seed(12345)

  # initialize vectors
  T_hats_s <- c()
  T_hats_e <- c()
  T_hats <- c()
  
  # loop to obtain many many T_hats
  for(i in 1:10000) {
    # draw a "bootstrap sample"
    student_bs_sample <- student_bs_pop[sample(1:nrow(student_bs_pop), size = n1), ]
    employee_bs_sample <- employee_bs_pop[sample(1:nrow(employee_bs_pop), size = n2), ]
    
    # compute T_hat (for students and employees separately, then combine)
    T_hats_s[i] <- N1 * mean(student_bs_sample[[var]])
    T_hats_e[i] <- N2 * mean(employee_bs_sample[[var]])
    T_hats[i] <- T_hats_s[i] + T_hats_e[i]
  }
  
  # T_hat; THIS IS OUR ESTIMATE OF THE TOTAL
  T_hat_s <- mean(T_hats_s)
  T_hat_e <- mean(T_hats_e)
  T_hat <- mean(T_hats)
  
  # compute s1 and s2
  s1 <- sd(student_annual[[var]])
  s2 <- sd(employee_annual[[var]])
  
  # V_hat_T_hat; THIS IS OUR ESTIMATE OF THE VARIANCE
  V_hat_T_hat_s <- (1-n1/N1) * (N1*(n1-1))/((N1-1)*n1) * N1^2 * (s1^2)/n1
  V_hat_T_hat_e <- (1-n2/N2) * (N2*(n2-1))/((N2-1)*n2) * N2^2 * (s2^2)/n2
  V_hat_T_hat <- V_hat_T_hat_s + V_hat_T_hat_e
  
  # SE_T_hat; THIS IS THE ESTIMATED STANDARD ERROR OF T_HAT
  SE_T_hat_s <- sd(T_hats_s)
  SE_T_hat_e <- sd(T_hats_e)
  SE_T_hat <- sqrt(V_hat_T_hat)
  sd_T_hat <- sd(T_hats) # double check via the simulation results
  
  # 95% CIs
  ME_s <- 1.96 * SE_T_hat_s
  ME_e <- 1.96 * SE_T_hat_e
  ME <- 1.96 * SE_T_hat
  CI_low_s <- T_hat_s - ME_s
  CI_low_e <- T_hat_e - ME_e
  CI_low <- T_hat - ME
  CI_high_s <- T_hat_s + ME_s
  CI_high_e <- T_hat_e + ME_e
  CI_high <- T_hat + ME
  
  # return df of results
  results <- data.frame(Estimate = c(T_hat_s, T_hat_e, T_hat),
                        SE = c(SE_T_hat_s, SE_T_hat_e, SE_T_hat),
                        `95_CI_low` = c(CI_low_s, CI_low_e, CI_low),
                        `95_CI_high` = c(CI_high_s, CI_high_e, CI_high)) %>%
    mutate(across(where(is.numeric), ~ format(round(.x, 1), big.mark = ",", nsmall = 1)))
  rownames(results) <- c("Students", "Employees", "Total")
  
  return(results)
}

# run algorithm for each outcome variable
miles_est <- estimate("annual_gas_miles")
gallons_est <- estimate("annual_gallons")
CO2_est <- estimate("annual_CO2")


## ----------------------------------------------------------------------------------------------------------------------------------------------
# print results
kable(miles_est, align = "r",
      caption = "Table 4: Final Estimates of Total Gas-Consuming Commuting Mileage",
      col.names = c("Estimate", "SE", "95% CI Low", "95% CI High")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(nrow(miles_est), bold = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------------------
kable(gallons_est, align = "r",
      caption = "Table 5: Final Estimates Gasoline Consumption (gal)",
      col.names = c("Estimate", "SE", "95% CI Low", "95% CI High")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(nrow(gallons_est), bold = TRUE)


## ----------------------------------------------------------------------------------------------------------------------------------------------
kable(CO2_est, align = "r",
      caption = "Table 6: Final Estimates of C02 Emissions (MTCO2e)",
      col.names = c("Estimate", "SE", "95% CI Low", "95% CI High")) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  row_spec(nrow(CO2_est), bold = TRUE)


## ----warning=FALSE, message=FALSE--------------------------------------------------------------------------------------------------------------
# list all issues
kable(issues_df) %>%
  kable_styling(bootstrap_options = "striped") %>%
  scroll_box(height = "400px")

