library(knitr)
knitr::opts_chunk$set(echo = T,  warning = FALSE, message = FALSE)
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)
# Libraries used
library(tidyverse)
library(haven)
library(dplyr)
library(readxl)
library(GGally)
library(forcats)
library(stargazer)
library(lme4)
library(car)
library(rstudioapi)
library(knitr)
library(kableExtra)
# Turning off scientific notation
options(scipen = 9999)
# Bespoke functions used ----
PointPlot <- function(dataset, independent_var, dependent_var, colour_var = NULL, title = NULL, subtitle = NULL, caption = NULL, x_label = "X variable", y_label = "Y variable", point_size = 1, line_intercept = NULL, line_slope = NULL, line_colour = "black", line_linewidth = 1, line_linetype = "solid", add_regression_line = FALSE) {
  
  # Loading required libraries
  library(ggplot2)
  library(showtext)
  
  # Enable showtext to render text
  showtext_auto()
  
  font_add_google("Roboto")
  
  # Adjust the aesthetic mapping based on the presence of the third variable
  if(!is.null(colour_var)) {
    plot <- ggplot(dataset, aes_string(x = independent_var, y = dependent_var, colour = colour_var))
  } else {
    plot <- ggplot(dataset, aes_string(x = independent_var, y = dependent_var))
  }
  
  # Base point plot
  plot <- plot + geom_point(alpha = 1, shape = 19, size = point_size, position = 'jitter') +
    theme_classic() +
    theme(plot.title = element_text(colour = 'black', size = 17.5, face = 'bold',
                                    family = 'Roboto'),
          plot.subtitle = element_text(colour = 'grey30', size = 12.5, face = 'italic',
                                       family = 'Roboto'),
          plot.caption = element_text(colour = 'grey30', size = 10, face = 'italic',
                                      family = 'Roboto'),
          axis.title = element_text(family = 'Roboto'),
          axis.text = element_text(size = 15.5,
                                   family = 'Roboto',
                                   colour = 'black'),
          legend.position = ifelse(is.null(colour_var), 'none', 'right'),
          legend.text = element_text(family = 'Roboto')) + 
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = x_label,
         y = y_label)
  
  # Add manual line if intercept and slope are provided
  if (!is.null(line_intercept) & !is.null(line_slope)) {
    plot <- plot + geom_abline(intercept = line_intercept, 
                               slope = line_slope, 
                               colour = line_colour, 
                               linewidth = line_linewidth, 
                               linetype = line_linetype)
  }
  
  # Add regression line with confidence interval if requested
  if (add_regression_line) {
    plot <- plot + geom_smooth(method = "lm", alpha = 0.2)
  }
  
  # Create a filename from the plot title
  file_name <- paste0(gsub(" ", "_", plot$labels$title), ".png")
  
  # Save the plot to the working directory
  ggsave(filename = file_name, plot = plot, width = 1000/100, height = 617/100, dpi = 100)
  
  return(plot)
}

DistPlot <- function(dataset, variable, formatted_name, subtitle = NULL, caption = NULL, y_label = NULL) {
  # Loading required libraries
  library(ggplot2)
  library(showtext)
  
  # Enable showtext to render text
  showtext_auto()
  
  font_add_google("Roboto")
  
  plot <- ggplot(dataset, aes_string(x = variable)) +
    geom_histogram(aes(y = ..density..),
                   colour = 1, fill = "white") +
    geom_density(fill = '#0072C6', colour = '#0072C6', alpha = 0.25) +
    theme_classic() +
    theme(plot.title = element_text(colour = 'black', size = 17.5, face = 'bold',
                                    family = 'Roboto'),
          plot.subtitle = element_text(colour = 'grey30', size = 12.5, face = 'italic',
                                       family = 'Roboto'),
          plot.caption = element_text(colour = 'grey30', size = 12.5, face = 'italic',
                                      family = 'Roboto'),
          axis.title.x = element_blank(),  # This removes the x-axis title
          axis.title.y = element_text(size = 15.5,
                                      family = 'Roboto',
                                      colour = 'black'),
          axis.text = element_text(size = 15.5,
                                   family = 'Roboto',
                                   colour = 'black'),
          legend.position = 'none') +
    labs(title = paste0("Distribution of ", formatted_name),
         subtitle = subtitle,
         caption = caption,
         y = y_label)
  
  # Create a filename from the plot title
  file_name <- paste0(gsub(" ", "_", plot$labels$title), ".png")
  
  # Save the plot to the working directory
  ggsave(filename = file_name, plot = plot, width = 1000/100, height = 617/100, dpi = 100)
  
  plot
}

ConfidencePlot <- function(data, title, subtitle, middle_point = 0, x_label = "Estimated Coefficient", caption = NULL) {
  library(ggplot2)
  library(showtext)
  
  # Enable showtext to render text
  showtext_auto()
  
  font_add_google("Roboto")
  
  if (!middle_point %in% c(0, 1)) {
    stop("middle_point must be 0 or 1")
  }
  
  conf_plot <- ggplot(data, aes(x = coefs, y = names)) +
    geom_errorbar(aes(xmin = lower, xmax = upper),
                  col = ifelse((data$lower < middle_point & data$upper > middle_point), "grey70", "forestgreen"),
                  width = 0.5, size = 1, alpha = 0.5) +
    geom_point(col = ifelse((data$lower < middle_point & data$upper > middle_point), "grey70", "black"), size = 1, alpha = 1) +
    geom_vline(xintercept = middle_point, col = 'red', size = 0.5, linetype="dashed") +
    theme_classic() +
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = x_label,
         y = "") +
    theme(axis.text = element_text(size = 15.5,
                                   family = 'Roboto',
                                   colour = 'black'),
          axis.title = element_text(family = 'Roboto', size = 15.5),
          plot.title = element_text(hjust = 0, colour = 'black', size = 17.5, face = 'bold',
                                    family = 'Roboto', margin = margin(b = 10)),
          plot.subtitle = element_text(hjust = 0, colour = 'grey30', size = 12.5, face = 'italic',
                                       family = 'Roboto', margin = margin(b = 5)),
          plot.caption = element_text(colour = 'grey30', size = 10, face = 'italic',
                                      family = 'Roboto'))
  
  # Create a filename from the plot title
  file_name <- paste0(gsub(" ", "_", conf_plot$labels$title), ".png")
  
  # Save the plot to the working directory
  ggsave(filename = file_name, plot = conf_plot, width = 1200/100, height = 740/100, dpi = 100)  # Adjust height as necessary
  
  return(conf_plot)
}

PartialRegressionPlot <- function(model, variable, x_var_name = "X1", y_var_name = "Dependent Variable", title = NULL) {
  # Close any existing plotting device
  graphics.off()
  
  # Construct axis labels using the specified names
  xlab <- paste("Residuals of", x_var_name, "Controlling for Other Variables")
  ylab <- paste("Residuals of", y_var_name, "(Excluding", x_var_name, ")")
  
  # Create the plot without a main title
  avPlots(model, variable, xlab = xlab, ylab = ylab)
  
  # Add a title using the title function
  title(main = title)
}

HomoskedasticityPlot <- function(model, title = "Homoskedasticity Check", subtitle = NULL, 
                                 x_label = "Fitted Values", y_label = "Standardized Residuals", 
                                 point_size = 1.2, add_loess_line = FALSE) {
  
  # Loading required libraries
  library(ggplot2)
  library(showtext)
  library(lmtest) # For Breusch-Pagan test
  
  # Enable showtext to render text
  showtext_auto()
  
  font_add_google("Roboto")
  
  # Calculate standardized residuals
  std_residuals <- rstandard(model)
  
  # Fitted values
  fitted_values <- fitted(model)
  
  # Create a data frame for plotting
  plot_data <- data.frame(FittedValues = fitted_values, StdResiduals = std_residuals)
  
  # Perform Breusch-Pagan test
  bp_test <- bptest(model)
  
  # Format the p-value for the caption
  if (bp_test$p.value < 0.05) {
    p_value_text <- "p < 0.05"
  } else if (bp_test$p.value > 0.05) {
    p_value_text <- "p > 0.05"
  } else {
    p_value_text <- "p = 0.05"
  }
  
  # Create a caption including the formatted p-value text
  caption <- paste("Breusch-Pagan Test:", p_value_text)
  
  # Base point plot with adjusted sizes for readability
  plot <- ggplot(plot_data, aes(x = FittedValues, y = StdResiduals)) +
    geom_point(alpha = 0.5, shape = 19, size = point_size, position = 'jitter') +
    theme_classic() +
    theme(plot.title = element_text(colour = 'black', size = 50, face = 'bold', family = 'Roboto'),
          plot.subtitle = element_text(colour = 'grey30', size = 30, face = 'italic', family = 'Roboto'),
          plot.caption = element_text(colour = 'grey30', size = 50, face = 'italic', family = 'Roboto'),
          axis.title = element_text(size = 30, family = 'Roboto'),
          axis.text = element_text(size = 30, family = 'Roboto', colour = 'black'),
          legend.position = 'none') + 
    labs(title = title, subtitle = subtitle, caption = caption, x = x_label, y = y_label)
  
  # Add loess smoothing line if requested
  if (add_loess_line) {
    plot <- plot + geom_smooth(method = "loess", alpha = 0.2)
  }
  
  # Create a filename from the plot title
  file_name <- paste0(gsub(" ", "_", plot$labels$title), ".png")
  
  # Save the plot with dimensions that balance size and readability
  ggsave(filename = file_name, plot = plot, width = 5.91, height = 4.50, dpi = 300)
  
  return(plot)
}

HomoskedasticityDetailPlot <- function(model, dataframe, categorical_var_name, 
                                       title = "Homoskedasticity Check", subtitle = NULL, 
                                       x_label = "Fitted Values", y_label = "Standardized Residuals", 
                                       point_size = 1.2, add_loess_line = FALSE) {
  
  # Loading required libraries
  library(ggplot2)
  library(showtext)
  library(lmtest) # For Breusch-Pagan test
  
  # Enable showtext to render text
  showtext_auto()
  
  font_add_google("Roboto")
  
  # Calculate standardized residuals
  std_residuals <- rstandard(model)
  
  # Fitted values
  fitted_values <- fitted(model)
  
  # Ensure the categorical variable exists in the dataframe
  if (!categorical_var_name %in% names(dataframe)) {
    stop("Specified categorical variable not found in the provided dataframe.")
  }
  
  # Create a data frame for plotting
  plot_data <- data.frame(FittedValues = fitted_values, 
                          StdResiduals = std_residuals, 
                          Category = dataframe[[categorical_var_name]])
  
  # Perform Breusch-Pagan test
  bp_test <- bptest(model)
  
  # Format the p-value for the caption
  p_value_text <- ifelse(bp_test$p.value < 0.05, "p < 0.05",
                         ifelse(bp_test$p.value > 0.05, "p > 0.05", "p = 0.05"))
  
  # Create a caption including the formatted p-value text
  caption <- paste("Breusch-Pagan Test:", p_value_text)
  
  # Base point plot with adjusted sizes for readability
  plot <- ggplot(plot_data, aes(x = FittedValues, y = StdResiduals, color = Category)) +
    geom_point(alpha = 0.5, shape = 19, size = point_size, position = 'jitter') +
    theme_classic() +
    theme(plot.title = element_text(colour = 'black', size = 17.5, face = 'bold', family = 'Roboto'),
          plot.subtitle = element_text(colour = 'grey30', size = 12.5, face = 'italic', family = 'Roboto'),
          plot.caption = element_text(colour = 'grey30', size = 10, face = 'italic', family = 'Roboto'),
          axis.title = element_text(size = 15.5, family = 'Roboto'),
          axis.text = element_text(size = 15.5, family = 'Roboto', colour = 'black'),
          legend.position = 'right', legend.text = element_text(size = 12.5, family = 'Roboto')) + 
    labs(title = title, subtitle = subtitle, caption = caption, x = x_label, y = y_label)
  
  # Add loess smoothing line if requested
  if (add_loess_line) {
    plot <- plot + geom_smooth(method = "loess", alpha = 0.2, se = FALSE)
  }
  
  # Create a filename from the plot title
  file_name <- paste0(gsub(" ", "_", plot$labels$title), ".png")
  
  # Save the plot with dimensions that balance size and readability
  ggsave(filename = file_name, plot = plot, width = 10, height = 6.17, dpi = 100)
  
  return(plot)
}

QQPlotModel <- function(model, title = 'Normality of Residuals Check', subtitle = 'QQ-Plot', caption = NULL) {
  
  # Loading required libraries
  library(ggplot2)
  
  # Extract residuals from the model
  model_residuals <- residuals(model)
  
  # Create the QQ-plot
  plot <- ggplot() +
    stat_qq(aes(sample = model_residuals)) +
    stat_qq_line(aes(sample = model_residuals), colour = "blue") +
    theme_classic() +
    theme(plot.title = element_text(colour = 'black', size = 17.5, face = 'bold'),
          plot.subtitle = element_text(colour = 'grey30', size = 12.5, face = 'italic'),
          plot.caption = element_text(colour = 'grey30', size = 10, face = 'italic'),
          axis.title = element_text(),
          axis.text = element_text(size = 15.5, colour = 'black')) + 
    labs(title = title,
         subtitle = subtitle,
         caption = caption,
         x = "Theoretical Quantiles",
         y = "Sample Quantiles")
  
  # Create a filename from the plot title or use a default one
  file_name <- if (!is.null(title)) {
    paste0(gsub(" ", "_", title), ".png")
  } else {
    "QQPlot.png"
  }
  
  # Save the plot to the working directory
  ggsave(filename = file_name, plot = plot, width = 1000/100, height = 617/100, dpi = 100)
  
  return(plot)
}
AHAH_df <- read.csv('Group_7_AHAH_V3_0.csv') # Access to healthy assets and hazards V3 data
IOD_income_df <- read.csv('Group_7_IoD2019_Income.csv') # Index of Multiple Deprivation Income Domian Indicator Data (measured in 2015)
IOD_health_df <- read.csv('Group_7_IoD2019_Health.csv') # Index of Multiple Deprivation Health Domian Indicator Data
IOD_barriers_df <- read.csv('Group_7_IoD2019_Barriers.csv') # Index of Multiple Deprivation Barriers to Housing and Services Domain Indicator Data
IOD_education_df <- read.csv('Group_7_IoD2019_Education.csv') # Index of Multiple Deprivation Education, Skills and Training Deprivation Domain Indicator Data
IOD_employment_df <- read.csv('Group_7_IoD2019_Employment.csv') # Index of Multiple Deprivation Employment Deprivation Domain Indicator Data (measured in 2015)
ONS_MIDYEAR_2015 <- read.csv('Group_7_ONS_MIDYEAR_2015.csv') # Mid-year population estimates for 2015 in England and Wales (for creating rates from the numerator values given in the income and employment measures)
RUC2011 <- read.csv('Group_7_RUC2011.csv')
## Renaming columns for consistent joining
AHAH_df <- AHAH_df %>% rename("LSOA.code..2011." = lsoa11)
ONS_MIDYEAR_2015 <- ONS_MIDYEAR_2015 %>% rename("LSOA.code..2011." = Area.Codes)
RUC2011 <- RUC2011 %>% rename("LSOA.code..2011." = LSOA11CD)

## Creating complete combined dataset
complete_combined_data <- IOD_barriers_df %>%
  left_join(IOD_education_df, by = "LSOA.code..2011.") %>%
  left_join(IOD_employment_df, by = "LSOA.code..2011.") %>%
  left_join(IOD_health_df, by = "LSOA.code..2011.") %>%
  left_join(IOD_income_df, by = "LSOA.code..2011.") %>%
  left_join(AHAH_df, by = "LSOA.code..2011.") %>%
  left_join(ONS_MIDYEAR_2015, by = "LSOA.code..2011.") %>%
  left_join(RUC2011, by = "LSOA.code..2011.")

## Retaining only columns of interest
combined_data <- complete_combined_data %>% 
  select(
    LSOA.code..2011.,
    Local.Authority.District.name..2019.,
    Income.Domain.numerator,
    Years.of.potential.life.lost.indicator,
    Housing.affordability.indicator,
    Staying.on.in.education.post.16.indicator,
    Employment.Domain.numerator,
    ah3gp, 
    ah3hosp, 
    ah3dent, 
    ah3phar,
    All.Ages,
    RUC11
  )

## Removing commas and converting columns 3:12 to numeric
combined_data <- combined_data %>% 
  mutate(across(.cols = 3:12, 
                .fns = ~ as.numeric(str_remove_all(.x, ","))))

## Creating income and employment deprivation rate variables
combined_data$income_deprivation_rate <- (combined_data$Income.Domain.numerator/combined_data$All.Ages) * 100
combined_data$employment_deprivation_rate <- (combined_data$Employment.Domain.numerator /combined_data$All.Ages) * 100

## Creating average distance (in minutes) to a healthcare service (including GP practice, hospital, dentist and pharmacy)
combined_data$avg_healthcare_distance <- rowMeans(combined_data[, c("ah3gp", "ah3hosp", "ah3dent", "ah3phar")], na.rm = TRUE)

## Renaming column names for understanding
colnames(combined_data) <- c('lsoa_code', 'local_authority_name', 'no._of_income_deprived_individuals_2015', 'years_of_lives_lost_2013_2017', 'housing_affordability_score_2016', 'proportion_of_above16_individuals_inschool_2010_2012', 'no._of_deprived_employment_2015_2016', 'avg_dist_gppractice_in_mins_2022', 'avg_dist_hospital_in_mins_2022', 'avg_dist_dentist_in_mins_2022', 'avg_dist_pharmacy_in_mins_2022', 'population_estimates_for_allages_2015', 'rural_urban_class_2011','income_deprivation_rate_2015','employment_deprivation_rate_2015','avg_healthcare_distance_2022')
## Saving as CSV
write.csv(combined_data, file = "Group_7_combined_data.csv", row.names = FALSE)
write.csv(complete_combined_data, file = "Group_7_complete_combined_data.csv", row.names = FALSE)
## Loading combined data 
combined_data <- read.csv('Group_7_combined_data.csv')

## Checking for duplicates
There_are_duplicates <- any(duplicated(combined_data))
if (There_are_duplicates) {
  cat("There are duplicates in the combined_data.\n")
} else {
  cat("There are no duplicates in combined_data.\n")
}

# Calculate missingness percentages for each numerical variable (IV and DV)
missingness <- sapply(combined_data, function(x) mean(is.na(x)) * 100)
missingness_summary <- data.frame(variable = names(missingness), Missingness_percentage = missingness)
missingness_summary

# Removing Null Values from data set
combined_data <- na.omit(combined_data)

# First 6 rows of data
head(combined_data)

# Last 6 rows of data
tail(combined_data)
summary(combined_data)

# Descriptive table for numerical variables 
stargazer(combined_data, type = "html", digits = 1, title = "Table 1: Summary Statistics", out = "table.html")

# Use rstudioapi to view the table
rstudioapi::viewer("table.html")

## SD of numerical variable
sd(combined_data$lsoa_code)
sd(combined_data$no._of_income_deprived_individuals_2015)
sd(combined_data$years_of_lives_lost_2013_2017)
sd(combined_data$housing_affordability_score_2016)
sd(combined_data$proportion_of_above16_individuals_inschool_2010_2012)
sd(combined_data$no._of_deprived_employment_2015_2016)
sd(combined_data$avg_dist_gppractice_in_mins_2022)
sd(combined_data$avg_dist_hospital_in_mins_2022)
sd(combined_data$avg_dist_dentist_in_mins_2022)
sd(combined_data$avg_dist_pharmacy_in_mins_2022)
sd(combined_data$population_estimates_for_allages_2015)
sd(combined_data$income_deprivation_rate_2015)
sd(combined_data$employment_deprivation_rate_2015)
sd(combined_data$avg_healthcare_distance_2022)
# The number of unique names in the rural_urban_class column 
unique_names <- table(combined_data$rural_urban_class_2011)
print(unique_names)
rural_urban_class_2011_counts <- as.numeric(unique_names)
print(rural_urban_class_2011_counts)

## Summary statistics for Rural_urban_class_2011
max(rural_urban_class_2011_counts)
min(rural_urban_class_2011_counts)
median(rural_urban_class_2011_counts)
IQR(rural_urban_class_2011_counts)

# The number of unique names in the local_authority_name column 
unique_names_1 <- table(combined_data$local_authority_name)
print(unique_names_1)
local_authority_name_counts <- as.numeric(unique_names_1)
print(local_authority_name_counts)

## Summary statistics for local_authority_name
max(local_authority_name_counts)
min(local_authority_name_counts)
median(local_authority_name_counts)
IQR(local_authority_name_counts)

## T-test
# Four sample independent t-test is used to compare the significance level in combined_data data set. Student’s t-test assumes both groups of data are normally distributed and the variances of the two distributions are the same.
x1 <-combined_data$income_deprivation_rate_2015
x2 <-combined_data$employment_deprivation_rate_2015
x3 <-combined_data$housing_affordability_score_2016
y <-combined_data$avg_healthcare_distance_2022

# Plot a 2x2 histogram grid
par(mfrow=c(2, 2))
hist(x1)
hist(x2)
hist(x3)
hist(y)

#Based on histogram plots, we have to perform a Welch's t-test for x1, x2, both of which are skewed, and for x3 as well since DV is highly skewed despite x3 being normally distributed.
t_test1<- t.test(x1,y)    
t_test2<- t.test(x2,y)
t_test3<- t.test(x3,y)
t_test1
t_test2
t_test3

## Covariance values of our primary and secondary IV against primary DV
cov(x1,y)
cov(x2,y)
cov(x3,y)

## Correlation values of our primary and secondary IV against primary DV
# As x1, x2 are not normally distributed, we will use 'spearman' method. We will use 'spearman' method for x3 despite it being normally distributed as DV is highly skewed.
cor(x1,y, method='spearman')
cor(x2,y, method='spearman')
cor(x3,y, method='spearman')
DistPlot(combined_data, "income_deprivation_rate_2015", "Income Deprivation Rate Across LSOAs in England (2015)", subtitle = "Income Deprivation Rate per LSOA (2015) based on Mid-Year Population Estimates", caption = "Data sources: Ministry of Housing, Communities & Local Government, 2019; Office for National Statistics, 2021")

DistPlot(combined_data, "avg_healthcare_distance_2022", "Average Minutes to Healthcare Services Across LSOAs in England (2022)", subtitle = "Mean Access Time to Healthcare Services per LSOA", caption = "Data source: Consumer Data Reasearch Centre (2022)")

DistPlot(combined_data, "housing_affordability_score_2016", "Housing Affordability Indicator Across LSOAs in England (2016)", subtitle = "Higher Scores Indicate Increased Deprivation in Entering Ownership or Private Rental Market", caption = "Data source: Consumer Data Reasearch Centre (2022)")

DistPlot(combined_data, "employment_deprivation_rate_2015", "Employment Deprivation Rate in England (2015)", subtitle = "Employment Deprivation Rate per LSOA (2015) based on Mid-Year Population Estimates", caption = "Data sources: Ministry of Housing, Communities & Local Government, 2019; Office for National Statistics, 2021")

# Both the main explanatory variable and outcome variable have a very strong right skew. Thus, a log transformation in both of these is examined.
## Adding log transformed variables to dataframe:
combined_data$log_avg_healthcare_distance_2022 <- log(combined_data$avg_healthcare_distance_2022)
combined_data$log_income_deprivation_rate_2015 <- log(combined_data$income_deprivation_rate_2015)
combined_data$log_employment_deprivation_rate_2015 <- log(combined_data$employment_deprivation_rate_2015)

## Checking distributions:
DistPlot(combined_data, "log_income_deprivation_rate_2015", 'log(Income Deprivation Rate) Across LSOAs in England (2015)', subtitle = "Income Deprivation Rate per LSOA (2015) based on Mid-Year Population Estimates", caption = "Data sources: Ministry of Housing, Communities & Local Government, 2019; Office for National Statistics, 2021")

DistPlot(combined_data, "log_avg_healthcare_distance_2022", "log(Average Minutes to Healthcare Services) Across LSOAs in England (2022)", subtitle = "Logarithm of Mean Access Time to Healthcare Services per LSOA", caption = "Data source: Consumer Data Reasearch Centre (2022)")

DistPlot(combined_data, "log_employment_deprivation_rate_2015", 'log(Employment Deprivation Rate) Across LSOAs in England (2015)', subtitle = "Employment Deprivation Rate per LSOA (2015) based on Mid-Year Population Estimates", caption = "Data sources: Ministry of Housing, Communities & Local Government, 2019; Office for National Statistics, 2021")

## Filtering out rows with infinite values and creating a complete case sample:
complete_case_sample <- combined_data %>%
  select(log_income_deprivation_rate_2015, log_avg_healthcare_distance_2022, 
         housing_affordability_score_2016, rural_urban_class_2011) %>%
  filter(!is.infinite(log_avg_healthcare_distance_2022), 
         !is.infinite(log_income_deprivation_rate_2015)) %>%
  na.omit()

## Saving as CSV
write.csv(complete_case_sample, file = "Group_7_complete_case.csv", row.names = FALSE)

# Checking linearity of relationships between x variables
ggpairs(complete_case_sample[,c("log_income_deprivation_rate_2015", "housing_affordability_score_2016", "log_avg_healthcare_distance_2022")])

PointPlot(combined_data, "log_employment_deprivation_rate_2015", "log_income_deprivation_rate_2015", title = "Income Deprivation Rate (2015) as a Function of Employment Deprivation Rate (2015)", subtitle = "For Lower layer Super Output Areas Across England", x_label = "log(Employment Deprivation Rate) (2015)", y_label = "log(Income Deprivation Rate) (2015)", caption = "Data sources: Ministry of Housing, Communities & Local Government, 2019; Office for National Statistics, 2021", add_regression_line = TRUE)

result <- cor.test(combined_data$log_employment_deprivation_rate_2015, combined_data$income_deprivation_rate_2015, method = "pearson")

# creating a df with correlation test results
model_summary <- data.frame(
  Estimate = round(result$estimate, 4),
  t.value = round(result$statistic, 2),
  p = ifelse(result$p.value < 0.05, "< 0.05", round(result$p.value, 10))
)

# pretty table using stargazer
stargazer(model_summary, type = "html", 
          title = "Results of Pearson's Correlation Test",
          summary = FALSE,
          omit.stat = c("all"), 
          single.row = TRUE, rownames = FALSE, out = 'x1x2.html')
rstudioapi::viewer("x1x2.html")

# As employment deprivation rate and income deprivation rate are highly correlated, employment deprivation rate will be excluded from analyses (to avoid issues of multicollinearity).

# Examining linearity between xi and y
PointPlot(complete_case_sample, "log_income_deprivation_rate_2015", "log_avg_healthcare_distance_2022", title = "Average Healthcare Distance (2022) as a Function of Income Deprivation Rate (2015)", subtitle = "For Lower layer Super Output Areas Across England", x_label = "log(Income Deprivation Rate) (2015)", y_label = "log(Mean Access Time to Healthcare Services) (2022)", caption = "Data sources: Ministry of Housing, Communities & Local Government, 2019; Office for National Statistics, 2021; Consumer Data Reasearch Centre (2022)", add_regression_line = TRUE)

PointPlot(complete_case_sample, "housing_affordability_score_2016", "log_avg_healthcare_distance_2022", title = "Average Healthcare Distance (2022) as a Function of Housing Affordability Score (2016)", subtitle = "For Lower layer Super Output Areas Across England", x_label = "Housing Affordability Score (2016)", y_label = "log(Mean Access Time to Healthcare Services) (2022)", caption = "Data sources: Ministry of Housing, Communities & Local Government, 2019; Office for National Statistics, 2021; Consumer Data Reasearch Centre (2022)", add_regression_line = TRUE)

PointPlot(combined_data, "log_employment_deprivation_rate_2015", "log_avg_healthcare_distance_2022", title = "Average Healthcare Distance (2022) as a Function of Employment Deprivation Rate (2015)", subtitle = "For Lower layer Super Output Areas Across England", x_label = "log(Employment Deprivation Rate) (2015)", y_label = "log(Mean Access Time to Healthcare Services) (2022)", caption = "Data sources: Ministry of Housing, Communities & Local Government, 2019; Office for National Statistics, 2021; Consumer Data Reasearch Centre (2022)", add_regression_line = TRUE)
### PLotting of table for slide 11
combined_data_sum <- combined_data[c("local_authority_name","housing_affordability_score_2016", "income_deprivation_rate_2015", "employment_deprivation_rate_2015","avg_healthcare_distance_2022", "rural_urban_class_2011")]

html_table <- stargazer(
  combined_data_sum,
  type = "html",
  title = "Table 1: Summary Statistics",
  digits = 1,
  column.labels = c(
    "Housing Affordability Score (2016)",
    "Income Deprivation Rate (2015)",
    "Employment Deprivation Rate (2015)",
    "Average Healthcare Distance (2022)"
  ),
  out = "table.html"
)

# See HTML table in R Studio
rstudioapi::viewer("table.html")


### Calculate missingness percentages for each variable for slide 11
missingness <- sapply(combined_data_sum, function(x) mean(is.na(x)) * 100)

# Create HTML table using stargazer
html_table <- stargazer(missingness, type = "html", title = "Table 2: Missingness Summary/%", summary = FALSE, flip=TRUE)
html_file <- "missingness_table.html"
writeLines(html_table, html_file)

# See HTML table in R Studio
rstudioapi::viewer(html_file)


### count of unique names for rural/urban for slide 12
unique_class2011 <- table(combined_data_sum$rural_urban_class_2011)
unique_class2011.1 <- as.data.frame(unique_class2011)

# Rename the columns
colnames(unique_class2011.1) <- c("Rural/Urban Class (2011)", "Count")

# Create HTML table using stargazer
html_table1 <- stargazer(unique_class2011.1, type = "html", title = "Unique Names and its Respective Counts", summary = FALSE)
cat(html_table1)
writeLines(html_table1, html_file)

# See HTML table in R Studio
rstudioapi::viewer(html_file)

# Calculate summary statistics
summary_ruralurban <- c(
  Max = max(rural_urban_class_2011_counts),
  Min = min(rural_urban_class_2011_counts),
  Med = median(rural_urban_class_2011_counts),
  IQR = IQR(rural_urban_class_2011_counts)
)

# Create data frame for summary statistics
summary_df <- data.frame(statistic = names(summary_ruralurban), value = summary_ruralurban)

# Create HTML table using stargazer
html_table2 <- stargazer(summary_df, type = "html", title = "Summary Statistics", summary = FALSE, rownames = FALSE)
cat(html_table2)
writeLines(html_table2, html_file)

# See HTML table in R Studio
rstudioapi::viewer(html_file)


## count of unique names for local authority names for slide 12
unique_localauthority <- table(combined_data_sum$local_authority_name)

# Convert the result to a data frame
unique_localauthority1 <- as.data.frame(unique_localauthority)

colnames(unique_localauthority1) <- c("Local Authority Names", "Count")

# Create HTML table using stargazer
html_table3 <- stargazer(unique_localauthority1, type = "html", title = "Unique Names and its Respective Counts", summary = FALSE)
cat(html_table3)
writeLines(html_table3, html_file)
rstudioapi::viewer(html_file)

# Calculate summary statistics
summary_localauthority <- c(
  Max = max(local_authority_name_counts),
  Min = min(local_authority_name_counts),
  Med = median(local_authority_name_counts),
  IQR = IQR(local_authority_name_counts)
)

summary_df <- data.frame(statistic = names(summary_localauthority ), value = summary_localauthority )

# Create HTML table using stargazer
html_table4 <- stargazer(summary_df, type = "html", title = "Summary Statistics", summary = FALSE, rownames = FALSE)
cat(html_table4)
writeLines(html_table4, html_file)

# See HTML table in R Studio
rstudioapi::viewer(html_file)


### Repeat for complete_case.csv for slide 16
## Read the complete_case file
complete_case_sum <- read.csv("Group_7_complete_case.csv")

html_table <- stargazer(
  complete_case_sum,
  type = "html",
  title = "Table 3: Summary Statistics",
  digits = 1,
  column.labels = c(
    "Log Income Deprivation Rate (2015)",
    "Log Average Healthcare Distance (2022)",
    "Housing Affordability Score (2016)",
    "Rural/Urban Class (2011)"
  ),
  out = "table.html"
)

# See HTML table in R Studio
rstudioapi::viewer("table.html")

### Calculate missingness percentages for each variable for slide 16
missingness <- sapply(complete_case_sum, function(x) mean(is.na(x)) * 100)

# Create HTML table using stargazer
html_table <- stargazer(missingness, type = "html", title = "Table 4: Missingness Summary/%", summary = FALSE, flip=TRUE)
html_file <- "missingness_table.html"
writeLines(html_table, html_file)

# See HTML table in R Studio
rstudioapi::viewer(html_file)


### covariance of x1,x2,x3 before log transformation for slide 17
x1 <-combined_data_sum$income_deprivation_rate_2015
x2 <-combined_data_sum$employment_deprivation_rate_2015
x3 <-combined_data_sum$housing_affordability_score_2016
y <-combined_data_sum$avg_healthcare_distance_2022
cov_x1y <- cov(x1, y)
cov_x2y <- cov(x2, y)
cov_x3y <- cov(x3, y)

# Create a data frame for the covariances
covariances_df <- data.frame(
  variables = c("x1 and y", "x2 and y", "x3 and y"),
  covariance = c(cov_x1y, cov_x2y, cov_x3y)
)

# Create HTML table using stargazer
html_table <- stargazer(covariances_df, type = "html", title = "Covariances before log transformation", summary = FALSE, rownames = FALSE)
html_file <- "covariances_table.html"
writeLines(html_table, html_file)

# See HTML table in R Studio
rstudioapi::viewer(html_file)


###covariance of x4,x5 after log transformation for slide 17
complete_case_sum <- read.csv("Group_7_complete_case.csv")
x4 <-complete_case_sum$log_income_deprivation_rate_2015
x5 <-complete_case_sum$housing_affordability_score_2016
y1 <-complete_case_sum$log_avg_healthcare_distance_2022
cov_x4y1 <- cov(x4, y1)
cov_x5y1 <- cov(x5, y1)

# Create a data frame for the covariances
covariances_df <- data.frame(
  variables = c("x4 and y1", "x5 and y1"),
  covariance = c(cov_x4y1, cov_x5y1)
)

# Create HTML table using stargazer
html_table <- stargazer(covariances_df, type = "html", title = "Covariances after log transformation", summary = FALSE, rownames = FALSE)
html_file <- "covariances_table.html"
writeLines(html_table, html_file)

# See HTML table in R Studio
rstudioapi::viewer(html_file)


## Correlation test for slide 18
complete_case_sum <- read.csv("Group_7_complete_case.csv")
x4 <-complete_case_sum$log_income_deprivation_rate_2015
x5 <-complete_case_sum$housing_affordability_score_2016
y1 <-complete_case_sum$log_avg_healthcare_distance_2022
corx4y1 <- cor.test(x4, y1, method="pearson")
corx5y1 <- cor.test(x5, y1, method="pearson")

# Create a data frame to store correlation results
# format.pval function is used to format the p-value to make it into a string ie. a range
corr_df <- data.frame(
  variables = c("x4 and y1", "x5 and y1"),
  Pearson_Correlation = c(corx4y1$estimate, corx5y1$estimate),
  t_value = c(corx4y1$statistic, corx5y1$statistic),
  Estimate = c(corx4y1$estimate, corx5y1$estimate),
  p_value = ifelse(corx4y1$p.value == 0, "< 0.001", format.pval(corx4y1$p.value))
)

# Create HTML table using stargazer
html_table <- stargazer(corr_df, type = "html", title = "Pearson Correlations After Log Transformation", summary = FALSE, rownames = FALSE)
html_file <- "cor_table.html"
writeLines(html_table, html_file)

# See HTML table in R Studio
rstudioapi::viewer(html_file)
# Model 1:
# The logarithm of average healthcare distance modeled as a function of the logarithm of income deprivation rate.
mod1 <- lm(log_avg_healthcare_distance_2022 ~ log_income_deprivation_rate_2015, data = complete_case_sample)
summary(mod1)

# Model 1 represents the logarithm of average travel time to a healthcare service within an LSOA as a function of the logarithm of the income deprivation rate. As both the dependent and independent variables are log transformed, the coefficients may be interpreted in terms of percentage change. That is, a 1% increase in the income deprivation rate is associated with a 0.35% decrease in the average travel time to a healthcare service. This coefficient is statistically significant considering a significance level of 5% (alpha = 0.05). Thus, according to this model, we may reject the null hypothesis that there is no relationship between income deprivation rate and average healthcare travel distance. Further, the p-value associated with the F-statistic is also significant considering the same alpha, sugegsting at least one variable in the model has some explanatory power regarding average healthcare distance. This statistically is not that useful regarding this model however, as only one explanatory variable is considered. The coefficient of determination for this model indicates that we are able to explain 13.1% of the variation in average travel time to healthcare services with this model.

# Model 2:
# The logarithm of average healthcare distance modeled as a function of the logarithm of income deprivation rate and the housing affordability indicator
mod2 <- lm(log_avg_healthcare_distance_2022 ~ log_income_deprivation_rate_2015 + housing_affordability_score_2016, data = complete_case_sample)

# Exponentiating untransformed variable coefficients to allow easier interpretation
mod2$coefficients["housing_affordability_score_2016"] <- exp(mod2$coefficients["housing_affordability_score_2016"])

# Viewing model results
summary(mod2)

# Model 2 represents the logarithm of average travel time to a healthcare service within an LSOA as a function of the logarithm of the income deprivation rate and the housing affordability indicator. Here, we can see that a 1% increase in the income deprivation rate is associated with a 0.19% decrease in the average travel time to a healthcare service. Thus, we can see that when we control for a further underlying economic variable, the explanatory power of income deprivation alone decreases. Again, this coefficient is statistically significant considering a significance level of 5% (alpha = 0.05). For interpretation of the housing affordability indicator, the exponential of the coefficient is considered (0.88). Thus, we can see that a 1 unit increase in the housing affordability indicator is associated with a 12% decrease in the average travel time to a healthcare service, again significant considering an alpha of 0.05. Remembering that increases in the housing affordability indicator signify increased inability to afford to enter owner-occupation or the private rental market, this again appears to indicate that more economically deprived areas have greater geographical access to healthcare services. Further, the p-value associated with the F-statistic is also significant at the 5% level, again suggesting at least one variable in the model has some explanatory power regarding average healthcare distance. The adjusted coefficient of determination for this model indicates that we are able to explain 19.3% of the variation in average travel time to healthcare services with this model.

# Model 3:
# The logarithm of average healthcare distance modeled as a function of the logarithm of income deprivation rate, the housing affordability indicator, and the re-leveled Rural-Urban Classification (RUC11) with "Urban city and town" as the reference category
mod3 <- lm(log_avg_healthcare_distance_2022 ~ log_income_deprivation_rate_2015 + housing_affordability_score_2016 + forcats::fct_relevel(rural_urban_class_2011, "Urban city and town"), data = complete_case_sample)

# Exponentiated coefficient data
mod3_exp_coeff <- data.frame(exp(coef(mod3)))

# Exponentiating untransformed variable coefficients WITHIN the model to allow easier interpretation
mod3$coefficients[3:10] <- exp(mod3$coefficients[3:10])

# Viewing model results
summary(mod3)

# Creating data for confidence interval plot
ConfidencePlotData <- data.frame (
  "names" = as.factor (c("Rural town and fringe", "Rural town and fringe in a sparse setting", "Rural village and dispersed", "Rural village and dispersed in a sparse setting", "Urban city and town in a sparse setting", "Urban major conurbation", "Urban minor conurbation")), # Creating predictor names (row names)
  "coefs" = mod3_exp_coeff[4:10, 1],
  "lower" = confint (mod3) [4:10, 1], # Creating a lower confidence interval [ , 1] list for predictors 2-3 in the model [2:5 , ]
  "upper" = confint (mod3) [4:10, 2] # Creating an upper confidence interval [ , 2] list for predictors 2-3 in the model [2:5 , ]
)
ConfidencePlotData$names <- factor(ConfidencePlotData$names, levels = unique(ConfidencePlotData$names))

# Creating confidence interval plot for RUC coefficients
ConfidencePlot(ConfidencePlotData, "Healthcare Access by Rural-Urban Classification in England", "Reference Category: Urban City and Town", x_label = "Exponentiated Coefficient", middle_point = 1, caption = "*95% CI Indicated by Green Error Bars. Red dashed line indicates reference group. Controlling for income deprivation (2015) and housing affordability indicators (2016).")

# Model 3 expands on Model 2 by including the Rural-Urban Classification (RUC11) as a factor variable, allowing us to account for geographic differences in average travel time to healthcare services. The model estimates the logarithm of average travel time to a healthcare service within a Lower Layer Super Output Area (LSOA) as a function of the logarithm of the income deprivation rate, the housing affordability indicator, and the RUC11 categories. We observe that a 1% increase in the income deprivation rate is associated with a 0.10% decrease in average travel time to healthcare services, when holding the other variables constant. This relationship is statistically significant at the 5% level, with a p-value less than 0.05, indicating that income deprivation continues to be an important factor in explaining variations in healthcare access, albeit with a smaller coefficient compared to Model 2. Similarly, a 1 unit increase in the housing affordability indicator, which reflects increased difficulty in entering owner-occupation or the private rental market, is associated with a 7% decrease in average travel time to healthcare services. This relationship is also statistically significant at the 5% level. The new variables introduced, representing the RUC11 categories, show significant variations in healthcare access across different geographic areas. For instance, compared to Urban city and town areas, Rural village and dispersed areas experience a 344% increase in average travel time to healthcare services, indicating that rural areas face substantial barriers in healthcare access. These relationships are statistically significant at the 5% level, with the exception of Urban city and town in a sparse setting category, which does not show a significant difference in healthcare access compared to Urban city and town areas. The adjusted coefficient of determination for this model is 0.537, suggesting that the model explains approximately 53.7% of the variation in average travel time to healthcare services. The inclusion of the RUC11 categories significantly enhances the explanatory power of the model compared to Model 2. The p-value associated with the F-statistic is also significant at the 5% level, indicating that at least one variable in the model has some explanatory power regarding average healthcare distance. In conclusion, Model 3 provides a more nuanced understanding of the factors affecting healthcare access, by accounting for both economic and geographic variables. The significant differences in healthcare access across the RUC11 categories highlight the importance of considering geographic disparities when analyzing healthcare access.
results <- stargazer(mod1, mod2, mod3, type="html",
                     dep.var.labels=c("log(Mean Healthcare Service Distance [MHSD])"),
                     covariate.labels=c("log(Income Deprivation Rate 2015)","Housing Affordability Score 2016 [HAS]","Rural town and fringe [RUC 2011]","Rural town and fringe in a sparse setting [RUC 2011]","Rural village and dispersed [RUC 2011]", "Rural village and dispersed in a sparse setting [RUC 2011]", "Urban city and town in a sparse setting [RUC 2011]", "Urban major conurbation [RUC 2011]", "Urban minor conurbation [RUC 2011]"),
                     notes = "Coefficients for HAS and RUC 2011 are exponentiated, reflecting a multiplicative effect on MHSD.",
                     ci = TRUE)
# Checking assumptions: Model 1 
# Adding residual values to dataframe
complete_case_sample$resids1 <- residuals(mod1)

# Linearity (using partial regression plots)
PartialRegressionPlot (mod1, 'log_income_deprivation_rate_2015', x_var_name = "X1", y_var_name = "Y", title = "Partial Regression: log(MHSD) | log(IDR)")

# Constant variance of residuals 
HomoskedasticityPlot(mod1, add_loess_line = TRUE, title = "Model 1 Scale-Location")

# Durbin-Watson test
dw1 <- dwtest(mod1)

# Create a data frame with the Durbin-Watson test results
dw_df <- data.frame(
  Statistic = dw1$statistic,
  P = format.pval(dw1$p.value, digits = 16)
)

# Use stargazer to create an HTML table of the Durbin-Watson results
stargazer(dw_df, type = "html", title = "Durbin-Watson Test Results", 
          summary = FALSE, rownames = FALSE, out = 'dw_test_results.html')

# Use rstudioapi to view the table in RStudio Viewer
rstudioapi::viewer('dw_test_results.html')

# Normality of residuals check
QQPlotModel(mod1, subtitle = "QQ-Plot for Model 1")

# Zero conditional mean of residual values
mean(mod1$residuals)
resid_mod1 <- lm(resids1 ~ log_income_deprivation_rate_2015, data = complete_case_sample)
summary(resid_mod1)

# Zero conditional mean regression results
stargazer(resid_mod1, type="html",
          dep.var.labels=c("Residuals of Model 1"),
          covariate.labels=c("log(Income Deprivation Rate 2015)"), out = "residmod1.html")
rstudioapi::viewer("residmod1.html")

# Checking assumptions: Model 2
# Adding residual values to dataframe
complete_case_sample$resids2 <- residuals(mod2)

# Linearity (using partial regression plots)
PartialRegressionPlot (mod2, 'log_income_deprivation_rate_2015', x_var_name = "X1", y_var_name = "Y", title = "Partial Regression: log(MHSD) | log(IDR) + Controls")
PartialRegressionPlot (mod2, 'housing_affordability_score_2016', x_var_name = "X2", y_var_name = "Y", title = "Partial Regression: log(MHSD) | HAS + Controls")

# Multicollinearity (using VIF)
vif_values1 <- vif(mod2)

# storing VIF in a dataframe 
vif_df1 <- data.frame(Variable = names(vif_values1), VIF = vif_values1)
vif_df1$Variable[1] <- "log(Income Deprivation Rate 2015)"
vif_df1$Variable[2] <- "Housing Affordability Score 2016"

# creating a pretty table
stargazer(vif_df1, type = "html", title = "VIF Values", summary = FALSE, 
          rownames = FALSE, out = 'mod2vif.html')

# using rstudioapi to view the table
rstudioapi::viewer("mod2vif.html")

# Constant variance of residuals 
HomoskedasticityPlot(mod2, add_loess_line = TRUE, title = "Model 2 Scale-Location")

# Durbin-Watson test
dw2 <- dwtest(mod2)

# Create a data frame with the Durbin-Watson test results
dw_df2 <- data.frame(
  Statistic = dw2$statistic,
  P = ifelse(dw2$p.value > 0.05, '> 0.05',
             ifelse(dw2$p.value < 0.01, '< 0.01', '< 0.05'))
)

# Use stargazer to create an HTML table of the Durbin-Watson results
stargazer(dw_df2, type = "html", title = "Durbin-Watson Test Results", 
          summary = FALSE, rownames = FALSE, out = 'dw_test_results2.html')

# Use rstudioapi to view the table in RStudio Viewer
rstudioapi::viewer('dw_test_results2.html')

# Normality of residuals check
QQPlotModel(mod2, subtitle = "QQ-Plot for Model 2")

# Zero conditional mean of residual values
mean(mod2$residuals)
resid_mod2 <- lm(resids2 ~ log_income_deprivation_rate_2015 + housing_affordability_score_2016, data = complete_case_sample)
summary(resid_mod2)

# Zero conditional mean regression results
stargazer(resid_mod2, type="html",
          dep.var.labels=c("Residuals of Model 2"),
          covariate.labels=c("log(Income Deprivation Rate 2015)","Housing Affordability Score 2016 [HAS]"), out = "residmod2.html")
rstudioapi::viewer("residmod2.html")

# Checking assumptions: Model 3 
# Adding residual values to dataframe
complete_case_sample$resids3 <- residuals(mod3)

# Linearity (using partial regression plots)
PartialRegressionPlot (mod3, 'log_income_deprivation_rate_2015', x_var_name = "X1", y_var_name = "Y", title = "Partial Regression: log(MHSD) | log(IDR) + Controls")
PartialRegressionPlot (mod3, 'housing_affordability_score_2016', x_var_name = "X2", y_var_name = "Y", title = "Partial Regression: log(MHSD) | HAS + Controls")

# Multicollinearity (using VIF)
vif_values2 <- data.frame(vif(mod3))
vif_values2$Variable <- c('log(Income Deprivation Rate 2015)', 'Housing Affordability Score 2016', 'Rural Urban Classification 2011')
vif_df2 = vif_values2[, c("Variable", "GVIF")] 

# creating a pretty table
stargazer(vif_df2, type = "html", title = "GVIF Values", summary = FALSE, 
          rownames = FALSE, out = 'mod3vif.html')

# using rstudioapi to view the table
rstudioapi::viewer("mod3vif.html")

# Constant variance of residuals plot
HomoskedasticityPlot(mod3, add_loess_line = TRUE, title = "Model 3 Scale-Location")

# Plot indicating groups
HomoskedasticityDetailPlot(mod3, add_loess_line = TRUE, title = "Model 3 Scale-Location", dataframe = complete_case_sample, categorical_var_name = "rural_urban_class_2011")

# Testing for non-constant variance across groups:
leveneTest(complete_case_sample$resids3, complete_case_sample$rural_urban_class_2011, center=mean)

# df for levene's test results
levene_results <- data.frame(
  `Degrees of Freedom (Group)` = 7,
  `Degrees of Freedom (Residual)` = 32706,
  `F Value` = 189.04,
  `P Value` = '< 0.01'
)

names(levene_results) <- c("Degrees of Freedom (Group)", 
                           "Degrees of Freedom (Residual)", 
                           "F Value", 
                           "P Value")

# Viewing the table with descriptive column names
stargazer(levene_results, 
          type = "html", 
          title = "Levene's Test for Homogeneity of Variance", 
          out = "levene_test.html", 
          summary = FALSE, 
          rownames = FALSE)
rstudioapi::viewer('levene_test.html')



# Durbin-Watson test
dw3 <- dwtest(mod3)

# Create a data frame with the Durbin-Watson test results
dw_df3 <- data.frame(
  Statistic = dw3$statistic,
  P = ifelse(dw3$p.value > 0.05, '> 0.05',
             ifelse(dw3$p.value < 0.01, '< 0.01', '< 0.05'))
)

# Use stargazer to create an HTML table of the Durbin-Watson results
stargazer(dw_df3, type = "html", title = "Durbin-Watson Test Results", 
          summary = FALSE, rownames = FALSE, out = 'dw_test_results3.html')

# Use rstudioapi to view the table in RStudio Viewer
rstudioapi::viewer('dw_test_results3.html')

# When considering the independence of residuals, we might have reason to think that this assumption is not satisfied within the model, as it is likely that some degree of spatial autocorrelation could be influencing our results - that is, that LSOAs closer to each-other could theoretically have more similar values of X. This idea is supported by the results of the durbin-watson test statistics shown - notably, getting closer to 2 (representing less reduced autocorrelation) when including a variable that partially accounts for spatiality (the RUC2011 indicator). As such, we do see a considerable amount of positive autocorrelation in the data (according to a Durbin-Watson statistic of 0.83 being considerably below 2). This means that residuals that follow each other in the data tend to be slightly similar in magnitude. Considering the null hypothesis of no autocorrelation and considering the p-value being significant considering an alpha of 0.05, we fail to reject the null hypothesis that there is no autocorrelation presented by the variables within our model. Thus, we might not be confident that the independence of residuals assumption is satisfied.

# Normality of residuals check
QQPlotModel(mod3, subtitle = "QQ-Plot for Model 3")

# Zero conditional mean of residual values
mean(mod3$residuals)
resid_mod3 <- lm(resids3 ~ log_income_deprivation_rate_2015 + housing_affordability_score_2016 + rural_urban_class_2011, data = complete_case_sample)
summary(resid_mod3)

# Zero conditional mean regression results
stargazer(resid_mod3, type="html",
          dep.var.labels=c("Residuals of Model 3"),
          covariate.labels=c("log(Income Deprivation Rate 2015)","Housing Affordability Score 2016 [HAS]","Rural town and fringe [RUC 2011]","Rural town and fringe in a sparse setting [RUC 2011]","Rural village and dispersed [RUC 2011]", "Rural village and dispersed in a sparse setting [RUC 2011]", "Urban city and town in a sparse setting [RUC 2011]", "Urban major conurbation [RUC 2011]", "Urban minor conurbation [RUC 2011]"), out = "residmod3.html")
rstudioapi::viewer("residmod3.html")