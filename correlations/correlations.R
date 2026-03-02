# This script runs Pearson, Spearman's, point bi-serial, and phi
# correlations using the dataset "ice_correlations.csv".

###################################

# Install packages
install.packages("jmv")
install.packages("scatr")

# Activate packages
library(jmv)
library(scatr)

# Set your working directory to the folder on your computer that contains
# the "ice_correlations.csv" dataset
setwd("path/to/your/folder")

# Import "ice_correlations.csv" and name it "data"
data <- read.csv("ice_correlations.csv")

# View "data"
data

# Ensure the following variables are in "numeric" format
data$study_hours <- as.numeric(data$study_hours)
data$exam_score <- as.numeric(data$exam_score)
data$stress_level <- as.numeric(data$stress_level)
data$sleep_hours <- as.numeric(data$sleep_hours)
data$practice_trials <- as.numeric(data$practice_trials)
data$confidence <- as.numeric(data$confidence)
data$experience_years <- as.numeric(data$experience_years)
data$performance_rating <- as.numeric(data$performance_rating)
data$caffeine_mg <- as.numeric(data$caffeine_mg)
data$ranked_skill <- as.numeric(data$ranked_skill)
data$job_performance <- as.numeric(data$job_performance)
data$symptom_reduction <- as.numeric(data$symptom_reduction)

# Check structure of "data"
str(data)

###################################

# Are study_hours and exam_score correlated?

# Look at scatterplot
scatr::scat(
  data = data, # dataset name
  x = 'study_hours',
  y = 'exam_score')

# Run Pearson correlation
jmv::corrMatrix(
  data = data, # dataset name
  vars = vars(study_hours, exam_score),
  pearson = TRUE, # run Pearson correlation
  spearman = FALSE, # don't run Spearman correlation
  kendall = FALSE, # don't run Kendall's tau
  sig = TRUE, # provide significance level
  flag = TRUE, # flag significant correlations with asterisks
  n = TRUE,  # print sample size
  ci = TRUE, # print CI
  ciWidth = 95, # set CI width to 95%
  plots = TRUE, # provide correlation matrix plot
  plotStats = TRUE # provide statistics correlation matrix plot
  ) 

###################################

# Are confidence and practice_trials correlated?

# Look at scatterplot
scatr::scat(
  data = data, # dataset name
  x = 'confidence',
  y = 'practice_trials')

# Apply square root transformation to confidence and add it as a new
# variable named "sqrt_confidence" in "data"
data$sqrt_confidence <- sqrt(data$confidence)

# Look at scatterplot with sqrt_confidence and practice_trials
scatr::scat(
  data = data, # dataset name
  x = 'sqrt_confidence',
  y = 'practice_trials')

# Run Pearson correlation with sqrt_confidence and practice_trials
jmv::corrMatrix(
  data = data, # dataset name
  vars = vars(sqrt_confidence, practice_trials),
  pearson = TRUE, # run Pearson correlation
  spearman = FALSE, # don't run Spearman correlation
  kendall = FALSE, # don't run Kendall's tau
  sig = TRUE, # provide significance level
  flag = TRUE, # flag significant correlations with asterisks
  n = TRUE,  # print sample size
  ci = TRUE, # print CI
  ciWidth = 95, # set CI width to 95%
  plots = TRUE, # provide correlation matrix plot
  plotStats = TRUE # provide statistics correlation matrix plot
) 

# Run Spearman correlation between confidence and practice_trials
jmv::corrMatrix(
  data = data, # dataset name
  vars = vars(confidence, practice_trials),
  pearson = FALSE, # don't run Pearson correlation
  spearman = TRUE, # run Spearman correlation
  kendall = FALSE, # don't run Kendall's tau correlation
  sig = TRUE, # provide significance level
  flag = TRUE, # flag significant correlations with asterisks
  n = TRUE,  # print sample size
  plots = TRUE, # provide correlation matrix plot
  plotStats = TRUE # provide statistics correlation matrix plot
) 

###################################

# Are caffeine_mg and reaction_time correlated?

# Look at scatterplot
scatr::scat(
  data = data, # dataset name
  x = 'caffeine_mg',
  y = 'reaction_time')

# Apply natural log transformation to caffeine_mg and add it as a new
# variable named "log_caffeine_mg" in "data"
data$log_caffeine_mg <- log(data$caffeine_mg)

# Look at scatterplot with log_caffeine_mg and reaction_time
scatr::scat(
  data = data, # dataset name
  x = 'log_caffeine_mg',
  y = 'reaction_time')

# Run Pearson correlation between log_caffeine_mg and reaction_time
jmv::corrMatrix(
  data = data, # dataset name
  vars = vars(log_caffeine_mg, reaction_time),
  pearson = TRUE, # run Pearson correlation
  spearman = FALSE, # don't run Spearman correlation
  kendall = FALSE, # don't run Kendall's tau correlation
  sig = TRUE, # provide significance level
  flag = TRUE, # flag significant correlations with asterisks
  n = TRUE,  # print sample size
  ci = TRUE, # print CI
  ciWidth = 95, # set CI width to 95%
  plots = TRUE, # provide correlation matrix plot
  plotStats = TRUE # provide statistics correlation matrix plot
) 

# Run Spearman correlation between caffeine_mg and reaction_time
jmv::corrMatrix(
  data = data, # dataset name
  vars = vars(caffeine_mg, reaction_time),
  pearson = FALSE, # don't run Pearson correlation
  spearman = TRUE, # run Spearman correlation
  kendall = FALSE, # don't run Kendall's tau correlation
  sig = TRUE, # provide significance level
  flag = TRUE, # flag significant correlations with asterisks
  n = TRUE,  # print name size
  plots = TRUE, # provide correlation matrix plot
  plotStats = TRUE # provide statistics correlation matrix plot
) 


###################################

# Are treatment_group & symptom_reduction correlated?

# Make sure treatment_group is coded as an integer
# 0 (control) and 1 (treatment)
str(data$treatment_group)
# This should yield: int [1:120] 1 0 1 0 1 1 1 1 1 1 ...

# Make sure treatment_group ONLY includes 0's and 1's
unique(data$treatment_group)
# This should yield: [1] 1 0

# Run point-biserial correlation between treatment_group and symptom_reduction
jmv::corrMatrix(
  data = data, # dataset name
  vars = vars(treatment_group, symptom_reduction),
  pearson = TRUE, # run Pearson correlation
  spearman = FALSE, # don't run Spearman correlation
  kendall = FALSE, # don't run Kendall's tau correlation
  sig = TRUE, # provide significance level
  flag = TRUE, # flag significant correlations with asterisks
  n = TRUE,  # print sample size
  ci = TRUE, # print CI
  ciWidth = 95, # set CI width to 95%
  plots = TRUE, # provide correlation matrix plot
  plotStats = TRUE # provide statistics correlation matrix plot
) 


###################################

# Are attended_workshop & passed_exam correlated?

# Make sure both variables are coded as integers
str(data$attended_workshop)
str(data$passed_exam)

# Make sure both variables ONLY include 0's and 1's
unique(data$attended_workshop)
unique(data$passed_exam)

# Run phi correlation
jmv::contTables(
  formula = ~ attended_workshop:passed_exam,
  data = data, # dataset name
  phiCra = TRUE, # provide Phi and Cramer's V
  exp = TRUE # provide the expected counts
  )