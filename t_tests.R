# This script runs one-sample, independent-samples, and paired-samples
# t-tests using the dataset "ice_t_tests.csv".

# To run a line of code, highlight it and click "Run".
# It is located on the upper right corner of this window with a green arrow.

# e.g., to run the following line of code:

sum(1,2)

# Highlight "sum(1,2)" and click "Run". The results will appear
# in the console below.

###################################

# Install "jmv" package
install.packages("jmv")

# Activate "jmv" package
library(jmv)

# Set your working directory to the folder on your computer that contains
# the "ice_t_tests.csv" dataset
setwd("path/to/your/folder")

# Import "ice_t_tests.csv" and name it "data"
data <- read.csv("ice_t_tests.csv")

# View "data"
data

###################################

# Run independent samples t test to answer the following question:
# Did men or women make more money in 2022? (Note: I ran multiple
# t statistics for demonstration purposes)

jmv::ttestIS(
  formula = income_2022 ~ manwoman, # DV ~ IV
  data = data, # dataset name
  vars = income_2022, # the DV
  students = TRUE, # run Student's t
  welchs = TRUE, # run Welch's t
  mann = TRUE, # run Mann-Whitney U
  norm = TRUE, # run Shapiro-Wilk test
  eqv = TRUE, # run Levene's test
  effectSize = TRUE, # provide effect size
  ciES = TRUE, # provide CI
  ciWidth = 95, # set width of CI to 95%
  desc = TRUE, # provide descriptive statistics
  plots = TRUE) # provide descriptive plots; will appear in plots viewer

###################################

# Run paired samples t test to answer the following question: 
# Did people make more money in 2022 than they did in 2023? (Note: I ran
# multiple t statistics for demonstration purposes)

jmv::ttestPS(
  data = data, # dataset name
  pairs = list(
    list(
      i1="income_2022", # DV
      i2="income_2023")), # DV
  students = TRUE, # run Student's t
  wilcoxon = TRUE, # run Wilcoxon signed rank test 
  norm = TRUE, # run Shapiro-Wilk test
  effectSize = TRUE, # provide effect size
  ciES = TRUE, # provide CI
  ciWidth = 95, # set width of CI to 95%
  desc = TRUE, # provide descriptive statistics
  plots = TRUE) # provide descriptive plots; will appear in plots viewer

###################################

# Run one sample t test to answer the following question: 
# Did our sample make more money in 2022 than the national average
# in 2022 (μ = 50k)? (Note: I ran multiple t statistics for
# demonstration purposes)

jmv::ttestOneS(
  data = data, # dataset name
  vars = income_2022, # sample mean
  students = TRUE, # run Student's t
  wilcoxon = TRUE, # run Wilcoxon signed rank test 
  testValue = 50000, # population mean
  norm = TRUE, # run Shapiro-Wilk test
  effectSize = TRUE, # provide effect size
  ciES = TRUE, # provide CI
  ciWidth = 95, # set width of CI to 95%
  desc = TRUE, # provide descriptive statistics
  plots = TRUE) # provide descriptive plots; will appear in plots viewer

