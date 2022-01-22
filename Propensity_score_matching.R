library(MatchIt)
#read the data
library(readxl)
setwd("folder_name")  # Enter the name of the folder.
df <- read_excel("Input_PSM.xlsx") # File with all the covariates and the treatment (0/1).
head(df)

# No matching; constructing a pre-match matchit object
#Include as many covariates as needed.
m.out0 <- matchit(treatment ~ covariate1 + covariate2 + covariate3, data = df, method = NULL, distance = "logit")

# Checking balance prior to matching
summary(m.out0)
#Values of standardized mean differences and eCDF statistics close to zero and values of variance ratios close to one indicate good balance.

# 1:1 Nearest PS matching w/o replacement
set.seed(1234)
m.out1 <- matchit(treatment ~ covariate1 + covariate2 + covariate3, data = df, method = "nearest", distance = "logit",
                  caliper=0.2, std.caliper = TRUE)
# Checking balance after matching
summary(m.out1, un = FALSE)


#OPTIMAL MATCHING, try different methods and choose the one depending on the rwquirements of the study.
# No matching; constructing a pre-match matchit object
m.out0 <- matchit(treatment ~ covariate1 + covariate2 + covariate3, data = df, method = NULL, distance = "mahalanobis")
# Checking balance prior to matching
summary(m.out0)

set.seed(1234)
m.out1 <- matchit(treatment ~ covariate1 + covariate2 + covariate3, data = df, method = "optimal", distance = "mahalanobis", ratio=1)
summary(m.out1, un = FALSE)

#Displays the average absolute within-pair difference of each covariate, this has to be small for better balance.
plot(m.out1, type = "jitter", interactive = FALSE)

#We can visually examine balance on the covariates using plot() with type = "qq":
plot(m.out1, type = "qq", interactive = FALSE, which.xs = c("covariate1", "covariate2", "covariate3"))

#Love plot
plot(summary(m.out1))
