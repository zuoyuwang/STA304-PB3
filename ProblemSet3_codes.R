library(tidyverse)
setwd("/Users/tonystark/Desktop/PS3")
# Loading in the cleaned survey Data
survey_data <- read_csv("outputs/survey_data.csv")

# Loading in the cleaned census Data
census_data_trump <- read_csv("outputs/census_data_trump.csv")

census_data_biden <- read_csv("outputs/census_data_biden.csv")


# build logistoc model
# full model
full_trump <- glm(vote_trump ~ income + gender + age_group + race + hispanic + foreign_born + language + degree + employed, data = survey_data, family=binomial)  
summary(full_trump)

full_biden <- glm(vote_biden ~ income + gender + age_group + race + hispanic + foreign_born + language + degree + employed, data = survey_data, family=binomial)  
summary(full_biden)


# model selection
library(MASS)

#step BIC regression
step_BIC_trump <- full_trump %>% stepAIC(k=log(nrow(survey_data)))

step_BIC_biden <- full_biden %>% stepAIC(k=log(nrow(survey_data)))

final_trump <- glm(formula = vote_trump ~ income + gender + age_group + race + 
                     hispanic + foreign_born + degree, family = binomial, data = survey_data)

final_biden <- glm(formula = vote_biden ~ gender + race + degree, family = binomial, data = survey_data)
summary(final_trump)
summary(final_biden)

# Post-Stratification

#trump
census_data_trump$estimate_trump <- final_trump %>% predict(newdata = census_data_trump, type="response")

predict_trump <- census_data_trump %>% 
  mutate(alp_predict_prop = estimate_trump*n) %>%summarise(alp_predict = sum(alp_predict_prop) / sum(n))


#biden
census_data_biden$estimate_biden <- final_biden %>% predict(newdata = census_data_biden, type="response")

predict_biden <- census_data_biden %>% 
  mutate(alp_predict_prop = estimate_biden*n) %>%summarise(alp_predict = sum(alp_predict_prop) / sum(n))

# final predictions
predict_trump
predict_biden

# tables and plots

#variables
Names <- c("vote_2020", "household_income", "gender", "age", "race_ethnicity", "hispanic", "foreign_born", "education")
Types <- c("Categorical", "Categorical", "Binary", "Categorical", "Categorical", "Binary", "Binary", "Categorical")
Levels <- c("[Donald Trump, Joe Biden, ...]", "[$75,000 to $79,999,...]", "[Female,Male]","[25, 34,...]", "[Black, or African American, White,...]", "[Yes, No]", "[The United States, Another country]", "[Associate Degree,...]")
variables_table <- tibble(Names, Types, Levels)
knitr::kable(variables_table, caption = "Original Variables Summary Table")

Names <- c("vote_trump", "income", "gender", "age_group", "race", "hispanic", "foreign_born", "degree")
Types <- c("Binary", "Categorical", "Binary", "Categorical", "Categorical", "Binary", "Binary", "Categorical")
Levels <- c("[1,0]", "[15,000-49,999,...]", "[Female,Male]","[18-34,35-59,Above 60]", "[Black, White, Other]", "[Yes, No]", "[The United States, Another country]", "[Less than Associate, Associate and Above...]")
variables_table <- tibble(Names, Types, Levels)
knitr::kable(variables_table, caption = "Reformatted Variables Summary Table")


# odds ratio and confidence interval
cimat = Pmisc::ciMat(0.95)
coef_table = summary(final_trump)$coef[, rownames(cimat)] %*% cimat
knitr::kable(exp(coef_table), 
             caption="Odds Ratio and 95% Confidence Interval of the Coefficients for Trump Model")

cimat = Pmisc::ciMat(0.95)
coef_table = summary(final_biden)$coef[, rownames(cimat)] %*% cimat
knitr::kable(exp(coef_table), 
             caption="Odds Ratio and 95% Confidence Interval of the Coefficients for Biden Model")


# check multicollinearity
library(car)
vif(final_trump)
vif(final_biden)
library(car)
knitr::kable(vif(final_trump), caption="VIF Table of Trump Model")
knitr::kable(vif(final_biden), caption="VIF Table of Biden Model")

# ROC curve
library(pROC)
p <- predict(final_trump,  type="response")
roc_logit <- roc(survey_data$vote_trump ~ p)
TPR <- roc_logit$sensitivities
FPR <- 1 - roc_logit$specificities
plot(FPR, TPR, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, lwd = 2,col = 'red', bty = "n", main="Figure 1. ROC curve of Trump Model")
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit),2)))

p <- predict(final_biden,  type="response")
roc_logit <- roc(survey_data$vote_biden ~ p)
TPR <- roc_logit$sensitivities
FPR <- 1 - roc_logit$specificities
plot(FPR, TPR, xlim = c(0,1), ylim = c(0,1), type = 'l', lty = 1, lwd = 2,col = 'red', bty = "n", main="Figure 2. ROC curve of Biden Model")
abline(a = 0, b = 1, lty = 2, col = 'blue')
text(0.7,0.4,label = paste("AUC = ", round(auc(roc_logit),2)))

