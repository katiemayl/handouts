# Linear models

library(readr)
library(dplyr)

person1 <- read_csv(
  file = 'data/census_pums/sample.csv')
head(person1)

person <- read_csv(
  file = 'data/census_pums/sample.csv',
  col_types = cols_only(
    AGEP = 'i',  # Age
    WAGP = 'd',  # Wages or salary income past 12 months
    SCHL = 'i',  # Educational attainment
    SEX = 'f',   # Sex
    OCCP = 'f',  # Occupation recode based on 2010 OCC codes
    WKHP = 'i')) # Usual hours worked per week past 12 months
str(person)
#what's the "i", "f", etc? type of factor
person <- within(person, {
  SCHL <- factor(SCHL)
  levels(SCHL) <- list(
    'Incomplete' = c(1:15),
    'High School' = 16,
    'College Credit' = 17:20,
    'Bachelor\'s' = 21,
    'Master\'s' = 22:23,
    'Doctorate' = 24)}) %>%
  filter(
    WAGP > 0,
    WAGP < max(WAGP, na.rm = TRUE))

# Formula Notation
# ~ means left is dependent var and indep var(s) are on right; data= nameof dataframe
fit <- lm(
  formula = WAGP ~ SCHL,
  data = person)

library(ggplot2)
ggplot(person, aes(x=SCHL, y=WAGP))+ geom_boxplot()
fit
#need to fit model (above)
# ??? look up more
#lm function just does least squares


#to not include a constant and force a 0 intercept, use "-1"

fit <- lm(
  log(WAGP) ~ SCHL,
  person)
summary(fit)

# Metadata matters

fit <- lm(
  log(WAGP) ~ AGEP,
  person)
summary(fit)


#agep is continuous data, not factor data

# GLM families

fit <- glm(log(WAGP) ~ SCHL,
           family = gaussian,
           data = person)
summary(fit)

# Logistic Regression
fit <- glm(SEX ~ WAGP,
           family = binomial,
           data = person)
#are wages a predictor of sex- is there a gender wage gap?
summary(fit)
levels(person$SEX)

#have a pos wage coeff, and larger wages are associated w being a man

anova(fit, update(fit, SEX ~ 1), test = 'Chisq')
#lm and glm allow weighting of observations

# Random Intercept

library(lme4)
fit <- lmer(
  log(WAGP) ~ (1|OCCP) + SCHL,
  data = person)

# Random Slope

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = person)
summary(fit)

fit <- lmer(
  log(WAGP) ~ (WKHP | SCHL),
  data = person, 
  control = lmerControl(optimizer = "bobyqa"))

ggplot(person,
       aes(x = WKHP, y = log(WAGP), color = SCHL)) +
  geom_point() +
  geom_line(aes(y = predict(fit))) +
  labs(title = 'Random intercept and slope with lmer')