library(tidyverse)
library(car)
library(MASS)
library(lmtest)
library(sandwich)
install.packages("sandwich")

set.seed(42)  # Ensure reproducibility

# Simulate data
n <- 100
year_of_construction <- sample(1950:2020, n, replace = TRUE)
location_area <- sample(c("North", "East", "West", "South"), n, replace = TRUE)
building_type <- sample(c("Residential", "Commercial", "Mixed"), n, replace = TRUE)

# Simulate number of floors 
number_of_floors <- round(runif(n, min = 5, max = 50) + 
                            (year_of_construction - 1950) / 10 + 
                            ifelse(location_area == "North", 5, 0) + 
                            ifelse(building_type == "Commercial", 3, 0) + 
                            rnorm(n, 0, 5))

# Create dataframe
buildings_df <- data.frame(number_of_floors, year_of_construction, location_area, building_type)

# Linear regression analysis
reg_model <- lm(number_of_floors ~ year_of_construction + location_area + building_type, data = buildings_df)
summary(reg_model)

# Tests
# Check for linearity and homoscedasticity
par(mfrow=c(2,2))
plot(reg_model)

# Test for Normality of Residuals 
shapiro.test(residuals(reg_model))

# Test for multicollinearity
vif(reg_model)

# Test for heteroscedasticity (Breusch-Pagan test & White test)
bptest(reg_model)
bptest(reg_model, ~ fitted.values(reg_model) + I(fitted.values(reg_model)^2), data = buildings_df)

# Coefficient significance tests
coef(summary(reg_model))

# Collinearity diagnostics
colldiag <- colldiag(reg_model, tol=1e-7, plot=TRUE)

# Robust standard errors
coeftest(reg_model, vcov = vcovHC(reg_model, type = "HC1"))
