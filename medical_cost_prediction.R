medical <- read.csv("medical_cost_prediction_dataset.csv", header=TRUE)
medical 

#Insurance Dummies
medical$dummy_private <- ifelse(medical$insurance_type == "Private", 1, 0)
medical$dummy_government <- ifelse(medical$insurance_type == "Government", 1, 0)
medical$dummy_none <- ifelse(medical$insurance_type == "None", 1, 0)
medical 

#City Type Dummies 
medical$dummy_semiurban <- ifelse(medical$city_type == "Semi-Urban", 1, 0)
medical$dummy_urban <- ifelse(medical$city_type == "Urban", 1, 0)
medical$dummy_rural <- ifelse(medical$city_type == "Rural", 1, 0)

medical_reg <- lm(annual_medical_cost ~ ., data = medical)
summary(medical_reg)

#Residual Plot
medical_scatter <- resid(medical_reg)
plot(fitted(medical_reg), medical_scatter)
title(main = "Residual Values")

#QQ Plot
r <- medical_reg$residual
standardize_r <- r/sd(r)

qqnorm(r)
qqline(r)

#KS Test
ks.test(standardize_r, "pnorm")

#Durbin Watson Test
library(car)
durbinWatsonTest(medical_reg, alternative=c("two.sided"))

library(forecast)
medical_ts <- ts(medical, frequency = 1)
medical_ts

single_ts <- medical_ts[, "annual_medical_cost"]

fit <- auto.arima(single_ts)
forecast_results <- forecast(fit, h=12)

forecast_results
plot(forecast_results)

medical_train <- medical[1:3999, ]
medical_test <- medical[4000:5000, ]

#Create labels for training and test data

medical_train_labels <- medical[1:3999, ]
medical_test_labels <- medical[4000:5000, ]

#Train model  
model <- lm(annual_medical_cost ~ ., data = medical_train)
summary(model)

predictions <- predict(model, newdata = medical_test)
head(predictions)
predictions

test_case <- data.frame(age = 70, gender = 'Female', bmi = 32.0, smoker = 'Yes', diabetes = 1,
                        hypertension = 0, heart_disease = 1, asthma = 0, physical_activity_level = 'Low',
                        daily_steps = 3000, sleep_hours = 6.0, stress_level = 8, 
                        doctor_visits_per_year = 7, hospital_admissions = 1, medication_count = 5, 
                        insurance_type = 'Government', insurance_coverage_pct = 80, city_type = 'Urban',
                        previous_year_cost = 8000
)

predicted_price <- predict(model, newdata = test_case)

predicted_price
