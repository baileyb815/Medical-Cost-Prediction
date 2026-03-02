#Upload data
medical <- read.csv("medical_cost_prediction_dataset.csv", header=TRUE)
medical 

#Regression model 
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

#Split data into train and test 
medical_train <- medical[1:3999, ]
medical_test <- medical[4000:5000, ]

#Create labels for training and test data
medical_train_labels <- medical[1:3999, ]
medical_test_labels <- medical[4000:5000, ]

#Train model  
model <- lm(annual_medical_cost ~ ., data = medical_train)
summary(model)

#Test model on a new patient 
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

