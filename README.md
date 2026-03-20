# Used Car Price Prediction — MSc Dissertation Project

**Author:** Pradyuman Kumar  
**Institution:** Queen's University Belfast — MSc Business Analytics  
**Supervisors:** Dr. Rohit Nishant & Dr. Byron Graham (Programme Director, Queen's Business School)

---

## Project Overview

This project builds and evaluates multiple machine learning models to predict the selling price of used cars in India using the CarDekho dataset. The goal was to identify which regression technique most accurately predicts market value based on vehicle characteristics such as age, mileage, engine size and fuel type.

---

## Dataset

- **Source:** CarDekho (cardekho_dataset.csv)
- **Features used:** vehicle age, kilometres driven, mileage, engine size, max power, fuel type, seller type, transmission type, brand, seats
- **Target variable:** Selling price (INR)

---

## What This Project Does

### 1. Data Cleaning & Preprocessing
- Removes missing values and duplicate records
- Standardises categorical text fields (fuel type, seller type, transmission)
- Detects and removes outliers using the IQR method across all numeric columns
- Sets a fixed random seed (40425764) throughout for full reproducibility

### 2. Exploratory Data Analysis (EDA)
- Distribution plots for all numeric features (selling price, vehicle age, km driven, engine, max power, mileage, seats)
- Bar charts for categorical features (fuel type, seller type, transmission type, brand, model)
- Correlation heatmap across all numeric variables
- Chi-Squared tests for categorical feature significance
- Key visualisations including:
  - Top 10 most sold cars
  - Top 10 most expensive cars
  - Top 10 costliest brands by average price
  - Average mileage by brand
  - Kilometres driven vs selling price (by fuel type)
  - Mileage vs selling price with regression line
  - Vehicle age vs mileage distribution

### 3. Model Building & Evaluation
Six regression models were built and evaluated:

| Model | Method |
|---|---|
| Linear Regression | Baseline OLS regression |
| Support Vector Regression | Radial kernel SVM |
| Decision Tree Regressor | CART decision tree |
| Random Forest Regressor | Ensemble of 500 trees |
| Ridge Regression | L2 regularisation (cv.glmnet) |
| Lasso Regression | L1 regularisation (cv.glmnet) |

### 4. Cross-Validation
All models were evaluated using **10-Fold Cross Validation** via the `caret` package to ensure robust, unbiased performance measurement.

### 5. Performance Metrics
Each model was assessed on:
- **MSE** — Mean Squared Error
- **RMSE** — Root Mean Squared Error
- **MAE** — Mean Absolute Error
- **R²** — Coefficient of Determination

---

## Technologies Used

- **Language:** R
- **Key Libraries:** `caret`, `randomForest`, `glmnet`, `e1071`, `rpart`, `ggplot2`, `dplyr`, `tidyverse`, `reshape2`

---

## How to Run

1. Clone this repository
2. Place `cardekho_dataset.csv` in your working directory (or update the file path in the script)
3. Open `Updated code for dissertation.R` in RStudio
4. Install required packages if not already installed:
```r
install.packages(c("caret", "readxl", "tidyverse", "glmnet", "e1071", 
                   "randomForest", "rpart", "ggplot2", "reshape2"))
```
5. Run the script from top to bottom

---

## Key Findings

- Random Forest Regressor achieved the strongest predictive performance across MSE, RMSE and R² metrics
- Engine size and max power showed the highest positive correlation with selling price
- Vehicle age and kilometres driven were the strongest negative predictors of price
- Diesel vehicles commanded a price premium over petrol equivalents in the dataset

---

## Research Paper

This project accompanied a published dissertation paper:  
**"Predicting the Price of Used Cars in India Using Machine Learning Techniques"**  
Queen's Business School, Queen's University Belfast (2024)# used-car-price-prediction
 MSc research project — predicting used car prices in India using ML (Python/R)
