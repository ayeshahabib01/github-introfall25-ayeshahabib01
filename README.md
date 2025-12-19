# Predicting Exam Performance from Student Habits

**Authors:** Ethan Herrell, Ayesha Habib, Charlie Heyworth, Emma West  
**Course:** SYS 3501 – Computational Methods for AI Systems  

## Project Overview
This project analyzes how student lifestyle habits relate to academic performance. Using exploratory data analysis (EDA) and machine learning, we examine how factors such as study time, sleep, mental health, exercise, and entertainment usage influence exam outcomes.

The analysis combines visualization, feature engineering, and classification models to identify patterns associated with high and low exam performance.

## Data
- **Source:** Student Habits vs Academic Performance (Kaggle, simulated data)
- **Size:** 1000 students × 16 variables (727 rows after cleaning)
- **Key Features:**
  - Study hours, sleep, exercise, mental health
  - Social media and Netflix usage
  - Attendance, diet quality, demographics
- **Target Variable:** `exam_score` (used as both continuous and binary outcomes)

## Methods
### Exploratory Data Analysis
- Removal of missing values and IQR-based outlier filtering
- Histograms, box plots, scatter plots, and correlation heatmaps
- Identification of strong positive correlations (study hours, wellness)
- Identification of negative correlations (entertainment time)

### Feature Engineering
- **Wellness Score:** sleep + exercise + mental health
- **Total Entertainment Hours:** social media + Netflix
- Binary classification targets for modeling

### Models
- **Logistic Regression**
  - Accuracy ≈ 66%
  - ROC–AUC ≈ 0.71
  - High interpretability
- **Random Forest**
  - Accuracy ≈ 94%
  - ROC–AUC ≈ 0.98
  - Captures non-linear relationships

## Key Findings
- Study hours are the strongest predictor of exam performance
- Higher wellness is associated with better exam outcomes
- Increased entertainment time is linked to lower exam scores
- Academic performance reflects both study habits and lifestyle balance

## Repository Structure
```
├── notebooks/        # Python notebooks (EDA + models)
├── data/             # Original and cleaned datasets
├── slides/           # Flash Talk presentation slides
├── README.md         # Project overview and instructions
```

## How to Reproduce
1. Clone the repository
2. Open the Python notebooks in Jupyter or Google Colab
3. Install required libraries:
   - pandas
   - numpy
   - matplotlib
   - seaborn
   - scikit-learn
4. Run the notebooks from top to bottom
