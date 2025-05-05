# Infectious Diseases (CDAD 1046EQ)

## Cholera Epidemic Data Analysis – Data Vizualization #1 

This project investigates a cholera epidemic that occurred in 2023 in a large city in a low-income country. The analysis uses two datasets:

- `cases.csv`: Weekly records of cholera cases and deaths among individuals aged 5 years and older.
- `cc-study.csv`: A case-control study of cholera patients and controls, detailing socioeconomic, behavioral, and water/food-related exposures.

The objective was to explore the progression of the epidemic, evaluate public health interventions, and identify key risk factors for infection.

---

## Repository Contents
```
Data Vizualization#1 /
├── data/           # Contains raw datasets: cases.csv and cc-study.csv
├── outputs/        # Generated plots and tables
├── script/         # Annotated R script used for analysis
├── Dataviz 1 Report.pdf   # Final report answering all questions
├── .DS_Store       # macOS metadata
└── README.md       
```

---

## Tools Used

- **R / RStudio**
- **ggplot2**, **dplyr** for data visualization and analysis

---

## How to Reproduce the Analysis

1. Load `cases.csv` and `cc-study.csv` into R.
2. Use the provided script in the `/script` folder to:
   - Plot epidemic curve
   - Compute weekly CFR
   - Run logistic regressions for case-control data
3. Interpret output based on public health knowledge of cholera

---

## Author

**Shadad Hossain**  
CDAD 1046EQ – Data Visualization/Analysis  
Spring 2025

