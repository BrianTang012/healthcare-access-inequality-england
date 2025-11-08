# Healthcare Access & Socioeconomic Deprivation Analysis 🏥

A comprehensive R analysis examining the relationship between socioeconomic deprivation and geographical access to healthcare services across England.

---

## 📋 Project Overview

This research investigates how income deprivation, housing affordability, and rural-urban classification affect travel times to healthcare services using Lower Layer Super Output Areas (LSOA) data across England.

**Research Question:**  
How does socioeconomic deprivation relate to geographical access to healthcare services in England, and to what extent do rural-urban disparities moderate this relationship?

---

## 🚀 Quick Start

### Prerequisites
- **R** (version 4.0+ recommended)  
- **RStudio** (optional but recommended)

### Installation & Setup

1. Clone the repository:
```bash
git clone https://github.com/BrianTang012/healthcare-access-inequality-england.git
cd healthcare-access-inequality-england
Install required R packages:

r
Copy code
# Run in R console
required_packages <- c("tidyverse", "haven", "dplyr", "readxl", "GGally", 
                       "forcats", "stargazer", "lme4", "car", "rstudioapi", 
                       "knitr", "kableExtra", "showtext", "lmtest")

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
📁 Project Structure
text
Copy code
healthcare-access-inequality-england/
├── 📊 datasets/
│   ├── Group_7_AHAH_V3_0.csv
│   ├── Group_7_IoD2019_Income.csv
│   ├── Group_7_IoD2019_Health.csv
│   ├── Group_7_IoD2019_Barriers.csv
│   ├── Group_7_IoD2019_Education.csv
│   ├── Group_7_IoD2019_Employment.csv
│   ├── Group_7_ONS_MIDYEAR_2015.csv
│   └── Group_7_RUC2011.csv
├── 📜 scripts/
│   └── R_analysis.R
└── README.md
🏃‍♂️ Running the Analysis
Open the main script in R/RStudio:

text
Copy code
scripts/R_analysis.R
Set your working directory to the project root:

r
Copy code
setwd("path/to/healthcare-access-inequality-england")
Run the entire script or execute sections sequentially.

Outputs generated in your working directory:

Processed datasets (Group_7_combined_data.csv, Group_7_complete_case.csv)

Statistical tables (HTML)

Visualization plots (PNG)

📊 Data Sources
Dataset	Description	Variables Used	Source
AHAH V3.0	Access to healthcare services	Distance to GP, hospital, dentist, pharmacy	Consumer Data Research Centre
IoD2019 Income	Income deprivation	Income deprivation numerator	MHCLG
IoD2019 Employment	Employment deprivation	Employment deprivation numerator	MHCLG
IoD2019 Barriers	Housing barriers	Housing affordability indicator	MHCLG
ONS Mid-Year 2015	Population estimates	All ages population	Office for National Statistics
RUC2011	Geographic classification	Rural-Urban categories	Office for National Statistics

🔑 Key Variables
Dependent Variable

log_avg_healthcare_distance_2022

Independent Variables

log_income_deprivation_rate_2015

housing_affordability_score_2016

rural_urban_class_2011

Created Variables

income_deprivation_rate_2015

employment_deprivation_rate_2015

avg_healthcare_distance_2022

📈 Analysis Approach
Statistical Models
Model 1: log(MHSD) ~ log(Income Deprivation Rate)

Model 2: log(MHSD) ~ log(Income Deprivation Rate) + Housing Affordability Score

Model 3: log(MHSD) ~ log(Income Deprivation Rate) + Housing Affordability Score + Rural-Urban Classification

Methodological Steps
Merge multiple datasets using LSOA codes

Calculate rates and average distances

Handle missing values and transformations

Exploratory analysis (distributions, correlations)

Regression modeling (three nested linear models)

Diagnostic checking (assumption validation)

🎯 Key Findings
Economic Access Paradox: More deprived areas have better geographical access to healthcare services

Housing Affordability Impact: Higher scores → shorter travel times (7–12% decrease per unit)

Geographic Disparities: Rural areas have much longer travel times; rural village areas 344% higher than urban

Model 3 explains 53.7% of variance (vs 19.3% in Model 2)

📁 Output Files
Group_7_combined_data.csv, Group_7_complete_case.csv

Distribution plots, scatter plots with regression lines, diagnostic plots, confidence interval plots (PNG)

Summary statistics, regression tables, correlation tests, model diagnostics (HTML)

⚠️ Limitations
Cross-sectional analysis — Cannot establish causality

Spatial autocorrelation — Some residual patterns may remain

Temporal mismatch — Variables measured across different years (2015–2022)

Focuses on geographic access, not service quality

Required removal of incomplete cases

🔧 Custom Functions
PointPlot(), DistPlot(), ConfidencePlot(), PartialRegressionPlot(), HomoskedasticityPlot(), QQPlotModel()

🤝 Contributing
Fork the repo and submit pull requests. Ensure reproducibility is maintained.

📄 License
Academic/research purposes. Cite original data sources.

📧 Contact
Open an issue on the GitHub repository.
