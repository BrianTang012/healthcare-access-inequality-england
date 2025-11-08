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
```

2. Install required R packages (run in R console):

```r
required_packages <- c(
  "tidyverse", "haven", "dplyr", "readxl", "GGally",
  "forcats", "stargazer", "lme4", "car", "rstudioapi",
  "knitr", "kableExtra", "showtext", "lmtest"
)

new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
```

## 📁 Project Structure
```bash
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
```

## 🏃‍♂️ Running the Analysis
Set your working directory in R:
```r
setwd("path/to/healthcare-access-inequality-england")
```
Then run the main analysis script:
```r
source("scripts/R_analysis.R")
```

## 📊 Data Sources

| Dataset | Description | Variables Used | Source |
|---------|------------|----------------|--------|
| AHAH V3.0 | Access to healthcare services | Distance to GP, hospital, dentist, pharmacy | Consumer Data Research Centre |
| IoD2019 Income | Income deprivation | Income deprivation numerator | MHCLG |
| IoD2019 Employment | Employment deprivation | Employment deprivation numerator | MHCLG |
| IoD2019 Barriers | Housing barriers | Housing affordability indicator | MHCLG |
| IoD2019 Education | Education deprivation | Education deprivation score | MHCLG |
| ONS Mid-Year 2015 | Population estimates | All ages population | Office for National Statistics |
| RUC2011 | Geographic classification | Rural-Urban categories | Office for National Statistics |

## 🔑 Key Variables

**Dependent Variable:**  
- `log_avg_healthcare_distance_2022`: Logarithm of average travel time (minutes) to GP practices, hospitals, dentists, and pharmacies.

**Independent Variables:**  
- `log_income_deprivation_rate_2015`: Logarithm of income deprivation rate (%)  
- `housing_affordability_score_2016`: Housing affordability indicator (higher = more deprivation)  
- `rural_urban_class_2011`: Rural-Urban Classification (8 categories)

**Created Variables:**  
- `income_deprivation_rate_2015`  
- `employment_deprivation_rate_2015`  
- `avg_healthcare_distance_2022`

## 📈 Analysis Approach

**Statistical Models:**  
- Model 1 (Basic): `log(MHSD) ~ log(Income Deprivation Rate)`  
- Model 2 (Economic Factors): `log(MHSD) ~ log(Income Deprivation Rate) + Housing Affordability Score`  
- Model 3 (Full Geographic): `log(MHSD) ~ log(Income Deprivation Rate) + Housing Affordability Score + Rural-Urban Classification`

**Methodological Steps:**  
1. Merge multiple datasets using LSOA codes.  
2. Calculate rates and average distances.  
3. Handle missing values and apply necessary transformations.  
4. Conduct exploratory analysis (distributions, correlations).  
5. Perform regression modeling with three nested linear models.  
6. Check model diagnostics and validate assumptions.

## 🎯 Key Findings

- **Economic Access Paradox:** More deprived areas have better geographical access to healthcare services.  
- **Housing Affordability Impact:** Higher scores are associated with shorter travel times (7–12% decrease per unit).  
- **Geographic Disparities:** Rural areas have much longer travel times; rural village areas are 344% higher than urban.  
- **Model Performance:** Model 3 explains 53.7% of variance versus 19.3% in Model 2.

## 📊 Interactive Analysis & Visualizations

For interactive visualizations, detailed charts, and exploratory data analysis, view the complete report on RPubs:

**[🔗 View Full Interactive Report on RPubs](https://rpubs.com/BrianTTS/1119474)**

The RPubs report includes:
- Interactive data visualizations
- Dynamic regression plots
- Exploratory data analysis
- Model diagnostic charts
- Complete statistical outputs

## 📁 Output Files

- **Processed Data:** `Group_7_combined_data.csv`, `Group_7_complete_case.csv`  
- **Visualizations:** Distribution plots, scatter plots with regression lines, diagnostic plots, confidence interval plots (PNG)  
- **Statistical Outputs:** Summary statistics, regression tables, correlation tests, model diagnostics (HTML)

## ⚠️ Limitations

- Cross-sectional analysis — Cannot establish causality.  
- Spatial autocorrelation — Some residual patterns may remain.  
- Temporal mismatch — Variables measured across different years (2015–2022).  
- Focuses on geographic access, not service quality.  
- Required removal of incomplete cases.

## 🔧 Custom Functions

- `PointPlot()`  
- `DistPlot()`  
- `ConfidencePlot()`  
- `PartialRegressionPlot()`  
- `HomoskedasticityPlot()`  
- `QQPlotModel()`

## 🤝 Contributing

Feel free to fork the repository and submit pull requests. Ensure reproducibility is maintained.

## 📄 License

For academic/research purposes. Cite original data sources appropriately.

## 📧 Contact

Open an issue on the [GitHub repository](https://github.com/BrianTang012/healthcare-access-inequality-england).
