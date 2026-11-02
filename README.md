# Credit Card Customer Churn Analysis

An **IT3081 Statistical Modelling group project** investigating recorded credit card customer attrition using R, supported by an interactive **Retention Decision-Support System (DSS)** dashboard prototype.

The project combines statistical analysis, model evaluation, industry feedback, and recommendations for a proposed customer-review process.

## Live Dashboard

[**Open the Retention DSS Dashboard →**](https://retention-dss-2026-ds-77.netlify.app/)

Explore the dashboard directly in your browser. No installation is required.

The dashboard is a retrospective academic demonstration. It does not represent a production banking system or actual customer interventions.

## Project Objectives

- Examine customer characteristics and activity associated with recorded attrition.
- Investigate selected relationships using statistical inference.
- Compare predictive models and evaluate the selected model on held-out data.
- Demonstrate how model scores could support human review and hypothetical capacity planning.
- Propose future validation and pilot evaluation informed by statistical findings and industry feedback.

## Dataset

The project uses the **BankChurners dataset**, containing **10,127 credit card customer records**.

**Source:** [Credit Card Customers — Sakshi Goyal, Kaggle](https://www.kaggle.com/datasets/sakshigoyal7/credit-card-customers)

| Subset | Records | Purpose |
|---|---:|---|
| Full dataset | 10,127 | Descriptive analysis and statistical investigation |
| Training subset | 8,101 | Model fitting, cross-validation, and threshold selection |
| Test subset | 2,026 | Held-out model evaluation |

The dataset contains recorded attrition labels and aggregated customer characteristics. It does not contain dated longitudinal transaction histories.

Financial amounts in the dashboard are described as **dataset monetary units**, because the national currency is unverified.

## Statistical Approach

### Descriptive Analysis

Exploration of customer demographics, relationships, transaction activity, inactivity, missing or unknown categories, and potential outliers.

### Statistical Inference

Selected group comparisons and hypothesis tests, including:

- Welch’s t-test.
- Proportion testing.
- Brown–Forsythe testing.
- Welch ANOVA.
- Holm adjustment for the primary multiple comparisons.

### Predictive Modelling

Models evaluated include:

- Baseline logistic regression.
- Logistic regression with natural spline terms.
- Ridge regression.
- LASSO.
- Elastic Net.

Model selection used **training 10-fold cross-validated log loss**. The classification threshold was selected by maximising F1 using training out-of-fold predictions.

PCA, Bayesian methods, time-series analysis, and experimental design are discussed as potential approaches or future work. They are not presented as completed implementations.

## Selected Model Results

The selected model was **logistic regression with natural spline terms**.

| Test-set measure | Result |
|---|---:|
| ROC-AUC | Approximately 0.960 |
| Log loss | Approximately 0.180 |
| Average precision | Approximately 0.847 |
| Training-selected threshold | 0.339210811098011 |
| Records flagged | 360 |
| Records below threshold | 1,666 |

At the selected threshold:

| Classification outcome | Records |
|---|---:|
| True positives | 259 |
| False positives | 101 |
| True negatives | 1,599 |
| False negatives | 67 |

These results describe classification of recorded attrition in the dataset. Model scores have **not been validated as probabilities of future customer departure**.

## Dashboard Features

- **Project Overview:** project context, objectives, and dataset populations.
- **Customer Activity:** descriptive findings and statistical comparisons.
- **Model Evidence:** model comparisons, performance, calibration, and threshold evaluation.
- **Review Queue:** searchable and sortable anonymised demonstration records, ordered by highest score first by default.
- **Workload & Capacity:** hypothetical review-capacity planning.
- **Simulated Review Tracking:** demonstration decisions and notes.
- **Pilot Proposal:** proposed evaluation framework and expert feedback.
- **Methodology & Limitations:** analytical assumptions and future requirements.
- **Final Recommendations:** proposed next steps and decision criteria.

### Demonstration Behaviour

- The review queue contains **2,026 test records**, of which **360** meet the selected threshold.
- At **100 hypothetical review slots**, **100 records** are within capacity and **260** are beyond capacity.
- Review tracking covers all test records, including those below the threshold.
- Decisions and notes exist only for the current page session.
- Refreshing the page or selecting **Reset Demo** clears simulated decisions and notes.
- Review statuses do not indicate actual customer contact, retention, or investigation.

## Repository Structure

```text
credit-card-customer-churn-analysis/
├── README.md
├── .gitignore
├── DS-WD-77.R
├── SM- Credit Card Customer Churn Analysis.Rproj
├── BankChurners.csv
├── prepare_dashboard_data.R
├── 2026-DS-77.pptx
└── dashboard/
    ├── index.html
    ├── app.js
    ├── styles.css
    ├── serve.ps1
    ├── verify.cjs
    ├── README.md
    └── data/
```

| File or folder | Purpose |
|---|---|
| `DS-WD-77.R` | Main statistical analysis |
| `SM- Credit Card Customer Churn Analysis.Rproj` | RStudio project configuration |
| `BankChurners.csv` | Source dataset |
| `prepare_dashboard_data.R` | Prepares dashboard data from saved analysis outputs |
| `2026-DS-77.pptx` | Final group presentation |
| `dashboard/` | Dashboard application and prepared data |

## Run the R Analysis

1. Download the repository using **Code → Download ZIP** and extract it, or clone the repository.
2. Open `SM- Credit Card Customer Churn Analysis.Rproj` in RStudio.
3. Keep `BankChurners.csv` in the project’s top-level folder.
4. Open `DS-WD-77.R`.
5. Install `glmnet` if it is not already available:

```r
install.packages("glmnet")
```

6. Run the analysis sections in order.

The script uses the relative dataset path:

```r
data_path <- "BankChurners.csv"
```

Running the analysis generates output folders beside the dataset:

```text
Task3_Manual_R_Outputs/
Task4_Manual_R_Outputs/
Task5_Manual_R_Outputs/
```

The script includes interactive table-viewing commands, so RStudio is the intended environment for working through the analysis.

## Run the Dashboard Locally

The dashboard includes prepared data. **You do not need to rerun the R analysis to view it.**

### Windows PowerShell

1. Open the `dashboard` folder in File Explorer.
2. Type `powershell` in File Explorer’s address bar and press Enter.
3. Run:

```powershell
powershell -ExecutionPolicy Bypass -File .\serve.ps1
```

4. Open **http://localhost:8000** in your browser.

Keep the PowerShell window open while using the dashboard.

### Python Alternative

If Python is installed, open a terminal in the `dashboard` folder and run:

```bash
python -m http.server 8000
```

Then open **http://localhost:8000**.

Serve the dashboard through a local server rather than opening `index.html` directly. An internet connection is needed to load externally hosted chart and font resources.

## Rebuild the Dashboard Data

The repository already includes the data needed for the dashboard.

To regenerate it:

1. Run the main R analysis to produce the Task 3, 4, and 5 output folders.
2. From the project’s top-level folder, run:

```r
source("prepare_dashboard_data.R")
```

The preparation script combines saved test predictions with source customer attributes, creates demonstration IDs, and writes dashboard-ready files into `dashboard/data/`.

## Team and Responsibilities

This is a collaborative group project. The table below reflects the documented task allocation.

| Team member | Role | Assigned responsibilities |
|---|---|---|
| **Aqsa M. A. F.** | Group leader | Statistical inference, predictive modelling, and compilation of the literature review |
| **Waduwage C. A.** | Member A | Problem framing and descriptive analysis |
| **Aadhila M. A. F.** | Member B | Experimental design and critical evaluation of PCA and Bayesian methods |
| **Faldano Pinghe F. T. K.** | Member C | Time-series evaluation, innovation proposal, and final recommendations |

Industry expert validation and literature contributions were allocated across the group.

The repository brings together the group’s work and should not be interpreted as an individual project by the repository owner.

## Limitations and Future Work

- **Retrospective data:** recorded attrition labels do not establish future predictive validity.
- **No causal conclusions:** observed associations do not demonstrate why an individual customer left.
- **Limited history:** the source dataset contains aggregated measures rather than dated customer trajectories.
- **Prototype scope:** review decisions and capacity assumptions are illustrative.
- **Further validation:** operational use would require authorised organisational data, prospective evaluation, and appropriate governance.
- **Longer histories:** collecting 12–24 months of dated transaction and payment history is a future recommendation, not an implemented dashboard feature.
- **Pilot evaluation:** a controlled pilot would be needed to assess whether proposed interventions improve outcomes.

## Acknowledgements

- Sakshi Goyal for making the BankChurners dataset available through Kaggle.
- The industry expert who provided feedback on the proposed retention framework.
- All group members for their contributions to the project.
