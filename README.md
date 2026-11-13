# Explainable Early-Warning System for At-Risk University Students

A big-data pipeline that predicts which students will fail or withdraw, using only information available at weeks 4, 8 and 12 of a course. It explains each prediction in terms of student behaviour, stores the results in MongoDB, and presents them in an R Shiny dashboard.

Data: Open University Learning Analytics Dataset (OULAD), CC BY 4.0. Cite as Kuzilek, Hlosta & Zdrahal (2017), *Scientific Data*, 4, 170171, https://doi.org/10.1038/sdata.2017.171.

Authors: [add names]. Course: [add course]. Date: [add date].

Live demo (public, exported data only): https://studentsatriskclarity.shinyapps.io/early-warning-dashboard/



## What it does

1. Builds features per student at three checkpoints (day 28, 56, 84), using only records dated up to the cutoff and removing students who had already left.
2. Trains logistic regression and random forest models in Spark MLlib, with a flagging threshold chosen on validation data to favour recall.
3. Evaluates on a random student split and on an unseen course year (train 2013, test 2014), with results by module and by group.
4. Explains each score with an exact decomposition of the logistic regression, grouped into engagement, assessments, background and course themes. Only behavioural reasons are shown to users.
5. Stores predictions and explanations in MongoDB and shows them in a Shiny dashboard.

## Folder layout

```
data/raw/          the 7 OULAD CSV files (not in the repository; download them yourself)
data/processed/    feature tables (Parquet) and records.jsonl (generated)
notebooks/         01 to 07, run in order
app/app.R          the Shiny dashboard
reports/           result tables and figures (generated)
docs/           final report and slides (exported Word or PDF files)
requirements.txt   Python packages
```

## Setup (Windows, PowerShell)

1. **Java 17**: `winget install EclipseAdoptium.Temurin.17.JDK`, then reopen the terminal and check `java -version`.
2. **Python environment**: create a virtual environment(Python 3.13), activate it, then `pip install -r requirements.txt`.
3. **MongoDB**: install MongoDB Community and make sure the service is running (`Get-Service -Name MongoDB`). The code expects `mongodb://localhost:27017`.
4. **R (4.6.1 used)** with the packages `shiny`, `bslib`, `mongolite`, `DT` and `ggplot2`.
5. **Data**: download OULAD from https://research.stem.open.ac.uk/ouanalyse/open-dataset-more/ and put the seven CSV files in `data/raw/`.

## Running the project

Run the notebooks in order from the `notebooks/` folder, with the virtual environment selected as the kernel.

| Notebook | Purpose | Main output |
|---|---|---|
| `01_eda` | Explore the data, define the at-risk label, cohorts and checkpoints | none (read-only) |
| `02_features` | Build the feature table for each checkpoint in Spark | `data/processed/features_day{28,56,84}.parquet` |
| `03_modelling` | Train and evaluate models, choose thresholds, temporal check | `reports/results_by_cutoff.csv` |
| `04_explain` | Check the explanation maths on one checkpoint | none |
| `05_records` | Build one explanation record per student per checkpoint | `data/processed/records.jsonl` |
| `06_mongo` | Load the records and metrics into MongoDB (`oulad_ews` database) | `predictions` and `model_metrics` collections |
| `07_checks` | Recall by module, fairness by group | tables printed in the notebook |

Then start the dashboard from the project folder:

```powershell
& "C:\Program Files\R\R-4.6.1\bin\Rscript.exe" -e "shiny::runApp('app', launch.browser=TRUE)"
```

## Headline results

- AUC on held-out students rises from about 0.77 at week 4 to 0.83 at week 12 (logistic regression and random forest perform almost identically).
- Trained on 2013 and tested on 2014, AUC falls by no more than 0.015.
- At the chosen thresholds, about three in four at-risk students are caught at 62% to 68% precision. Recall varies widely by module.
- Error rates differ between groups (gender, disability, deprivation band). See the report for details and limitations.

## Notes

- Dashboard figures include students the model was trained on unless "Held-out test students only" is ticked. Use held-out figures when quoting results.
- "Actual outcome" in the dashboard is available only because the data are historical.
- Random seeds are fixed where used, but Spark results can differ slightly between runs and machines.
