# Folder Structure
```
├── Jupyter Notebooks           <- Jupyter Notebooks using py
│   └─── src                    <- Source code for use in notebooks
├── RMarkdown                   <- .Rmd files (i.e. forecasting methods, benchmarks)
│   └─── html_outputs           <- Outputs of .Rmd files as html
├── R_benchmarks                <- R scripts for creating benchmarks + ETSARIMA
│    └─── archive               <- Discarded r scripts for benchmarks (too slow, unmodularized)
├── data                        <- Data used in this project (i.e. M4 by domain/period, example data)
├── Images                      <- Images used in Rmd and notebook files
├── results                     <- Results and tables
│    └─── M4_ETSARIMA           
│    └─── M4_benchmarks8
│    └─── Reg_benchmarks8
├── src                         <- Source code for use in this project
    └─── data                   <- Scripts to download
├── .gitattributes
├── .gitignore
├── MT TM1.Rproj                <- .Rproj file for project structure
├── README.md                   <- Top-level README
```

# MT-TM1
Folder for MT part 1 focusing on:

- Theoretical background
- Statistical methods, i.e. benchmark methods of M4

FPP files covering chapters in the book, covering theoretical and practical aspects of the benchmark methods:

> Hyndman, R. J., & Athanasopoulos, G. (2018) Forecasting: principles and practice, 2nd edition, OTexts: Melbourne, Australia.

## Benchmarks
The 8 basic statistical benchmark methods used in the M4 Competition, which are:

- Naïve method: Simply the same value as yesterday
- Seasonal Naïve: Same value as last season
- Naive2: Naive method after deseasonalizing the series
- Simple Exponential Smoothing (SES)
- Holt
- Damped trend exponential Smoothing
- Theta method: Winner of M3
- Comb: Combination benchmark of SES, Holt and Damped trend (average)

In addition, I added the following forecast methods:

- Automatic ARIMA: Automated procedure to select an ARIMA model
- ETS: Automatic procedure to select an exponential smoothing model
- ETSARIMA: Average of ETS and Auto.arima

## data
Non-processed data of M4 Competition separated by frequency

## images
Images and some tables (as images) for reports.

## results
Results of the analyses

## RMarkdown
RMarkdown files and respective outputs as html files in report form.

## src
All the base code files and utility functions which are further used in scripts to get results
