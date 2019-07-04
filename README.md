# MT-TM1
Folder for MT (part 1), focusing on:

- Theoretical background
- Statistical methods, i.e. benchmark methods of M4

FPP files covering chapters in the book, theoretical and practical aspects of the benchmark methods as well as general forecast aspects:

> Hyndman, R. J., & Athanasopoulos, G. (2018) Forecasting: principles and practice, 2nd edition, OTexts: Melbourne, Australia.

## Folder Structure
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


## Benchmarks

#### Traditional statistical methods

The 8 basic statistical benchmark methods used in the M4 Competition, namely:

- Naïve method: Simply the same value as yesterday
- Seasonal Naïve: Same value as last season
- Naive2: Naive method after deseasonalizing the series
- Simple Exponential Smoothing (SES)
- Holt
- Damped trend exponential Smoothing
- Theta method: Winner of M3
- Comb: Combination benchmark of SES, Holt and Damped trend (average)

#### Other widely used methods

In addition, I added the following forecast methods:

- Automatic ARIMA: Automated procedure to select an ARIMA model
- ETS: Automatic procedure to select an exponential smoothing model
- ETSARIMA: Average of ETS and Auto.arima

#### ML Methods

Two basic ML methods were used in the competition:

- RNN Bench: Sequential recurrent neural network (RNN) using `SimpleRNN` layer with 6 nodes in Keras.
- MLP Bench: Multilayer perceptron (MLP) network using `MLPRegressor` with 6 units from scikit learn.

Both methods in the given formulation performed badly in the M4 competition
