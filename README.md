# SEMwPLS
Data sets and code to accompany the book "Structural Equation Modelling with Partial Least Squares", CRC Press (2021) by Mehmet Mehmetoglu and Sergio Venturini.

## Software installation
The software used in the book are Stata and `R`. To get the PLS-SEM software used in the book installed in your computer, you need to run the following code:

- for Stata, execute the following lines directly in Stata

    net install github, from("https://haghish.github.io/github/")
    github install sergioventurini/plssem

- for `R`, install the `cSEM` and `plspm` packages as follows

``` r
install.packages("devtools")
devtools::install_github("M-E-Radamaker/cSEM")
devtools::install_github("gastonstat/plspm")
```

You can find more information about the `plssem` Stata package at [https://github.com/sergioventurini/plssem](https://github.com/sergioventurini/plssem).

For the `cSEM` and `plspm` packages you can refer instead to https://github.com/M-E-Radamaker/cSEM](https://github.com/M-E-Radamaker/cSEM) and https://github.com/gastonstat/plspm](https://github.com/gastonstat/plspm) respectively.