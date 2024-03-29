# SEMwPLS
Data sets and code to accompany the book "Structural Equation Modelling with Partial Least Squares", CRC Press (2021) by Mehmet Mehmetoglu and Sergio Venturini.

![Structural Equation Modelling with Partial Least Squares Using Stata and R](https://images-na.ssl-images-amazon.com/images/I/41TEKNZ1f4L._SX320_BO1,204,203,200_.jpg)

ISBN 978-1-482-22781-9

## Software installation notes
The software used in the book are Stata and `R`.

To get the PLS-SEM packages used in the book installed in your computer, you need to run the following code:

- for Stata, execute the following lines directly in Stata

``` stata
net install github, from("https://haghish.github.io/github/")
github install sergioventurini/plssem
```

- for `R`, install the `cSEM` and `plspm` packages as follows

``` r
install.packages("devtools")
devtools::install_github("M-E-Rademaker/cSEM")
devtools::install_github("gastonstat/plspm")
```

You can find more information about the `plssem` Stata package at [https://github.com/sergioventurini/plssem](https://github.com/sergioventurini/plssem).

For the `cSEM` and `plspm` `R` packages, you can refer instead to [https://github.com/M-E-Rademaker/cSEM](https://github.com/M-E-Rademaker/cSEM) and [https://github.com/gastonstat/plspm](https://github.com/gastonstat/plspm) respectively.
