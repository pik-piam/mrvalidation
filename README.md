# MadRat validation package

## Purpose and Functionality

Package contains routines to prepare data for validation exercises.


## Installation

For installation of the most recent package version an additional repository has to be added in R:

```r
options(repos = c(CRAN = "@CRAN@", pik = "https://rse.pik-potsdam.de/r/packages"))
```
The additional repository can be made available permanently by adding the line above to a file called `.Rprofile` stored in the home folder of your system (`Sys.glob("~")` in R returns the home directory).

After that the most recent version of the package can be installed using `install.packages`:

```r 
install.packages("mrvalidation")
```

Package updates can be installed using `update.packages` (make sure that the additional repository has been added before running that command):

```r 
update.packages()
```

## Travis CI Integration

[![Travis build status](https://travis-ci.com/pik-piam/mrvalidation.svg?branch=master)](https://travis-ci.com/pik-piam/mrvalidation)


## Questions / Problems

In case of questions / problems please contact Benjamin Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

```r 
citation("mrvalidation")
```
