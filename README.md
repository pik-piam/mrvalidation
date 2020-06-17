# madrat data preparation for validation purposes

R package **mrvalidation**, version **2.0.6**

  

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

## Questions / Problems

In case of questions / problems please contact Benjamin Leon Bodirsky <bodirsky@pik-potsdam.de>.

## Citation

To cite package **mrvalidation** in publications use:

Bodirsky B, Wirth S, Karstens K, Humpenoeder F, Stevanovic M, Mishra A, Biewald A, Weindl I, Chen D, Molina Bacca E,
Kreidenweis U, W. Yalew A, Humpenoeder F, Wang X, Dietrich J (2020). _mrvalidation: madrat data preparation for
validation purposes_. R package version 2.0.6.

A BibTeX entry for LaTeX users is

 ```latex
@Manual{,
  title = {mrvalidation: madrat data preparation for validation purposes},
  author = {Benjamin Leon Bodirsky and Stephen Wirth and Kristine Karstens and Florian Humpenoeder and Mishko Stevanovic and Abhijeet Mishra and Anne Biewald and Isabelle Weindl and David Chen and Edna {Molina Bacca} and Ulrich Kreidenweis and Amsalu {W. Yalew} and Florian {Humpenoeder } and Xiaoxi Wang and Jan Philipp Dietrich},
  year = {2020},
  note = {R package version 2.0.6},
}
```

