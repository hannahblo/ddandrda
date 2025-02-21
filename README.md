
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ddandrda

<!-- badges: start -->
<!-- badges: end -->

ddandrda stands for "Data Depth and Relational Data Analysis." (This version of the this branch applies to all projects coded *before* November 2024.)

Depth functions measure centrality and outlyingness with respect to a probability distribution or data cloud. To extend the concept of centrality to non-standard data, i.e. data that cannot be represented in normed vector spaces, this package utilizes formal concept analysis to define depth functions for such cases.
This package implements depth functions for mixed spatial-ordinal-numeric data, partial order-valued data, and hierarchical nominal data. For more details on these depth functions, please refer to the relevant literature below.

## Corresponding Literatur
- Hannah Blocher and Georg Schollmeyer (2025): Data depth functions for non-standard data by use of formal concept analysis. Journal of Multivariate Analysis, 205, 105372.
- Julian Rodemann, Hannah Blocher (2024): Partial Rankings of Optimizers. International Conference on Learning Representations ICLR 2024, Tiny Papers Track, Vienna, Austria.
- Hannah Blocher, Georg Schollmeyer, Malte Nalenz and Christoph Jansen (2024): Comparing Machine Learning Algorithms by Union-Free Generic Depth. International Journal of Approximate Reasoning, 169: 1-23.


## Installation

You can install the development version of ddandrda from
[GitHub](https://github.com/) with:


``` r
# install.packages("devtools")
devtools::install_github("hannahblo/ddandrda", ref = "version_before_nov24")
```
Please note that this package requires the RcppZiggurat package, which depends on RcppGSL. The RcppZiggurat package will only load successfully if the installed GSL on the Linux or Windows setup matches.

<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
