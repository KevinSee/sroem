
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sroem: Salmonid Redd Observer Error Models

<!-- badges: start -->
<!-- badges: end -->

The goal of the `sroem` package is to provide functionality related to
the redd observer error models for steelhead and Spring Chinook,
described in Murdoch et al. (2018) and Murdoch et al. (2019).

## Installation

You can install the R package `sroem` from [GitHub](https://github.com/)
with:

``` r
# install.packages("devtools")
devtools::install_github("KevinSee/sroem")
```

You can install the development version with:

``` r
# install.packages("devtools")
devtools::install_github("KevinSee/sroem@develop")
```

For the experimental branch, first commit and push its changes, then install
it in another workflow with:

``` r
devtools::install_github("KevinSee/sroem@ai_test")
```

To install the current local checkout without publishing it, run
`devtools::install()` from the repository root. Run the package tests with
`devtools::test()`. The tests live in `tests/testthat/`.

# References

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-Murdoch2019" class="csl-entry">

Murdoch, Andrew R., Charles H. Frady, Michael S. Hughes, and Kevin See.
2019. “Estimating Population Size and Observation Bias for Spring
Chinook Salmon.” *Conservation Science and Practice* 1 (11).
<https://doi.org/10.1111/csp2.120>.

</div>

<div id="ref-Murdoch2018" class="csl-entry">

Murdoch, Andrew R., Chad J. Herring, Charles H. Frady, Kevin See, and
Chris E. Jordan. 2018. “Estimating Observer Error and Steelhead Redd
Abundance Using a Modified Gaussian Area-Under-the-Curve Framework.”
*Canadian Journal of Fisheries and Aquatic Sciences* 75 (12): 2149–58.
<https://doi.org/10.1139/cjfas-2017-0335>.

</div>

</div>
