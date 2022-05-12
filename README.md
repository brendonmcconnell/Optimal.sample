# Optimal.sample: Optimal Sample Design for Cluster Randomized Trials with Different Costs in Treatment and Control.
Authors: Nancy A. Daza-Báez (<n.baez@ucl.ac.uk>), Brendon McConnell (<brendon.mcconnell@gmail.com>) and Marcos Vera-Hernández (<m.vera@ucl.ac.uk>)

This package provides the optimal sample design (number of treatment and control clusters and number units sampled within treatment and control clusters) for a Cluster Randomized Control Trial to either maximize power subject to a cost constraint or minimize costs subject to a given level of power. The outcome variable is assumed to be continuous, and to be available only at endline (post treatment).

This package is provided free of charge and without any guarantee. If you use this package, please, cite this package as well as McConnell and Vera-Hernandez (2022) "More powerful Cluster Randomized Control Trials". The software assumes that the cost function is C = `k0*(f0 + (v0*m0)) + k1*(f1 + (v1*m1))` where `k1` is the number of treatment clusters, `k0` is the number of control clusters, `m1` is the number of units sampled within treatment clusters, `m0` is the number of units sampled within control clusters, `f1` is the fixed cost of sampling a treatment cluster, `f0` is the fixed cost of sampling a control cluster, `v1` is the marginal cost of a treatment unit, and `v0` is the marginal cost of a control unit. This package contains two functions: `MinCosts.opt.R` minimizes the costs subject to a given level of power, and `MaxPower.opt.R` which maximizes power subject to a given level of cost.


The [online vignettes](https://github.com/Nancydaza/Nancydaza.github.io/blob/main/Optimal.sample.html) contains a gallery of examples.

# Installation 

You can install the development version of Optimal.sample from GitHub with:

```{r}

install.packages("devtools")
devtools::install_github("brendonmcconnell/Optimal.sample",build_vignettes = TRUE)

```
