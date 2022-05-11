
#' @title Power maximizing sample design for cluster Randomized Control Trials with only an endline measurement subject to a costs constraint.
#' @description This function delivers the number of clusters and the number of units (individuals, firms, etc) within cluster that maximizes power subject to
#' a cost constraint in a cluster Randomized Control Trial (RCT). The function assumes that the outcome variable is continuous and has only been measured at endline.
#' This function assumes that the cost function includes a fixed costs per cluster (that can be different for treatment and control clusters) as well as a cost
#' per unit within a cluster (that can be different for treatment and control clusters): \eqn{Costs = k0*(f0 + (v0*m0)) + k1*(f1 + (v1*m1))}.
#' The function provides the optimal number of clusters and units within clusters for three different cases: (1) when both the optimal number of clusters and
#' units are allowed to be different between treatment and control arms, (2) when the number of clusters are allowed to be different between treatment and control arms, but the number of
#' units is constrained to be the same in both arms, and (3) when the number of units within cluster are allowed to be different between treatment and control arms, but the
#' number of clusters is constrained to be the same in both arms.
#'
#' @author Nancy A. Daza-Báez, \email{n.baez@ucl.ac.uk}
#' @author Brendon McConnell, \email{B.I.Mcconnell@soton.ac.uk>}
#' @author Marcos Vera-Hernández, \email{m.vera@ucl.ac.uk}
#' @references McConnell and Vera-Hernández (2022). More Powerfull Cluster Randomized Control Trials. Mimeo
#' @import dplyr
#' @import GenSA
#' @import stringr
#' @import xlsx
#'
#' @param delta Vector. Size of the effect on the outcome variable (effect size measured in same units as the outcome variable).
#' @param sigma Vector. Standard deviation of the outcome variable.
#' @param rho Vector. Intra-cluster correlation.
#' @param alpha Vector. Significance level for the null hypothesis of no effect.
#' @param C Vector. Maximum level of costs of implementing the RCT. It includes data collection costs (baseline and endline) and the costs of implementing the intervention under study.
#' @param q Where (K - q) are the degrees of freedom to test the null hypothesis of null effect. Default is 1.
#' @param v0 Vector. Variable costs per unit in the control clusters. It includes the cost of data collection (baseline and endline) and the cost of implementing the intervention under study.
#' @param v1 Vector. Variable costs per unit in the treatment clusters. It includes the cost of data collection (baseline and endline) and the cost of implementing the intervention under study.
#' @param f0 Vector. Fixed costs per control cluster. It includes the total fixed cost: baseline and endline.
#' @param f1 Vector. Fixed costs per treatment cluster. It includes the total fixed cost: baseline and endline.
#' @param optimal.s Indicates whether the sample design should constrain the number of units per treatment and control clusters to be the same ("CONST-IND") or whether the sample design should constrain the treatment and control clusters to be the same "CONST-CLUST" or whether the solution should be fully unconstrained ("CLUST-IND").
#' @param initial.cond  Vector. Initial values of the number of sample units per cluster (m0, m1) and the number of clusters (k0, k1) - keep the order- that the optimization routine will use. Default is NULL, in which case, the function will compute these initial conditions.
#' @param seed Integer. Seed for the random number generator that the optimization routine GenSA will use. Default is 210613.
#' @param lb Vector. Minimum possible value for the optimal number of clusters and optimal number of units. Default is 1 for each parameter.
#' @param ub Vector. Maximum possible value for the optimal number of clusters and optimal number of units. Default is 1000 for each parameter.
#' @param temp Numeric. Temperature parameter for the GenSA optimization function. Default is NULL, in which case, the default value in GenSA function will be used.
#' @param output Indicates the name of the xlsx file where you want to save the results. Default is NULL, in which case, the results will be presented in a matrix.
#'
#' @return \code{maxpower.opt} returns a matrix of size (14 x number of Scenarios). Each scenario is a combination of of specify parameters, and fixed and variable costs per unit. For each scenario the matrix provides the following components: \describe{
#'
#' \item{scenario}{This is a vector of the number of the scenario displayed.}
#'
#' \item{delta}{This is a vector of the size of the effect on the outcome variable.}
#'
#' \item{sigma}{This is a vector of the standard deviation of the outcome variable.}
#'
#' \item{rho}{This is a vector of the intra-cluster correlation.}
#'
#' \item{C}{This is a vector of the maximum level of total costs of implementing the RCT. It includes data collection costs (baseline and endline) and the costs of implementing the intervention under study.}
#'
#' \item{v0}{This is a vector of the variable cost per control unit.}
#'
#' \item{v1}{This is a vector of the variable costs per treatment unit.}
#'
#' \item{f0}{This is a vector of the fixed costs per control cluster.}
#'
#' \item{f1}{This is a vector of the fixed costs per treatment cluster.}
#'
#' \item{k0}{This is a vector of the optimum number of control clusters that maximize power.}
#'
#' \item{k1}{This is a vector of the optimum number of treatment clusters that maximize power.}
#'
#' \item{m0}{This is a vector of the optimum number of sample units per control cluster that maximize power.}
#'
#' \item{m1}{This is a vector of the optimum number of sample units per treatment cluster that maximize power.}
#'
#' \item{power}{This is the vector of the power of the RCT with the optimum number of clusters and units provided by this function.}
#'}
#'
#'
#'@examples
#'
#'## In this example, both fixed costs per cluster and variable cost per unit within cluster are different between treatment and control.
#'## There are three different scenarios, each with a different total costs. The syntax (optimal.s = "CLUST-IND") allows both the optimal number of clusters and units per
#'## cluster to be different between the treatment and control arms. The results will be saved in the "myresults.xlsx" file.
#'
#' maxpower.opt(delta = 0.25,
#'              sigma = 1,
#'              rho = 0.05,
#'              alpha = 0.05,
#'              C = c(815052.294, 974856.169, 1095876.675),
#'              v0 = 150,
#'              v1 = 2200,
#'              f0 = 500,
#'              f1 = 18000,
#'              optimal.s = "CLUST-IND",
#'              output = "myresults")
#'
#'## If you wish, you can specify initial conditions for the optimization algorithm: m0=20, m1=18, k0=15 and k1=18.
#'
#' maxpower.opt(delta = 0.25,
#'              sigma = 1,
#'              rho = 0.05,
#'              alpha = 0.05,
#'              C = c(815065.655, 877717.857, 995811.458),
#'              v0 = 150,
#'              v1 = 2200,
#'              f0 = c(500, 1500, 5000),
#'              f1 = 18000,
#'              optimal.s = "CLUST-IND",
#'              initial.cond = c(20, 18, 15, 18))
#'
#'## This is an example with three scenarios, each with a different value of the fixed cost per cluster in the treatment group (f1).
#'## The syntax (optimal.s = "CONST-IND") requests that the number of units per cluster is constrained to be the same in treatment as in control.
#'
#' maxpower.opt(delta = 0.25,
#'              sigma = 1,
#'              rho = 0.27,
#'              alpha = 0.05,
#'              C = c(75862.836, 145230.184, 204196.756),
#'              v0 = 25,
#'              v1 = 100,
#'              f0 = 381,
#'              f1 = c(500, 1981, 3500),
#'              optimal.s = "CONST-IND")
#'
#'## This is an example with three scenarios, each with a different value of the variable cost per unit in the treatment group (v1).
#'## The syntax (optimal.s = "CONST-CLUST") requests that the number of clusters is constrained to be the same in treatment as in control.
#'
#' maxpower.opt(delta = 0.25,
#'              sigma = 1,
#'              rho = 0.05,
#'              alpha = 0.05,
#'              C = c(144412.242, 251543.646, 384610.811),
#'              v0 = 150,
#'              v1 = c(250, 750, 1500),
#'              f0 = 500,
#'              f1 = 18000,
#'              optimal.s = "CONST-CLUST")
#'
#'
#' @export

maxpower.opt <- function(delta,
                         sigma,
                         rho,
                         alpha,
                         C,
                         q = 1,
                         v0,
                         v1,
                         f0,
                         f1,
                         optimal.s = c("CLUST-IND","CONST-IND","CONST-CLUST"),
                         initial.cond = NULL,
                         seed = 210613,
                         lb = NULL,
                         ub = NULL,
                         temp = NULL,
                         output = NULL){


  set.seed(seed)

  ### load libraries

  requireNamespace("dplyr")
  requireNamespace("GenSA")
  requireNamespace("stringr")
  requireNamespace("xlsx")

  #############
  ### Options of the optimal sample to estimate: cluster and individuals
  #############

  if (length(optimal.s) > 1L){
    stop("'optimal.s' must be of length 1")
  }

  if (str_detect(optimal.s, "CLUST-IND")){
    optimal <- 0
    n.par <- 3
  } else if (str_detect(optimal.s, "CONST-IND")){
    optimal <- 1
    n.par <- 2
  } else if (str_detect(optimal.s, "CONST-CLUST")) {
    (optimal <- 2)
    n.par <- 2
  }

  #############
  ### Check if Lower and upper boundary parameters has a correct length
  #############

    if  (!is.null(lb) & (length(lb) != n.par)) {
      stop ("Lower bounds vector size does not match with parameters size.")
    }
    if (!is.null(ub) & (length(ub) != n.par)) {
      stop ("Upper bounds vector size does not match with parameters size.")
    }
    if (!is.null(lb) & !is.null(ub) & length(lb) != length(ub)) {
      stop ("Lower and upper bounds vector do not have the same length.")
    }
    if (is.null(lb)){
      lb <- rep(1, n.par)
    }
    if (is.null(ub)){
      ub <- rep(1000, n.par)
    }

  #############
  ### Initial conditions: compute or prepare in a correct form
  ############

   beta <- 0.8

   if(is.null(initial.cond))  {

      initial.cond0 <- initialconditions.nA (delta = delta,
                                             sigma = sigma,
                                             rho = rho,
                                             alpha = alpha,
                                             beta = beta,
                                             q  = q,
                                             v0 = v0,
                                             v1 = v1,
                                             f0 = f0,
                                             f1 = f1,
                                             C  = C,
                                             model = "MP")

      initial.cond <- as.data.frame(initial.cond0[, c("scenario" , "m.ic", "m.ic", "k.ic", "k.ic")])
      colnames(initial.cond) <- c('scenario','m0.ic','m1.ic', 'k0.ic', 'k1.ic')

   }else{

     if (length(initial.cond)!=4){
       stop("Incorrect length for the initial conditions vector. The length must be equal to 4, the number of parameters m and k.")
     }

     scenario <- c(1:max(length(delta), length(sigma), length(rho), length(beta), length(v0), length(v1), length(f0), length(f1)))

     for (i in 1:length(initial.cond)){
       if (initial.cond[[i]] < 1){
         stop("Incorrect value for the initial conditions vector. The lower boundary of the parameters m and k is 1.")
       }
     }

     m0.ic <- initial.cond[[1]]
     m1.ic <- initial.cond[[2]]
     k0.ic  <- initial.cond[[3]]
     k1.ic  <- initial.cond[[4]]

     initial.cond <- as.data.frame(cbind(scenario, m0.ic, m1.ic, k0.ic, k1.ic))
   }


  #############
  ### Estimate the maximum power
  #############

  max.power.nA <- maxpower.nA (delta = delta,
                               sigma = sigma,
                               rho = rho,
                               alpha = alpha,
                               C = C,
                               q = q,
                               v0 = v0,
                               v1 = v1,
                               f0 = f0,
                               f1 = f1,
                               optimal = optimal,
                               initial.cond = initial.cond,
                               lb = lb,
                               ub = ub,
                               temp = temp)

  ### Return final matrix with all the parameters and the maximum power or Export results to excel

   if (is.null(output)){

    return (max.power.nA)

  } else {

    objects  <- list(output)
    nobjects <- length(objects)

    for (i in 1:nobjects) {
      if (i == 1)
        write.xlsx2(as.data.frame(max.power.nA), file = paste0(objects[[1]],'.xlsx'), col.names=TRUE, row.names=TRUE, append=FALSE)
    }
  }
}
