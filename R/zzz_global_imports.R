# zzz_global_imports.R
# This file imports all functions from stats used in r2stata
# so R CMD check does not give "no visible global function" notes

#' @importFrom stats binomial chisq.test complete.cases confint fisher.test glm
#'   kruskal.test lm logLik model.frame model.response na.omit pchisq pf
#'   pnorm pt qt quantile reformulate sd t.test var TukeyHSD aggregate aov
#'   as.formula bartlett.test
NULL
