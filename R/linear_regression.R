#' Stata-style Linear Regression Console
#'
#' Provides an interactive Stata-like console for performing linear regression
#' using \code{lm()}, mimicking the output format of Stata's \code{regress}
#' command. The function displays ANOVA decomposition (Model, Residual, Total),
#' F-statistic, R-squared values, Root MSE, and a coefficient table with
#' confidence intervals.
#'
#' @param data A data frame containing the variables used in the regression.
#' @param digits Integer specifying the number of decimal places to display
#'   for coefficients and sums of squares.
#'
#' @return Invisibly returns the fitted \code{lm} object.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start interactive StataR linear regression console
#' stata_regress(mtcars)
#' ## In console:
#' regress mpg wt hp
#' regress mpg wt
#'  exit
#' }
stata_regress <- function(data, digits = 4) {

  do_regress <- function(formula, data, digits = 4) {
    fit <- lm(formula, data = data)
    sm <- summary(fit)
    coefs <- sm$coefficients
    pnames <- rownames(coefs)

    # Sample sizes
    n <- length(fit$fitted.values)
    p <- length(coefs[,1])
    k <- p - 1

    # SS, MS
    y <- model.response(model.frame(fit))
    yhat <- fit$fitted.values
    resid <- fit$residuals

    SS_total <- sum((y - mean(y))^2)
    SS_resid <- sum(resid^2)
    SS_model <- SS_total - SS_resid

    df_model <- k
    df_resid <- n - p
    df_total <- n - 1

    MS_model <- SS_model / df_model
    MS_resid <- SS_resid / df_resid

    # F-statistic
    Fstat <- MS_model / MS_resid
    pf_prob <- pf(Fstat, df_model, df_resid, lower.tail = FALSE)

    # Root MSE
    rootMSE <- sqrt(MS_resid)

    # Confidence intervals for coefficients
    alpha <- 0.05
    tval <- qt(1 - alpha/2, df_resid)
    ci_low <- coefs[,1] - tval * coefs[,2]
    ci_high <- coefs[,1] + tval * coefs[,2]

    # -----------------------
    # Print Stata-style header
    line <- function() cat(rep("-", 79), "\n", sep = "")
    line()
    cat("Source           SS        df       MS    Number of obs =", n, "\n")
    cat("Model   ", formatC(SS_model, digits=digits, format="f"), " ", df_model, " ",
        formatC(MS_model, digits=digits, format="f"),
        "  F(", df_model, ",", df_resid, ") =", formatC(Fstat, digits=2, format="f"),
        "  Prob > F =", formatC(pf_prob, digits=4, format="f"), "\n")
    cat("Residual", formatC(SS_resid, digits=digits, format="f"), " ", df_resid, " ",
        formatC(MS_resid, digits=digits, format="f"), "\n")
    cat("Total   ", formatC(SS_total, digits=digits, format="f"), " ", df_total, " ",
        formatC(SS_total/df_total, digits=digits, format="f"),
        "  R-squared =", formatC(sm$r.squared, digits=4, format="f"),
        "  Adj R-squared =", formatC(sm$adj.r.squared, digits=4, format="f"),
        "  Root MSE =", formatC(rootMSE, digits=4, format="f"), "\n")

    # -----------------------
    # Print coefficients table
    line()
    cat(sprintf("%-12s %12s %12s %8s %10s %12s %12s\n",
                "Variable", "Coef.", "Std. Err.", "t", "P>|t|", "[95% Conf. Interval]",""))
    line()

    for (i in seq_len(nrow(coefs))) {
      cat(sprintf("%-12s %12s %12s %8s %10s %12s %12s\n",
                  pnames[i],
                  formatC(coefs[i,1], digits=digits, format="f"),
                  formatC(coefs[i,2], digits=digits, format="f"),
                  formatC(coefs[i,3], digits=2, format="f"),
                  formatC(coefs[i,4], digits=3, format="f"),
                  formatC(ci_low[i], digits=digits, format="f"),
                  formatC(ci_high[i], digits=digits, format="f")
      ))
    }
    line()
    invisible(fit)
  }

  # -----------------------
  # StataR console
  repeat {
    cmd <- readline("StataR> ")
    cmd <- gsub("^\\s+|\\s+$", "", cmd)

    if (tolower(cmd) %in% c("quit","exit")) {
      cat("Exiting StataR regression console.\n")
      break
    }

    if (!grepl("^regress", cmd, ignore.case = TRUE)) {
      cat("Unknown command. Use: regress dep_var indep_vars...\n")
      next
    }

    cmd <- gsub("^regress\\s+", "", cmd, ignore.case = TRUE)
    vars <- unlist(strsplit(cmd, "\\s+"))

    if (length(vars) < 2) {
      cat("Provide dependent and at least one independent variable.\n")
      next
    }

    dep <- vars[1]
    indep <- vars[-1]
    all_vars <- c(dep, indep)

    missing <- all_vars[!all_vars %in% names(data)]
    if (length(missing) > 0) {
      cat("Variables not found:", paste(missing, collapse=", "), "\n")
      next
    }

    formula_obj <- as.formula(paste(dep, "~", paste(indep, collapse=" + ")))
    do_regress(formula_obj, data, digits)
  }
}

