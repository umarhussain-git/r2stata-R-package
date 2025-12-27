#' Stata-style Logistic Regression Console
#'
#' Provides an interactive StataR console for fitting logistic regression
#' models using Stata-like syntax. The function accepts commands of the form
#' \code{logit depvar indepvars [, or]} and prints output closely resembling
#' Stata's \code{logit} command, including likelihood-ratio statistics,
#' pseudo R-squared, coefficient estimates or odds ratios, standard errors,
#' z-statistics, p-values, and 95\% confidence intervals.
#'
#' @param data A data frame containing the dependent and independent variables.
#'   The dependent variable must be binary (0/1 or a two-level factor).
#' @param digits Integer specifying the number of decimal places to display
#'   for coefficient estimates and confidence intervals (default = 4).
#'
#' @return Invisibly returns \code{NULL}. Results are printed to the console
#'   in a Stata-style regression table.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start interactive StataR post hoc console
#' data <- mtcars
#' data$am <- as.factor(data$am)
#'
#' stata_logit(data)
#'
#' # In the console:
#' logit am mpg hp wt
#' logit am mpg hp wt, or
#' quit
#' }
stata_logit <- function(data, digits = 4) {

  repeat {
    cmd <- readline("StataR> ")
    cmd <- gsub("^\\s+|\\s+$", "", cmd)

    if (tolower(cmd) %in% c("quit", "exit")) {
      cat("Exiting StataR logit console.\n")
      break
    }

    # Detect OR option
    or_flag <- FALSE
    if (grepl(",\\s*or$", cmd, ignore.case = TRUE)) {
      or_flag <- TRUE
      cmd <- gsub(",\\s*or$", "", cmd, ignore.case = TRUE)
    }

    # Validate command
    if (!grepl("^logit\\s+", cmd, ignore.case = TRUE)) {
      cat("Unknown command. Use:\n")
      cat("  logit depvar indepvars [, or]\n")
      next
    }

    # Parse variables
    vars <- unlist(strsplit(
      gsub("^logit\\s+", "", cmd, ignore.case = TRUE),
      "\\s+"
    ))

    if (length(vars) < 2) {
      cat("Please specify a dependent variable and >=1 predictor.\n")
      next
    }

    dep <- vars[1]
    indep <- vars[-1]

    # Check variables
    missing <- vars[!vars %in% names(data)]
    if (length(missing) > 0) {
      cat("Variables not found:", paste(missing, collapse = ", "), "\n")
      next
    }

    # Convert outcome to 0/1 if factor
    if (is.factor(data[[dep]])) {
      if (length(levels(data[[dep]])) != 2)
        stop("Dependent variable must be binary (2 levels).")
      data[[dep]] <- as.numeric(data[[dep]]) - 1
    }

    # Convert character predictors to factors
    for (v in indep) {
      if (is.character(data[[v]]))
        data[[v]] <- as.factor(data[[v]])
    }

    # Build model
    formula_obj <- as.formula(paste(dep, "~", paste(indep, collapse = " + ")))
    fit <- glm(formula_obj, data = data, family = binomial)

    sm <- summary(fit)
    coefs <- sm$coefficients
    ci <- suppressMessages(confint(fit))
    rn <- rownames(coefs)

    # Odds ratio option
    if (or_flag) {
      coefs[, 1] <- exp(coefs[, 1])
      ci <- exp(ci)
      coef_label <- "Odds Ratio"
    } else {
      coef_label <- "Coef."
    }

    # Model stats
    n <- nrow(data)
    ll <- as.numeric(logLik(fit))
    null_ll <- as.numeric(
      logLik(glm(reformulate("1", dep), data = data, family = binomial))
    )
    lr_chi2 <- 2 * (ll - null_ll)
    df <- nrow(coefs) - 1
    p_lr <- 1 - pchisq(lr_chi2, df)
    pseudo_r2 <- 1 - ll / null_ll

    # Output
    line <- function() cat(strrep("-", 79), "\n")

    line()
    cat("Logistic regression                         Number of obs =", n, "\n")
    cat("                                              LR chi2(", df, ") =", round(lr_chi2, 2), "\n")
    cat("                                              Prob > chi2 =", formatC(p_lr, digits = 4, format = "f"), "\n")
    cat("Log likelihood =", round(ll, 5),
        "                   Pseudo R2 =", round(pseudo_r2, 4), "\n")
    line()

    cat(sprintf("%-12s %12s %12s %8s %10s %24s\n",
                dep, coef_label, "Std. Err.", "z", "P>|z|", "[95% Conf. Interval]"))
    line()

    for (i in seq_len(nrow(coefs))) {
      cat(sprintf(
        "%-12s %12s %12s %8s %10s %12s %12s\n",
        rn[i],
        formatC(coefs[i, 1], digits = digits, format = "f"),
        formatC(coefs[i, 2], digits = digits, format = "f"),
        formatC(coefs[i, 3], digits = 3, format = "f"),
        formatC(coefs[i, 4], digits = 3, format = "f"),
        formatC(ci[i, 1], digits = digits, format = "f"),
        formatC(ci[i, 2], digits = digits, format = "f")
      ))
    }

    line()
  }
}


