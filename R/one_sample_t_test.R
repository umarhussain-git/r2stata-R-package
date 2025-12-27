#' Stata-style One-Sample t Test function
#'
#' Provides an interactive Stata-like console for performing one-sample
#' t tests in R using Stata syntax. The function evaluates whether the mean
#' of a numeric variable differs from a specified null value and prints
#' results in a format closely resembling Stata’s \code{ttest} output.
#'
#' Supported syntax:
#' \code{ttest variable==value}
#'
#' @param df A data frame containing the numeric variable to be tested.
#'
#' @return Invisibly returns \code{NULL}. Test results are printed directly
#'   to the console in Stata-style format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start interactive StataR one sample t test console
#' stata_ttest_onesample(mtcars)
#'
#' # In the console
#'  # using estimated mean of 20 for mpg
#' ttest mpg==20
#'  exit
#' }
stata_ttest_onesample <- function(df) {

  repeat {
    cmd <- readline("StataR> ")
    if (tolower(cmd) %in% c("quit","exit")) { cat("Exiting StataR console.\n"); break }

    if (!grepl("==", cmd)) {
      cat("Unknown command or syntax. Use: ttest variable==value\n"); next
    }

    # Extract variable and mu using regex
    matches <- regmatches(cmd, regexec("ttest\\s+([a-zA-Z0-9_]+)==([0-9.]+)", cmd))[[1]]
    if (length(matches) != 3) {
      cat("Error: Syntax should be 'ttest variable==value'\n"); next
    }

    var <- matches[2]
    mu <- as.numeric(matches[3])

    if (!var %in% names(df)) { cat("Variable", var, "not found.\n"); next }

    # Prepare data
    x <- na.omit(df[[var]])
    n <- length(x)
    mean_x <- mean(x)
    sd_x <- sd(x)
    se <- sd_x / sqrt(n)
    dfree <- n - 1

    # t-statistic
    tval <- (mean_x - mu)/se

    # 95% confidence interval
    ci <- mean_x + c(-1,1) * qt(0.975, dfree) * se

    # Probabilities
    p_less <- pt(tval, dfree)
    p_two <- 2 * min(pt(tval, dfree), 1-pt(tval, dfree))
    p_greater <- 1 - pt(tval, dfree)

    # -----------------------------
    # Print Stata-style output
    # -----------------------------
    cat("\nOne-sample t test\n")

    # Header
    cat(sprintf("%-10s %3s %8s %10s %10s %23s\n",
                "Variable", "Obs", "Mean", "Std. err.", "Std. dev.", "[95% conf. interval]"))

    # Dotted line under header
    cat(paste(rep("-", 75), collapse=""), "\n")

    # Data row
    cat(sprintf("%-10s %3d %8.4f %10.7f %10.6f %8.5f %8.5f\n",
                var, n, mean_x, se, sd_x, ci[1], ci[2]))

    # t statistic
    cat(sprintf("\nmean = mean(%s) t = %.4f\n", var, tval))
    cat(sprintf("H0: mean = %.4f Degrees of freedom = %d\n", mu, dfree))
    cat("Ha: mean < mu Ha: mean != mu Ha: mean > mu\n")

    # Probabilities
    cat(sprintf("Pr(T < t) = %.4f Pr(|T| > |t|) = %.4f Pr(T > t) = %.4f\n\n",
                p_less, p_two, p_greater))
  }
}

