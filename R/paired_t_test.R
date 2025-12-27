#' Stata-Style Paired t-Test
#'
#' Provides an interactive console in R to perform paired t-tests between two numeric variables
#' in a data frame, mimicking Stata's `ttest var1==var2` command. The console outputs a
#' Stata-style summary table with mean, standard error, standard deviation, 95% confidence intervals,
#' and t-test statistics including one-sided and two-sided p-values.
#'
#' @param df A data frame containing the paired numeric variables.
#'
#' @return Prints a Stata-style paired t-test table to the console and the t-test results including:
#' mean difference, standard error, t-statistic, degrees of freedom, and p-values. Returns invisibly `NULL`.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create example data
#' df <- data.frame(
#'   before = c(21, 21, 22, 19, 23, 21, 20, 18, 22, 21, 24, 20),
#'   after  = c(24, 22, 23, 20, 24, 22, 21, 19, 23, 22, 25, 21)
#' )
#'
#' # Start the interactive paired t-test console
#' stata_ttest_paired(df)
#'
#' # Example command in the console:
#' ttest before==after
#'  exit
#' }
stata_ttest_paired <- function(df) {

  repeat {
    cmd <- readline("StataR> ")
    if (tolower(cmd) %in% c("quit","exit")) { cat("Exiting StataR console.\n"); break }

    # Parse command like: ttest var1==var2
    if (!grepl("==", cmd)) {
      cat("Unknown command. Use: ttest var1==var2\n"); next
    }

    matches <- regmatches(cmd, regexec("ttest\\s+([a-zA-Z0-9_]+)==([a-zA-Z0-9_]+)", cmd))[[1]]
    if (length(matches) != 3) {
      cat("Error: Syntax should be 'ttest var1==var2'\n"); next
    }

    var1 <- matches[2]
    var2 <- matches[3]

    if (!var1 %in% names(df) | !var2 %in% names(df)) {
      cat("Variables not found in dataset.\n"); next
    }

    x <- df[[var1]]
    y <- df[[var2]]

    # Remove missing pairs
    idx <- complete.cases(x, y)
    x <- x[idx]; y <- y[idx]
    n <- length(x)

    # Differences
    d <- x - y
    mean_d <- mean(d)
    sd_d <- sd(d)
    se_d <- sd_d / sqrt(n)

    tval <- mean_d / se_d
    dfree <- n - 1
    ci <- mean_d + c(-1,1) * qt(0.975, dfree) * se_d

    # Probabilities
    p_less <- pt(tval, dfree)
    p_two <- 2 * min(pt(tval, dfree), 1-pt(tval, dfree))
    p_greater <- 1 - pt(tval, dfree)

    # -----------------------------
    # Print Stata-style table
    # -----------------------------
    cat("\nPaired t test\n\n")

    cat(sprintf("%-6s %3s %8s %10s %10s %23s\n",
                "Variable", "Obs", "Mean", "Std. err.", "Std. dev.", "[95% conf. interval]"))
    cat(paste(rep("-",75), collapse=""), "\n")

    # var1
    cat(sprintf("%-6s %3d %8.3f %10.7f %10.6f %8.5f %8.5f\n", var1, n, mean(x), sd(x)/sqrt(n),
                sd(x), mean(x) - qt(0.975,n-1)*sd(x)/sqrt(n), mean(x) + qt(0.975,n-1)*sd(x)/sqrt(n)))
    cat(paste(rep("-",75), collapse=""), "\n")

    # var2
    cat(sprintf("%-6s %3d %8.3f %10.7f %10.6f %8.5f %8.5f\n", var2, n, mean(y), sd(y)/sqrt(n),
                sd(y), mean(y) - qt(0.975,n-1)*sd(y)/sqrt(n), mean(y) + qt(0.975,n-1)*sd(y)/sqrt(n)))
    cat(paste(rep("-",75), collapse=""), "\n")

    # diff
    cat(sprintf("%-6s %3d %8.3f %10.7f %10.6f %8.5f %8.5f\n", "diff", n, mean_d, se_d, sd_d, ci[1], ci[2]))
    cat(paste(rep("-",75), collapse=""), "\n")

    # t-statistic
    cat(sprintf("\nmean(diff) = mean(%s - %s) t = %.4f\n", var1, var2, tval))
    cat(sprintf("H0: mean(diff) = 0 Degrees of freedom = %d\n", dfree))
    cat("Ha: mean(diff) < 0 Ha: mean(diff) != 0 Ha: mean(diff) > 0\n")
    cat(sprintf("Pr(T < t) = %.4f Pr(|T| > |t|) = %.4f Pr(T > t) = %.4f\n", p_less, p_two, p_greater))

      }
}






