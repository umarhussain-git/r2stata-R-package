#' Stata-style Independent Two-Sample Tests
#'
#' Provides an interactive Stata-like console in R for performing
#' independent two-sample statistical tests. Supported commands include
#' \code{ttest} (equal variance and Welch) and \code{ranksum}
#' (Mann Whitney U test), with output formatted to closely resemble
#' Stata native results tables.
#'
#' @param df A data frame containing the numeric outcome variable and a
#'   two-level grouping variable.
#'
#' @return Invisibly returns \code{NULL}. Results are printed directly to
#'   the console in Stata-style format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' stata_ttest_indep(mtcars)
#'
#' # In the interactive console:
#' ttest mpg, by(am)
#' ttest mpg, by(am) welch
#' ranksum mpg, by(am)
#'  exit
#' }
#'
stata_ttest_indep <- function(df) {

  repeat {
    cmd <- readline("StataR> ")
    if (tolower(cmd) %in% c("quit","exit")) {
      cat("Exiting StataR console.\n"); break
    }

    # ----------------------------
    # Check for ttest command
    # ----------------------------
    ttest_matches <- regmatches(cmd, regexec("ttest\\s+([a-zA-Z0-9_]+),\\s*by\\(([a-zA-Z0-9_]+)\\)\\s*(welch)?", cmd))[[1]]

    # ----------------------------
    # Check for ranksum command
    # ----------------------------
    ranksum_matches <- regmatches(cmd, regexec("ranksum\\s+([a-zA-Z0-9_]+),\\s*by\\(([a-zA-Z0-9_]+)\\)", cmd))[[1]]

    if (length(ttest_matches) >= 3) {
      # --- t-test handling (existing code) ---
      var <- ttest_matches[2]
      by_var <- ttest_matches[3]
      use_welch <- length(ttest_matches) == 4 && ttest_matches[4] == "welch"

      if (!var %in% names(df)) { cat("Variable", var, "not found.\n"); next }
      if (!by_var %in% names(df)) { cat("Grouping variable", by_var, "not found.\n"); next }

      df[[by_var]] <- as.factor(df[[by_var]])
      groups <- levels(df[[by_var]])
      x <- df[[var]][df[[by_var]] == groups[1]]
      y <- df[[var]][df[[by_var]] == groups[2]]
      n1 <- length(na.omit(x)); n2 <- length(na.omit(y))
      mean1 <- mean(x, na.rm=TRUE); mean2 <- mean(y, na.rm=TRUE)
      sd1 <- sd(x, na.rm=TRUE); sd2 <- sd(y, na.rm=TRUE)
      se1 <- sd1 / sqrt(n1); se2 <- sd2 / sqrt(n2)

      if (use_welch) {
        t_res <- t.test(x, y, var.equal=FALSE)
        dfree <- t_res$parameter; tval <- t_res$statistic; se_diff <- t_res$stderr
        method <- "Two-sample t test with unequal variances"
      } else {
        sp <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2)/(n1+n2-2))
        se_diff <- sp * sqrt(1/n1 + 1/n2)
        tval <- (mean1 - mean2)/se_diff
        dfree <- n1 + n2 - 2
        method <- "Two-sample t test with equal variances"
      }

      # Combined
      combined <- na.omit(c(x,y))
      nC <- length(combined); meanC <- mean(combined); sdC <- sd(combined); seC <- sdC/sqrt(nC)
      ciC <- meanC + c(-1,1) * qt(0.975, nC-1) * seC
      diff <- mean1 - mean2
      ci_diff <- diff + c(-1,1) * qt(0.975, dfree) * se_diff

      # Print output
      cat("\n", method, "\n\n")
      cat(sprintf("%-6s %3s %8s %10s %10s %23s\n",
                  "Group", "Obs", "Mean", "Std. err.", "Std. dev.", "[95% conf. interval]"))
      cat(paste(rep("-", 75), collapse=""), "\n")
      cat(sprintf("%-6s %3d %8.3f %10.7f %10.6f %8.5f %8.5f\n", groups[1], n1, mean1, se1, sd1,
                  mean1 - qt(0.975, n1-1)*se1, mean1 + qt(0.975, n1-1)*se1))
      cat(paste(rep("-", 75), collapse=""), "\n")
      cat(sprintf("%-6s %3d %8.3f %10.7f %10.6f %8.5f %8.5f\n", groups[2], n2, mean2, se2, sd2,
                  mean2 - qt(0.975, n2-1)*se2, mean2 + qt(0.975, n2-1)*se2))
      cat(paste(rep("-", 75), collapse=""), "\n")
      cat(sprintf("%-6s %3d %8.3f %10.7f %10.6f %8.5f %8.5f\n", "Combined", nC, meanC, seC, sdC,
                  ciC[1], ciC[2]))
      cat(paste(rep("-", 75), collapse=""), "\n")
      cat(sprintf("%-6s %3s %8.3f %10.7f %10s %8.5f %8.5f\n", "diff", "", diff, se_diff, "", ci_diff[1], ci_diff[2]))
      cat(paste(rep("-", 75), collapse=""), "\n")
      cat(sprintf("\ndiff = mean(%s) - mean(%s) t = %.4f\n", groups[1], groups[2], tval))
      if (!use_welch) cat(sprintf("H0: diff = 0 Degrees of freedom = %d\n", dfree))
      else cat(sprintf("H0: diff = 0 Welch's degrees of freedom = %.4f\n", dfree))
      p_two <- 2*min(pt(tval, dfree), 1-pt(tval, dfree))
      cat("Ha: diff < 0 Ha: diff != 0 Ha: diff > 0\n")
      cat(sprintf("Pr(T < t) = %.4f Pr(|T| > |t|) = %.4f Pr(T > t) = %.4f\n",
                  pt(tval, dfree), p_two, 1-pt(tval, dfree)))

    } else if (length(ranksum_matches) >= 3) {
      # --- ranksum / Mann-Whitney test ---
      # --- ranksum / Mann-Whitney test ---
      var <- ranksum_matches[2]
      by_var <- ranksum_matches[3]
      if (!var %in% names(df)) { cat("Variable", var, "not found.\n"); next }
      if (!by_var %in% names(df)) { cat("Grouping variable", by_var, "not found.\n"); next }

      df[[by_var]] <- as.factor(df[[by_var]])
      groups <- levels(df[[by_var]])
      x <- df[[var]][df[[by_var]] == groups[1]]
      y <- df[[var]][df[[by_var]] == groups[2]]

      # Compute ranks
      combined <- na.omit(c(x,y))
      ranks <- rank(combined)
      rank_x <- sum(ranks[1:length(x)])
      rank_y <- sum(ranks[(length(x)+1):length(combined)])

      n1 <- length(x); n2 <- length(y)
      expected_x <- n1*(n1+n2+1)/2
      expected_y <- n2*(n1+n2+1)/2

      # Variance with tie adjustment
      tie_adj <- sum(table(combined)^3 - table(combined))/((n1+n2)*(n1+n2-1))/12
      var_adj <- n1*n2*(n1+n2+1)/12 - tie_adj
      z <- (rank_x - expected_x)/sqrt(var_adj)
      p_two <- 2 * (1 - pnorm(abs(z)))

      # Print Stata-style output with dotted lines
      cat("\nTwo-sample Wilcoxon rank-sum (Mann-Whitney) test\n\n")
      cat(sprintf("%-10s %5s %10s %10s\n", "Group", "Obs", "Rank sum", "Expected"))
      cat(paste(rep("-", 45), collapse=""), "\n")
      cat(sprintf("%-10s %5d %10.2f %10.2f\n", groups[1], n1, rank_x, expected_x))
      cat(paste(rep("-", 45), collapse=""), "\n")
      cat(sprintf("%-10s %5d %10.2f %10.2f\n", groups[2], n2, rank_y, expected_y))
      cat(paste(rep("-", 45), collapse=""), "\n")
      cat(sprintf("%-10s %5d %10.2f %10.2f\n", "Combined", n1+n2, sum(ranks), sum(ranks)))
      cat(paste(rep("-", 45), collapse=""), "\n")
      cat(sprintf("unadjusted variance %.2f\n", n1*n2*(n1+n2+1)/12))
      cat(sprintf("adjustment for ties %.2f\n", -tie_adj))
      cat(sprintf("adjusted variance %.2f\n", var_adj))
      cat(sprintf("Ho: %s(%s==%s) = %s(%s==%s)\n", var, by_var, groups[1], var, by_var, groups[2]))
      cat(sprintf("z = %.3f\n", z))
      cat(sprintf("Prob > |z| = %.4f\n", p_two))


    } else {
      cat("Unknown command. Use: ttest var, by(group) [welch] OR ranksum var, by(group)\n")
    }
  }
}


