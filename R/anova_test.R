#' Stata-style One-Way ANOVA and Kruskal–Wallis function
#'
#' Provides an interactive Stata-like console in R for performing
#' one-way analysis of variance (ANOVA) and Kruskal–Wallis
#' equality-of-populations rank tests using Stata syntax.
#' Results are printed in tables closely resembling Stata output,
#' including group summaries and test statistics.
#'
#' Supported commands:
#' \code{oneway variable, by(group)}
#' \code{kwallis variable, by(group)}
#'
#' @param df A data frame containing the numeric outcome variable and
#'   a grouping variable with two or more levels.
#'
#' @return Invisibly returns \code{NULL}. Results are printed directly
#'   to the console in Stata-style format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start interactive StataR ANOVA test console
#' stata_anova(iris)
#'
#' ## In the console:
#' oneway Sepal.Length, by(Species)
#' kwallis Sepal.Length, by(Species)
#'  exit
#' }

stata_anova <- function(df) {

  repeat {
    cmd <- readline("StataR> ")
    if (tolower(cmd) %in% c("quit","exit")) {
      cat("Exiting StataR console.\n")
      break
    }

    # Trim spaces
    cmd <- gsub("^\\s+|\\s+$", "", cmd)

    # Detect command type
    is_oneway  <- grepl("^oneway\\s+", cmd)
    is_kwallis <- grepl("^kwallis\\s+", cmd)

    if (!is_oneway & !is_kwallis) {
      cat("Unknown command. Use:\n")
      cat("  oneway var, by(group)\n")
      cat("  kwallis var, by(group)\n")
      next
    }

    # Extract variables
    matches <- regmatches(
      cmd,
      regexec("(oneway|kwallis)\\s+([a-zA-Z0-9_.]+)\\s*,\\s*by\\(([a-zA-Z0-9_.]+)\\)", cmd)
    )[[1]]

    if (length(matches) < 4) {
      cat("Syntax error. Correct usage:\n")
      cat("  oneway var, by(group)\n")
      cat("  kwallis var, by(group)\n")
      next
    }

    test_type <- matches[2]
    var   <- matches[3]
    group <- matches[4]

    if (!var %in% names(df) | !group %in% names(df)) {
      cat("Variable or group not found in dataset.\n")
      next
    }

    df[[group]] <- as.factor(df[[group]])
    groups <- levels(df[[group]])

    # -----------------------------
    # Summary table
    # -----------------------------
    cat("\n| Summary of", var, "\n")
    cat(sprintf("%-10s | %-8s %-10s %5s\n", group, "Mean", "Std. Dev.", "Freq"))
    cat(paste(rep("-",50), collapse=""), "\n")

    total_vals <- c()

    for (g in groups) {
      x <- df[[var]][df[[group]] == g]
      cat(sprintf(
        "%-10s | %-8.3f %-10.3f %5d\n",
        g, mean(x), sd(x), length(x)
      ))
      total_vals <- c(total_vals, x)
    }

    cat(paste(rep("-",50), collapse=""), "\n")
    cat(sprintf(
      "%-10s | %-8.3f %-10.3f %5d\n",
      "Total", mean(total_vals), sd(total_vals), length(total_vals)
    ))

    # -----------------------------
    # ONE-WAY ANOVA
    # -----------------------------
    if (test_type == "oneway") {
      cat("\nAnalysis of Variance\n")
      cat(sprintf("%-15s %-8s %-5s %-10s %-8s %-8s\n",
                  "Source", "SS", "df", "MS", "F", "Prob > F"))
      cat(paste(rep("-",65), collapse=""), "\n")

      aov_res <- aov(as.formula(paste(var, "~", group)), data = df)
      anova_tab <- summary(aov_res)[[1]]

      cat(sprintf("%-15s %-8.3f %-5d %-10.3f %-8.2f %-8.4f\n",
                  "Between groups",
                  anova_tab[1,"Sum Sq"],
                  anova_tab[1,"Df"],
                  anova_tab[1,"Mean Sq"],
                  anova_tab[1,"F value"],
                  anova_tab[1,"Pr(>F)"]))

      cat(sprintf("%-15s %-8.3f %-5d %-10.3f\n",
                  "Within groups",
                  anova_tab[2,"Sum Sq"],
                  anova_tab[2,"Df"],
                  anova_tab[2,"Mean Sq"]))

      cat(paste(rep("-",65), collapse=""), "\n")
      cat(sprintf("%-15s %-8.3f %-5d\n",
                  "Total",
                  sum(anova_tab[,"Sum Sq"]),
                  sum(anova_tab[,"Df"])))

      # Bartlett test
      bart <- bartlett.test(df[[var]], df[[group]])
      cat(sprintf(
        "Bartlett's test for equal variances: chi2(%d) = %.4f  Prob>chi2 = %.4f\n",
        bart$parameter, bart$statistic, bart$p.value
      ))
    }

    # -----------------------------
    # KRUSKAL–WALLIS
    # -----------------------------
    if (test_type == "kwallis") {

      cat("\nKruskal--Wallis equality-of-populations rank test\n\n")

      x <- df[[var]]
      g <- as.factor(df[[group]])

      ranks <- rank(x, ties.method = "average")
      df_rank <- data.frame(group = g, rank = ranks)

      tab <- aggregate(rank ~ group, data = df_rank, sum)
      obs <- table(g)

      # Header
      cat(sprintf("%-10s %5s %10s\n", group, "Obs", "Rank sum"))
      cat(paste(rep("-", 35), collapse=""), "\n")

      # Body
      for (i in seq_len(nrow(tab))) {
        cat(sprintf(
          "%-10s %5d %10.2f\n",
          tab$group[i],
          obs[tab$group[i]],
          tab$rank[i]
        ))
      }

      cat(paste(rep("-", 35), collapse=""), "\n")

      # Kruskal-Wallis test
      kw <- kruskal.test(x, g)

      # Tie correction (Stata-like)
      ties <- table(x)
      tie_adj <- sum(ties^3 - ties) / (length(x)^3 - length(x))
      chi2_ties <- as.numeric(kw$statistic / (1 - tie_adj))

      df_kw <- length(levels(g)) - 1

      cat(sprintf("chi2(%d) = %.3f\n", df_kw, kw$statistic))
      cat(sprintf("Prob = %.4f\n", kw$p.value))
      cat(sprintf("chi2(%d) with ties = %.3f\n", df_kw, chi2_ties))
      cat(sprintf("Prob = %.4f\n", kw$p.value))

    }
  }
}


