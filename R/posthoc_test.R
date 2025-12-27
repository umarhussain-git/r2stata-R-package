#' StataR-like One-Way ANOVA with Tukey Post Hoc Test
#'
#' Provides an interactive Stata-like console in R for performing
#' one-way analysis of variance (ANOVA) followed by Tukey’s Honestly
#' Significant Difference (HSD) post hoc multiple-comparison test
#' in a single step. Group summaries, ANOVA tables, and Tukey
#' pairwise comparisons are printed in a console-friendly format
#' resembling Stata output.
#'
#' Supported command:
#' \code{tukey outcome, by(group)}
#'
#' @param data A data frame containing a numeric outcome variable and
#'   a categorical grouping variable with two or more levels.
#' @param digits Number of decimal places to display in the output.
#' @importFrom stats TukeyHSD aggregate aov as.formula bartlett.test binomial
#'
#' @return Invisibly returns \code{NULL}. Results are printed directly
#'   to the console in StataR-style format.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start interactive StataR post hoc console
#' stata_posthoc(iris)
#'
#' ## In the console:
#' tukey Sepal.Length, by(Species)
#'  exit
#' }
stata_posthoc <- function(data, digits = 4) {

  repeat {
    cmd <- readline("StataR> ")
    cmd <- gsub("^\\s+|\\s+$", "", cmd)

    if (tolower(cmd) %in% c("quit","exit")) {
      cat("Exiting StataR console.\n")
      break
    }

    # ----- Tukey ANOVA + post hoc -----
    if (grepl("^tukey\\s+", cmd, ignore.case = TRUE)) {
      parts <- unlist(strsplit(cmd, ","))
      if(length(parts) < 2) {
        cat("Invalid syntax. Example: tukey outcome, by(group)\n")
        next
      }

      outcome <- trimws(gsub("^tukey\\s+", "", parts[1], ignore.case = TRUE))
      group   <- trimws(gsub("by\\(|\\)", "", parts[2], ignore.case = TRUE))

      # Check variable existence
      missing <- c(outcome, group)[!(c(outcome, group) %in% names(data))]
      if(length(missing) > 0) {
        cat("Variables not found:", paste(missing, collapse=", "), "\n")
        next
      }

      data[[group]] <- as.factor(data[[group]])

      # --- Summary table ---
      summary_tbl <- aggregate(data[[outcome]], by = list(Group = data[[group]]),
                               FUN = function(x) c(Mean = mean(x), SD = sd(x), N = length(x)))

      summary_df <- data.frame(
        Group = summary_tbl$Group,
        Mean = round(sapply(summary_tbl$x, `[`, 1), digits),
        `Std. Dev.` = round(sapply(summary_tbl$x, `[`, 2), digits),
        N = sapply(summary_tbl$x, `[`, 3)
      )

      # Add total row
      total_row <- data.frame(
        Group = "Total",
        Mean = round(mean(data[[outcome]], na.rm = TRUE), digits),
        `Std. Dev.` = round(sd(data[[outcome]], na.rm = TRUE), digits),
        N = nrow(data)
      )
      summary_df <- rbind(summary_df, total_row)

      # Print summary table
      cat("| Summary of", outcome, "\n")
      line <- function() cat(rep("-", 60), "\n", sep = "")
      line()
      print(summary_df, row.names = FALSE)
      line()

      # --- ANOVA ---
      aov_model <- aov(as.formula(paste(outcome, "~", group)), data = data)
      cat("\nAnalysis of Variance\n")
      line()
      print(summary(aov_model)[[1]])
      line()

      # --- Tukey post hoc ---
      cat("\nPost Hoc Tukey HSD Comparisons:\n")
      line()
      tukey_res <- TukeyHSD(aov_model)
      for(g in names(tukey_res)) {
        print(round(tukey_res[[g]], digits))
      }
      line()
      next
    }

    cat("Unknown command. Use 'tukey outcome, by(group)'\n")
  }
}



