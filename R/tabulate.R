#' Stata-Style Tabulate function
#'
#' Provides an interactive console in R that mimics Stata's `tabulate` command.
#' Users can type commands at the `StataR>` prompt to create frequency tables
#' for categorical variables, optionally summarizing by a grouping variable.
#' The output includes frequencies, percentages, cumulative percentages, and totals.
#'
#' @param df A data frame containing the categorical variables to tabulate.
#'
#' @return Prints formatted frequency tables to the console. The function does not return a value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start the interactive StataR tabulate console
#' stata_tabulate(ggplot2::diamonds)
#'
#' # In the console, example commands:
#'  tabulate cut
#'  tabulate cut, by(color)
#'  exit
#' }
stata_tabulate <- function(df) {

  do_tab <- function(tab_var, by_var = NULL) {
    if (!is.null(by_var)) {
      df[[by_var]] <- as.factor(df[[by_var]])
      df[[tab_var]] <- as.factor(df[[tab_var]])
      levels_by <- levels(df[[by_var]])

      for (lvl in levels_by) {
        cat(rep("-", 83), "\n", sep = "")
        cat("->", by_var, "=", lvl, "\n")

        sub_df <- df[df[[by_var]] == lvl, ]
        tab <- table(sub_df[[tab_var]])
        freq <- as.numeric(tab)
        percent <- round(100 * freq / sum(freq), 2)
        cum <- round(cumsum(percent), 2)

        cat(sprintf("%-10s | %4s %7s %7s\n", tab_var, "Freq.", "Percent", "Cum."))
        cat(rep("-", 12), "+", rep("-", 35), "\n", sep = "")

        for (i in seq_along(tab)) {
          cat(sprintf("%-10s | %4d %7.2f %7.2f\n",
                      names(tab)[i], freq[i], percent[i], cum[i]))
        }

        cat(rep("-", 12), "+", rep("-", 35), "\n", sep = "")
        cat(sprintf("%-10s | %4d %7.2f %7.2f\n", "Total", sum(freq), 100, 100))
      }
      cat(rep("-", 83), "\n", sep = "")

    } else {
      # no by variable
      df[[tab_var]] <- as.factor(df[[tab_var]])
      tab <- table(df[[tab_var]])
      freq <- as.numeric(tab)
      percent <- round(100 * freq / sum(freq), 2)
      cum <- round(cumsum(percent), 2)

      cat(rep("-", 50), "\n", sep = "")
      cat(sprintf("%-10s | %4s %7s %7s\n", tab_var, "Freq.", "Percent", "Cum."))
      cat(rep("-", 12), "+", rep("-", 35), "\n", sep = "")

      for (i in seq_along(tab)) {
        cat(sprintf("%-10s | %4d %7.2f %7.2f\n",
                    names(tab)[i], freq[i], percent[i], cum[i]))
      }

      cat(rep("-", 12), "+", rep("-", 35), "\n", sep = "")
      cat(sprintf("%-10s | %4d %7.2f %7.2f\n", "Total", sum(freq), 100, 100))
      cat(rep("-", 50), "\n", sep = "")
    }
  }

  # ------------------------
  # StataR console
  repeat {
    cmd <- readline("StataR> ")
    cmd <- gsub("^\\s+|\\s+$", "", cmd)

    if (tolower(cmd) %in% c("quit","exit")) {
      cat("Exiting StataR tabulate console.\n")
      break
    }

    # parse: tabulate var [, by(var)]
    if (!grepl("^tabulate", cmd, ignore.case = TRUE)) {
      cat("Unknown command. Use: tabulate var [, by(var)]\n")
      next
    }

    # remove "tabulate"
    cmd <- gsub("^tabulate\\s+", "", cmd, ignore.case = TRUE)

    # check for by
    if (grepl(",\\s*by\\(", cmd, ignore.case = TRUE)) {
      # split var and by
      parts <- strsplit(cmd, ",\\s*by\\(", perl=TRUE)[[1]]
      tab_var <- trimws(parts[1])
      by_var <- gsub("\\)", "", trimws(parts[2]))
      do_tab(tab_var, by_var)
    } else {
      tab_var <- trimws(cmd)
      do_tab(tab_var)
    }
  }
}


