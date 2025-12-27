#' Stata-Style Tabulate with Chi-Square Test
#'
#' Provides an interactive console in R that mimics Stata's `tabulate` command with options
#' for chi-square tests, Fisher exact tests, cell percentages, and column percentages.
#' Users can type commands at the `StataR>` prompt to create frequency tables for two
#' categorical variables, optionally displaying test statistics and percentages.
#'
#' @param df A data frame containing the categorical variables to tabulate.
#' @param digits Number of decimal places to display for percentages and test statistics (default 2).
#'
#' @return Prints formatted contingency tables with optional chi-square or Fisher exact test results.
#'         The function prints output to the console and does not return a value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start interactive StataR chi-square console
#' stata_chi2(ggplot2::diamonds)
#'
#' # Example commands in the console:
#'  tabulate cut color chi2
#'  tabulate cut color chi2, cell
#'  tabulate cut color chi2, column
#'  exit
#' }
stata_chi2 <- function(df, digits = 2) {

  tab_chi2 <- function(df, row_var, col_var,
                       chi2 = FALSE,
                       cell = FALSE,
                       column = FALSE,
                       exact = FALSE,
                       digits = 2) {

    # Use only observed factor levels to match base R
    row_f <- droplevels(factor(df[[row_var]]))
    col_f <- droplevels(factor(df[[col_var]]))

    tbl <- table(row_f, col_f)
    rs <- rowSums(tbl)
    cs <- colSums(tbl)
    gt <- sum(tbl)

    # ----- TABLE FORMATTING -----
    rw <- max(nchar(c(row_var, rownames(tbl), "Total"))) + 2
    cw <- max(nchar(c(colnames(tbl), as.character(tbl)))) + 6
    hline <- paste(rep("-", rw + (ncol(tbl)+1)*cw), collapse="")

    # ----- KEY -----
    if (cell || column) {
      cat("+------------------+\n")
      cat("| Key              |\n")
      cat("| frequency        |\n")
      if (cell)   cat("| cell percentage  |\n")
      if (column) cat("| column percentage|\n")
      cat("+------------------+\n\n")
    }

    # ----- HEADER -----
    cat(sprintf("%-*s", rw, ""))
    for (c in colnames(tbl)) cat(sprintf("%*s", cw, c))
    cat(sprintf("%*s\n", cw, "Total"))
    cat(hline, "\n")

    # ----- BODY -----
    for (r in rownames(tbl)) {
      # Counts row
      cat(sprintf("%-*s", rw, r))
      for (c in colnames(tbl)) cat(sprintf("%*d", cw, tbl[r,c]))
      cat(sprintf("%*d\n", cw, rs[r]))

      # Percentages row
      if (cell || column) {
        cat(sprintf("%-*s", rw, ""))
        for (c in colnames(tbl)) {
          vals <- c()
          if (cell)   vals <- c(vals, sprintf(paste0("%.",digits,"f"), 100*tbl[r,c]/gt))
          if (column) vals <- c(vals, sprintf(paste0("%.",digits,"f"), 100*tbl[r,c]/cs[c]))
          cat(sprintf("%*s", cw, paste(vals, collapse=" ")))
        }
        vals <- c()
        if (cell)   vals <- c(vals, sprintf(paste0("%.",digits,"f"), 100*rs[r]/gt))
        if (column) vals <- c(vals, sprintf(paste0("%.",digits,"f"), 100*rs[r]/gt))
        cat(sprintf("%*s\n", cw, paste(vals, collapse=" ")))
      }

      cat(hline, "\n")
    }

    # ----- TOTAL ROW -----
    cat(sprintf("%-*s", rw, "Total"))
    for (c in colnames(tbl)) cat(sprintf("%*d", cw, cs[c]))
    cat(sprintf("%*d\n", cw, gt))

    if (cell || column) {
      cat(sprintf("%-*s", rw, ""))
      for (c in colnames(tbl)) {
        vals <- c()
        if (cell)   vals <- c(vals, "100.00")
        if (column) vals <- c(vals, "100.00")
        cat(sprintf("%*s", cw, paste(vals, collapse=" ")))
      }
      cat(sprintf("%*s\n", cw, "100.00"))
    }

    # ----- STATISTICAL TESTS -----
    if (chi2) {
      nr <- nrow(tbl)
      nc <- ncol(tbl)

      chi <- suppressWarnings(chisq.test(row_f, col_f))

      if (exact && nr == 2 && nc == 2) {
        # Fisher exact for 2x2
        ft <- fisher.test(tbl)
        cat(sprintf("\nPearson chi2 [exact]   Pr = %.4f\n", ft$p.value))
      } else if (exact && (nr != 2 || nc != 2)) {
        # Exact not possible for larger tables
        cat("NOTE: Exact test only available for 2x2 tables; using Pearson chi2.\n")
        cat(sprintf("\nPearson chi2(%d) = %.4f   Pr = %.4f\n",
                    chi$parameter, chi$statistic, chi$p.value))
      } else {
        # Standard chi-square
        cat(sprintf("\nPearson chi2(%d) = %.4f   Pr = %.4f\n",
                    chi$parameter, chi$statistic, chi$p.value))
      }
    }

    cat("\n")
  }

  # ================= CONSOLE =================
  repeat {
    cmd <- readline("StataR> ")
    cmd <- trimws(cmd)

    if (tolower(cmd) %in% c("exit","quit")) {
      cat("Exiting StataR console.\n")
      break
    }

    if (grepl("^tabulate|^tab", cmd, ignore.case=TRUE)) {
      parts <- strsplit(cmd, ",")[[1]]
      main  <- trimws(parts[1])
      opts  <- if (length(parts) > 1) tolower(parts[2]) else ""

      m <- unlist(strsplit(main, "\\s+"))
      if (length(m) < 3) {
        cat("Syntax: tabulate rowvar colvar [options]\n")
        next
      }

      row_var <- m[2]
      col_var <- m[3]

      if (!all(c(row_var, col_var) %in% names(df))) {
        cat("Variables not found.\n")
        next
      }

      tab_chi2(
        df,
        row_var,
        col_var,
        chi2   = grepl("chi2", main, ignore.case=TRUE),
        cell   = grepl("cell", opts),
        column = grepl("column|col", opts),
        exact  = grepl("exact", opts),
        digits = digits
      )
      next
    }

    cat("Unknown command. Use: tabulate rowvar colvar [, options]\n")
  }
}





