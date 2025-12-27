#' Stata-Style Tabulate with Fisher Exact Test
#'
#' Provides an interactive console in R that mimics Stata's `tabulate` command for two categorical
#' variables and performs Fisher exact tests (for 2x2 tables) or simulated exact tests (for larger tables).
#' Users can also optionally display cell percentages and column percentages in the output.
#'
#' @param df A data frame containing the categorical variables to tabulate.
#' @param digits Number of decimal places to display for percentages and p-values (default 2).
#'
#' @return Prints formatted contingency tables to the console, including optional cell and column percentages.
#'         Performs Fisher exact tests and prints p-values. The function returns the `fisher.test` object invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start the interactive StataR Fisher exact console
#' stata_fisher(ggplot2::diamonds)
#'  # Example commands in the console:
#'  tabulate cut color exact
#'  tabulate cut color exact, cell
#'  tabulate cut color exact, column
#'  exit
#' }
stata_fisher <- function(df, digits = 2) {

  tab_fisher <- function(df, row_var, col_var,
                         cell = FALSE,
                         column = FALSE,
                         digits = 2) {

    # Use observed factor levels
    row_f <- droplevels(factor(df[[row_var]]))
    col_f <- droplevels(factor(df[[col_var]]))

    tbl <- table(row_f, col_f)
    rs <- rowSums(tbl)
    cs <- colSums(tbl)
    gt <- sum(tbl)

    # Table formatting
    rw <- max(nchar(c(row_var, rownames(tbl), "Total"))) + 2
    cw <- max(nchar(c(colnames(tbl), as.character(tbl)))) + 6
    hline <- paste(rep("-", rw + (ncol(tbl)+1)*cw), collapse="")

    # Key
    if (cell || column) {
      cat("+------------------+\n")
      cat("| Key              |\n")
      cat("| frequency        |\n")
      if (cell)   cat("| cell percentage  |\n")
      if (column) cat("| column percentage|\n")
      cat("+------------------+\n\n")
    }

    # Header
    cat(sprintf("%-*s", rw, ""))
    for (c in colnames(tbl)) cat(sprintf("%*s", cw, c))
    cat(sprintf("%*s\n", cw, "Total"))
    cat(hline, "\n")

    # Body
    for (r in rownames(tbl)) {
      cat(sprintf("%-*s", rw, r))
      for (c in colnames(tbl)) cat(sprintf("%*d", cw, tbl[r,c]))
      cat(sprintf("%*d\n", cw, rs[r]))

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

    # Total row
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

    # Fisher exact test
    nr <- nrow(tbl)
    nc <- ncol(tbl)
    if (nr == 2 && nc == 2) {
      ft <- fisher.test(tbl)
      cat(sprintf("\nFisher exact test p-value = %.4f\n", ft$p.value))
    } else {
      cat("NOTE: Exact test for tables larger than 2x2 is simulated using 100,000 replicates.\n")
      ft <- fisher.test(tbl, simulate.p.value = TRUE, B = 100000)
      cat(sprintf("\nFisher exact (simulated) p-value = %.4f\n", ft$p.value))
    }

    invisible(ft)
  }

  # ================= CONSOLE =================
  repeat {
    cmd <- readline("StataR> ")
    cmd <- trimws(cmd)

    if (tolower(cmd) %in% c("exit","quit")) {
      cat("Exiting StataR console.\n")
      break
    }

    # Parse tabulate command
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

      if (grepl("exact", cmd, ignore.case=TRUE)) {
        tab_fisher(df, row_var, col_var,
                   cell   = grepl("cell", opts),
                   column = grepl("column|col", opts),
                   digits = digits)
      } else if (grepl("chi2", cmd, ignore.case=TRUE)) {
        # Optional: you can call your existing chi2 function here
        cat("Use chi2 function for Pearson chi-square.\n")
      }

      next
    }

    cat("Unknown command. Use: tabulate rowvar colvar [, options]\n")
  }
}






