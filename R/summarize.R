#' Stata-style Summarize function
#'
#' Provides an interactive console in R that mimics Stata's `summarize` command.
#' Users can type commands at the `StataR>` prompt to summarize variables in a
#' data frame, optionally using `detail` for extended statistics or `by(group)`
#' to summarize within levels of a categorical variable.
#'
#' @param df A data frame containing the variables to summarize.
#' @param digits Number of decimal places to display in numeric summaries (default is 3).
#'
#' @return Prints formatted summary tables to the console. The function does not return a value.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' stata_summarize(mtcars)
#' summarize mpg
#' summarize mpg, detail
#' summarize mpg hp, by(cyl)
#'  exit
#' }
stata_summarize <- function(df, digits = 3) {

  repeat {
    cmd <- readline("StataR> ")
    cmd <- gsub("^\\s+|\\s+$", "", cmd)
    if (tolower(cmd) %in% c("quit","exit")) {
      cat("Exiting StataR console.\n"); break
    }

    # Parse command
    if (!grepl("^(sum|summarize)", cmd, ignore.case = TRUE)) {
      cat("Unknown command. Use: summarize [var1 var2 ...] [, by(group)] [, detail]\n"); next
    }

    # Detail option
    detail <- FALSE
    if (grepl(",\\s*detail", cmd, ignore.case = TRUE)) {
      detail <- TRUE
      cmd <- gsub(",\\s*detail", "", cmd, ignore.case = TRUE)
    }

    # By variable
    by_var <- NULL
    if (grepl(",\\s*by\\(([a-zA-Z0-9_.]+)\\)", cmd, ignore.case = TRUE)) {
      by_var <- sub(".*,\\s*by\\(([a-zA-Z0-9_.]+)\\).*", "\\1", cmd, ignore.case = TRUE)
      cmd <- sub(",\\s*by\\(([a-zA-Z0-9_.]+)\\)", "", cmd, ignore.case = TRUE)
    }

    # Variables
    vars <- gsub("^(sum|summarize)\\s*", "", cmd, ignore.case = TRUE)
    vars <- gsub("^\\s+|\\s+$", "", vars)
    if (vars == "") { vars <- NULL } else {
      vars <- unlist(strsplit(vars, "\\s+"))
      vars <- vars[vars %in% names(df)]
      if (length(vars) == 0) { cat("No valid variables found.\n"); next }
    }

    # Helper: print dotted line
    dotted_line <- function(width) cat(paste(rep("-", width), collapse=""), "\n")

    # Detailed summary
    detailed_summary <- function(x, varname) {
      n <- sum(!is.na(x))
      wgt_sum <- n
      meanx <- mean(x)
      sdx <- sd(x)
      varx <- var(x)
      skew <- if(n>2) sum((x-meanx)^3/((n-1)*sd(x)^3)) else NA
      kurt <- if(n>3) sum((x-meanx)^4/((n-1)*sd(x)^4)) else NA
      pct <- quantile(x, probs=c(0.01,0.05,0.10,0.25,0.5,0.75,0.9,0.95,0.99), na.rm=TRUE)
      smallest <- sort(x)[1:min(2,length(x))]
      largest  <- sort(x, decreasing = TRUE)[1:min(2,length(x))]

      cat("\n", varname, "\n")
      dotted_line(40)
      cat("Percentiles\tSmallest\n")
      for(i in 1:length(pct)) cat(names(pct)[i], "\t", pct[i], if(i<=length(smallest)) paste0("\t", smallest[i]) else "", "\n")
      dotted_line(40)
      cat("Obs", n, "\tSum of wgt.", wgt_sum, "\n")
      cat("Mean", meanx, "\tStd. dev.", sdx, "\n")
      cat("Variance", varx, "\tSkewness", ifelse(is.na(skew), ".", skew), "\tKurtosis", ifelse(is.na(kurt), ".", kurt), "\n")
      cat("Largest\n")
      for(i in 1:length(largest)) cat(largest[i], "\n")
      dotted_line(40)
    }

    # Summarize function
    summarize_table <- function(df_sub) {
      header <- sprintf("%-15s %6s %12s %10s %10s %10s", "Variable", "Obs", "Mean", "Std. Dev.", "Min", "Max")
      sep <- paste(rep("-", nchar(header)), collapse = "")
      dotted_line(nchar(header))
      cat(header, "\n")
      dotted_line(nchar(header))
      for (v in names(df_sub)) {
        x <- df_sub[[v]]
        cat(sprintf("%-15s %6d %12.3f %10.3f %10.3f %10.3f\n",
                    v, sum(!is.na(x)), mean(x, na.rm=TRUE),
                    sd(x, na.rm=TRUE), min(x, na.rm=TRUE), max(x, na.rm=TRUE)))
      }
      dotted_line(nchar(header))
    }

    # Apply by variable if exists
    if (!is.null(by_var)) {
      if (!by_var %in% names(df)) { cat("By variable not found.\n"); next }
      df[[by_var]] <- as.factor(df[[by_var]])
      groups <- levels(df[[by_var]])
      for (g in groups) {
        cat("\n-> ", by_var, " = ", g, "\n", sep="")
        df_sub <- if (!is.null(vars)) df[df[[by_var]]==g, vars, drop=FALSE] else df[df[[by_var]]==g, , drop=FALSE]
        for (v in names(df_sub)) {
          if (detail & is.numeric(df_sub[[v]])) detailed_summary(df_sub[[v]], v)
          else summarize_table(df_sub[v])
        }
      }
    } else {
      df_sub <- if (!is.null(vars)) df[vars] else df
      for (v in names(df_sub)) {
        if (detail & is.numeric(df_sub[[v]])) detailed_summary(df_sub[[v]], v)
        else summarize_table(df_sub[v])
      }
    }
  }
}




