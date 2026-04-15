#' Summarize count data
#'
#' Produces a numerical and graphical summary of a count response variable,
#' with the plot automatically adapting to the number and type of predictors
#' in the formula. A pairs plot is always returned alongside the main plot.
#'
#' @param formula A formula of the form \code{y ~ x1 + x2 + ...} where \code{y}
#'   is a non-negative integer count response. Offsets via \code{offset()} are
#'   supported.
#' @param data A data frame containing the variables in \code{formula}.
#'
#' @return A named list with four elements:
#' \describe{
#'   \item{summary}{A one-row \code{data.frame} containing:
#'     \code{mean}, \code{var}, \code{var_mean_ratio}, \code{n_zero},
#'     and \code{n_total}.}
#'   \item{counts}{A \code{data.frame} with columns \code{count} and \code{freq}
#'     giving the frequency of each observed count value.}
#'   \item{plot}{A \code{ggplot} object. The plot type depends on the number
#'     and type of predictors - see Details.}
#'   \item{pairs_plot}{A \code{ggpairs} object showing all pairwise
#'     relationships among the response and all predictors. The response is
#'     treated as continuous when it has more than 10 unique values, and as
#'     categorical (factor) otherwise.}
#' }
#'
#' @details
#' The graphical summary is chosen based on the predictors in \code{formula}:
#' \describe{
#'   \item{No predictors}{Histogram of the count response.}
#'   \item{One continuous predictor}{Scatter plot with a loess smooth.}
#'   \item{One categorical predictor}{Violin plot with jittered points.}
#'   \item{Two continuous predictors}{2D bin plot (\code{geom_bin2d}) with
#'     a viridis fill scale.}
#'   \item{Two categorical predictors}{Tile heatmap of mean counts.}
#'   \item{One continuous, one categorical predictor}{Scatter plot with loess
#'     smooths coloured by the categorical variable.}
#'   \item{Three or more predictors}{A warning is issued and only the first
#'     two predictors are used for the main plot. The pairs plot includes all
#'     predictors.}
#' }
#'
#' The \code{var_mean_ratio} in the summary table is the variance-to-mean
#' ratio. A value close to 1 is consistent with a Poisson distribution;
#' values substantially greater than 1 suggest overdispersion.
#'
#' @examples
#' set.seed(1)
#' df <- data.frame(
#'   y  = rpois(100, lambda = 3),
#'   x1 = rnorm(100),
#'   x2 = sample(c("A", "B"), 100, replace = TRUE)
#' )
#'
#' # No predictors
#' summarizeCountData(y ~ 1, data = df)
#'
#' # One continuous predictor
#' summarizeCountData(y ~ x1, data = df)
#'
#' # One categorical predictor
#' summarizeCountData(y ~ x2, data = df)
#'
#' # Mixed predictors
#' summarizeCountData(y ~ x1 + x2, data = df)
#'
#' @seealso \code{\link{plot_count_data}}
#'
#' @importFrom rlang .data
#' @export
summarizeCountData = function(formula, data) {
  mf = model.frame(formula, data)
  Terms = terms(formula, data = data)
  y = model.response(mf)

  if (is.null(y)) {
    stop("No response found in formula/data")
  }
  if (is.matrix(y)) {
    stop("Response must be a vector, not a matrix")
  }
  if (!is.numeric(y) || any(y < 0) || any(y != floor(y))) {
    stop("Response must be non-negative integer counts")
  }

  off = model.offset(mf)
  if (is.null(off)) {
    off = numeric(length(y))
  }

  X = model.matrix(Terms, mf)
  n = length(y)

  count_table = as.data.frame(table(y), stringsAsFactors = FALSE)
  names(count_table) = c("count", "freq")
  count_table$count = as.integer(as.character(count_table$count))

  summary_table = data.frame(
    mean = mean(y),
    var = var(y),
    var_mean_ratio = var(y) / mean(y),
    n_zero = sum(y == 0),
    n_total = n
  )

  # Extract predictors
  resp_name = as.character(formula[[2]])
  pred_vars = setdiff(names(mf), resp_name)
  pred_vars = pred_vars[!grepl("offset\\(", pred_vars)]
  pred_types = sapply(mf[pred_vars], function(v) {
    if (is.numeric(v)) "continuous" else "categorical"
  })

  p = plot_count_data(y, mf, pred_vars, pred_types)

  # Pairs plot: treat response as continuous if >10 unique values, else factor
  n_unique_y <- length(unique(y))
  y_pairs    <- if (n_unique_y > 10) y else factor(y)
  df_pairs   <- data.frame(y = y_pairs, mf[pred_vars])
  names(df_pairs)[1] <- resp_name
  pairs_p <- GGally::ggpairs(df_pairs)

  list(
    summary    = summary_table,
    counts     = count_table,
    plot       = p,
    pairs_plot = pairs_p
  )
}

#' Plot count data against predictors
#'
#' Internal plotting function called by \code{\link{summarizeCountData}}.
#' Selects an appropriate \code{ggplot} based on the number and type of
#' predictors supplied.
#'
#' @param y Numeric vector of non-negative integer counts (the response).
#' @param mf A model frame returned by \code{\link{model.frame}}.
#' @param pred_vars Character vector of predictor variable names to plot.
#'   If more than two names are supplied, only the first two are used and
#'   a warning is issued.
#' @param pred_types Named character vector classifying each element of
#'   \code{pred_vars} as either \code{"continuous"} or \code{"categorical"}.
#'
#' @return A \code{ggplot} object.
#'
#' @details
#' See \code{\link{summarizeCountData}} for the full description of which
#' plot type is produced for each predictor combination.
#'
#' @seealso \code{\link{summarizeCountData}}
#'
#' @importFrom rlang .data
#' @keywords internal
plot_count_data = function(y, mf, pred_vars, pred_types) {
  df = data.frame(y = y, mf[pred_vars])
  n_pred = length(pred_vars)

  if (n_pred == 0) {
    # No predictors - just show distribution of counts
    p = ggplot2::ggplot(df, ggplot2::aes(x = y)) +
      ggplot2::geom_histogram(
        binwidth = 1,
        fill = "steelblue",
        color = "white"
      ) +
      ggplot2::labs(x = "Count", y = "Frequency")
  } else if (n_pred == 1) {
    x1 = pred_vars[1]

    if (pred_types[1] == "continuous") {
      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[[x1]], y = y)) +
        ggplot2::geom_point(alpha = 0.4) +
        ggplot2::geom_smooth(method = "loess", se = TRUE) +
        ggplot2::labs(y = "Count")
    } else {
      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[[x1]], y = y)) +
        ggplot2::geom_violin(fill = "steelblue", alpha = 0.6) +
        ggplot2::geom_jitter(width = 0.1, alpha = 0.3) +
        ggplot2::labs(y = "Count")
    }
  } else if (n_pred == 2) {
    x1 = pred_vars[1]
    x2 = pred_vars[2]
    types = unname(pred_types)

    if (all(types == "continuous")) {
      p = ggplot2::ggplot(df, ggplot2::aes(x = .data[[x1]], y = .data[[x2]])) +
        ggplot2::geom_bin2d(bins = c(4, 4)) +
        ggplot2::scale_fill_steps() +
        ggplot2::labs(fill = "Count")
    } else if (all(types == "categorical")) {
      agg = aggregate(y ~ ., data = df, FUN = mean)
      p = ggplot2::ggplot(
        agg,
        ggplot2::aes(x = .data[[x1]], y = .data[[x2]], fill = y)
      ) +
        ggplot2::geom_tile() +
        ggplot2::scale_fill_steps() +
        ggplot2::labs(fill = "Mean count")
    } else {
      # One of each - put continuous on x, facet or fill by categorical
      cont_var = pred_vars[which(pred_types == "continuous")]
      cat_var = pred_vars[which(pred_types == "categorical")]

      p = ggplot2::ggplot(
        df,
        ggplot2::aes(x = .data[[cont_var]], y = y, color = .data[[cat_var]])
      ) +
        ggplot2::geom_point(alpha = 0.4) +
        ggplot2::geom_smooth(method = "loess", se = FALSE) +
        ggplot2::labs(y = "Count")
    }
  } else {
    # >3 predictors - use first two and warn
    warning("More than 2 predictors - plotting first two only (see pairs_plot for all)")
    return(plot_count_data(y, mf, pred_vars[1:2], pred_types[1:2]))
  }

  p + ggplot2::theme_minimal()
}
