#' Histogram Q-Q Plot
#'
#' Overlays a histogram (rotated to rest on the Q-Q line) with the theoretical
#' and empirical density curves, producing a richer alternative to a standard
#' normal Q-Q plot. Written by Will Cipolli.
#'
#' @param data Numeric vector of observations to plot.
#' @param color Logical; if `TRUE`, histogram bars are coloured by deviation
#'   from the expected count (default `FALSE`).
#' @param square Logical; if `TRUE`, x and y axes are forced to the same limits
#'   (default `FALSE`).
#' @param dist Character string naming the distribution family (default
#'   `"norm"`). Must have corresponding `d<dist>` and `q<dist>` functions.
#' @param height.scale Numeric scaling factor for histogram bar height
#'   (default `1`).
#' @param ... Additional parameters passed to the distribution's density and
#'   quantile functions (e.g. `mean`, `sd` for `dist = "norm"`). If omitted,
#'   parameters are estimated automatically via `MASS::fitdistr`.
#' @return A `ggplot` object.
#' @importFrom dplyr mutate slice group_by summarise n case_when select
#' @importFrom tidyr replace_na
#' @importFrom tibble tibble
#' @importFrom ggplot2 ggplot aes geom_polygon geom_segment geom_path stat_qq
#'   stat_qq_line theme_bw labs scale_fill_gradient scale_linetype_manual
#'   coord_cartesian
#' @noRd
histoqq <- function(
  data,
  color = FALSE,
  square = FALSE,
  dist = "norm",
  height.scale = 1,
  ...
) {
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Please install 'MASS'")
  }
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Please install 'scales'")
  }
  ##################################################################
  # Validate Input
  ##################################################################
  # Setup PMF/PDF and CDF functions
  d.name <- paste0("d", dist)
  q.name <- paste0("q", dist)

  # Check if both functions actually exist
  if (
    !exists(q.name, mode = "function") || !exists(d.name, mode = "function")
  ) {
    stop(paste0(
      "Invalid distribution specified. Could not find functions '",
      q.name,
      "()' or '",
      d.name,
      "()'. Please check your specification and ensure any necessary libraries are loaded."
    ))
  }

  # Specify the quantile and density functions
  qdist <- match.fun(q.name)
  ddist <- match.fun(d.name)

  # Validate there is variance in the data
  countunique <- data |> na.omit() |> unique() |> length()
  if (countunique == 1) {
    stop("There is no variation in your data object.")
  }

  # Warn if data are dropped due to missingness or non-finite values
  npass <- length(data)
  data <- data[is.finite(data)]
  if (npass > length(data)) {
    warning(paste0(
      "There were ",
      (npass - length(data)),
      " missing observation(s). We removed these to make the plot."
    ))
  }

  ##################################################################
  # Scale Data
  ##################################################################
  df <- tibble(x = as.numeric(data))

  ##################################################################
  # Check User Input and Estimate Where Necessary
  ##################################################################
  # Store additional parameters
  user.params <- list(...)

  # If no user parameters are entered, estimate if possible
  if (length(user.params) == 0) {
    if (dist == "unif") {
      # not well supported
      x1 <- min(df$x, na.rm = TRUE)
      xn <- max(df$x, na.rm = TRUE)
      user.params <- list(
        min = x1 - (xn - x1) / (nrow(df) - 1), # UMVUEs
        max = xn + (xn - x1) / (nrow(df) - 1)
      )
      # message("Automatically estimated parameters for the uniform distribution: min=", round(user.params$min, 3), ", max=", round(user.params$max, 3),
      #         " using UMVUEs")
    } else if (dist == "binom") {
      # not well supported
      # Method of Moments estimation for Binomial (n, p)
      x.bar <- mean(df$x, na.rm = TRUE)
      s2 <- var(df$x, na.rm = TRUE)
      max.x <- max(df$x, na.rm = TRUE)

      # Check if MoM is mathematically valid (sample variance MUST be strictly less than sample mean)
      if (!is.na(s2) && s2 < x.bar && s2 > 0) {
        est.size <- round((x.bar^2) / (x.bar - s2))
        est.size <- max(est.size, max.x) # in case est.size < max.x
        est.prob <- x.bar / est.size
        estimation.method <- "Method of Moments"
      } else {
        # If variance is too high use max observation
        est.size <- max.x
        est.prob <- x.bar / est.size
        estimation.method <- "Max Observation Fallback"
      }
      user.params <- list(size = est.size, prob = est.prob)
      # message("Automatically estimated parameters for binomial distribution: size=", est.size,
      #         ", prob=", round(est.prob, 3), " (Used: ", estimation.method, ")")
    } else {
      fit.map <- c(
        "norm" = "normal",
        "lnorm" = "log-normal",
        "exp" = "exponential",
        "logis" = "logistic",
        "geom" = "geometric",
        "nbinom" = "negative binomial",
        "pois" = "Poisson"
      )
      fit.name <- if (dist %in% names(fit.map)) fit.map[[dist]] else dist

      tryCatch(
        {
          fit <- MASS::fitdistr(df$x, fit.name)
          user.params <- as.list(fit$estimate)
          # message("Automatically estimated parameters for ", fit.name, " distribution: ",
          #         paste(names(user.params), round(unlist(user.params), 3), sep = "=", collapse = ", "))
        },
        error = function(e) {
          warning(
            "MASS::fitdistr could not automatically estimate parameters for the '",
            fit.name,
            "'distribution. Please check your specification and ensure any necessary libraries are loaded. Error: ",
            e$message
          )
        }
      )
    }
  }

  # Create functions with proper parameters to use elsewhere
  qdist.w <- function(p) {
    do.call(qdist, c(list(p), user.params))
  }
  ddist.w <- function(x) {
    do.call(ddist, c(list(x), user.params))
  }

  ##################################################################
  # QQ line parameters
  ##################################################################
  y.quant <- quantile(df$x, c(0.25, 0.75), na.rm = T) # Empirical Q1, Q3
  x.quant <- qdist.w(c(0.25, 0.75))
  slope <- ifelse(diff(x.quant) == 0, 1, diff(y.quant) / diff(x.quant)) # QQ Slope
  intercept <- y.quant[1] - slope * x.quant[1] # QQ intercept

  ##################################################################
  # Orthogonal Projection Logic
  ##################################################################
  # For any point (x, y) on the line, the direction PERPENDICULAR to the line
  # is given by the vector (-m, 1). We normalize this to have length 1.
  len <- sqrt(slope^2 + 1)

  # Unit vector PARALLEL to the line (to map 'dx' width)
  vx <- 1 / len
  vy <- slope / len

  # Unit vector PERPENDICULAR to the line (to map 'dy' height)
  nx <- -slope / len
  ny <- 1 / len

  ##################################################################
  # Histogram parameters
  ##################################################################
  # Is the distribution discrete
  test_probs <- seq(0.01, 0.99, length.out = 100)
  test_quants <- suppressWarnings(qdist.w(test_probs))
  test_quants <- test_quants[is.finite(test_quants)] # Ignore Inf boundaries
  is.discrete <- length(test_quants) > 0 && all(test_quants %% 1 == 0)
  # Specify binwidth
  range <- max(df$x, na.rm = T) - min(df$x, na.rm = T)
  bin.width <- range / nclass.Sturges(df$x) # Default hist() bin width
  # Should bins be non-negative? bounded?
  is.nonnegative <- min(df$x, na.rm = TRUE) >= 0 # Check domain
  support.bounds <- qdist.w(c(0, 1))
  lower.bound <- support.bounds[1]
  upper.bound <- support.bounds[2]
  # Specify bins
  bin.min <- if (is.finite(lower.bound)) {
    lower.bound
  } else {
    min(df$x, na.rm = TRUE)
  }
  bin.max <- if (is.finite(upper.bound)) {
    upper.bound
  } else {
    max(df$x, na.rm = TRUE)
  }
  bin.min <- min(bin.min, min(df$x, na.rm = TRUE))
  bin.max <- max(bin.max, max(df$x, na.rm = TRUE))

  if (is.discrete) {
    bin.min <- floor(bin.min) - 0.5
    bin.max <- ceiling(bin.max) + 0.5
    breaks <- seq(bin.min, bin.max, by = 1)
    bin.width <- 1
  } else {
    num.bins <- nclass.Sturges(df$x)
    breaks <- seq(bin.min, bin.max, length.out = num.bins + 1)
    bin.width <- diff(breaks)[1]
  }

  # Specify bin height
  hist.data <- df |>
    mutate(
      bin.cut = cut(x, breaks = breaks, include.lowest = TRUE),
      bin.idx = as.numeric(bin.cut),
      bin.mid = bin.min + (bin.idx - 0.5) * bin.width
    ) |>
    group_by(bin.mid) |>
    summarise(count = n(), .groups = "drop")

  # NEW CHANGE: Calculate exact max proportions for discrete, else use KDE max
  if (is.discrete) {
    kde.max <- max(table(df$x) / nrow(df))
  } else {
    dens.fit <- density(df$x)
    kde.max <- max(dens.fit$y, na.rm = TRUE)
  }
  hist.max.dens <- max(hist.data$count) / (nrow(df) * bin.width)
  max.empirical.dens <- max(kde.max, hist.max.dens)
  target.max.h <- range * (height.scale)
  density.multiplier <- target.max.h / max.empirical.dens
  bar.scale <- density.multiplier / (nrow(df) * bin.width)
  dens.cap <- max.empirical.dens * 2.5
  #true.w <- sqrt((bin.width / slope)^2 + bin.width^2) / 2
  draw.width <- if (is.discrete) 0.5 else bin.width
  true.w <- sqrt((draw.width / slope)^2 + draw.width^2) / 2
  ##################################################################
  # Histogram data for a polygon (Translated to Rest on QQ)
  ##################################################################
  hist.polygons <- hist.data |>
    # Start computations defining bin geometry
    mutate(
      expected.count = ddist.w(bin.mid) * nrow(df) * bin.width,
      deviation = abs(count - expected.count),
      # midpoint along bottom of histogram bar (x.c, y.c)
      x.c = (bin.mid - intercept) / slope,
      y.c = bin.mid,
      # Specify bin height
      h = count * bar.scale
    ) |>
    slice(rep(1:n(), each = 4)) |>
    mutate(
      point_id = rep(1:4, times = nrow(hist.data)),
      # Specify the +/- from (x.c, y.c) to define the histogram bar
      dx = case_when(point_id %in% c(1, 4) ~ -true.w, TRUE ~ true.w),
      dy = case_when(point_id %in% c(1, 2) ~ 0, TRUE ~ h),
      # Tilt the histogram to align with the angle of the QQ plot
      final.x = x.c + dx * vx + dy * nx,
      final.y = y.c + dx * vy + dy * ny
    )

  ##################################################################
  # Corresponding Gaussian Distribution (Translated to Rest on QQ)
  ##################################################################
  # Specifying bins and range for plotting densities
  plot.min <- max(
    min(c(df$x, qdist.w(c(0.001))), na.rm = T) - bin.width,
    lower.bound,
    na.rm = TRUE
  )
  plot.max <- min(
    max(c(df$x, qdist.w(c(0.999))), na.rm = T) + bin.width,
    upper.bound,
    na.rm = TRUE
  )

  if (is.discrete) {
    plot.range <- seq(ceiling(plot.min), floor(plot.max), by = 1)
  } else {
    base.seq <- seq(plot.min, plot.max, length.out = 1000)
    if (
      is.finite(lower.bound) &&
        lower.bound >= plot.min &&
        lower.bound <= plot.max
    ) {
      base.seq <- c(
        base.seq,
        lower.bound - 1e-9,
        lower.bound,
        lower.bound + 1e-9
      )
    }
    if (
      is.finite(upper.bound) &&
        upper.bound >= plot.min &&
        upper.bound <= plot.max
    ) {
      base.seq <- c(
        base.seq,
        upper.bound - 1e-9,
        upper.bound,
        upper.bound + 1e-9
      )
    }
    plot.range <- sort(unique(base.seq))
  }

  # Get the data for plotting the theoretical distribution
  theoretical.density <- data.frame(val = plot.range) |>
    mutate(dens = ddist.w(val)) |>
    mutate(
      dens = ddist.w(val),
      dens = replace_na(dens, 0),
      dens = pmin(dens, dens.cap)
    ) |>
    mutate(
      h = dens * density.multiplier,
      x.base = (val - intercept) /
        slope -
        (if (is.discrete) (true.w / 2) * vx else 0),
      y.base = val - (if (is.discrete) (true.w / 2) * vy else 0),
      x = x.base + h * nx,
      y = y.base + h * ny
    )

  ##################################################################
  # Empirical Density Estimate (Translated to Rest on QQ)
  ##################################################################
  dens.fit <- density(df$x)
  # Get the data for plotting the kernal density
  if (is.discrete) {
    kde.curve <- as.data.frame(table(df$x)) |>
      mutate(val = as.numeric(as.character(Var1)), dens = Freq / nrow(df)) |>
      dplyr::select(val, dens) |>
      mutate(
        h = dens * density.multiplier,
        x.base = (val - intercept) / slope + (true.w / 2) * vx,
        y.base = val + (true.w / 2) * vy,
        x = x.base + h * nx,
        y = y.base + h * ny
      )
  } else {
    kde.curve <- data.frame(val = dens.fit$x, dens = dens.fit$y) |>
      mutate(
        h = dens * density.multiplier,
        x.base = (val - intercept) / slope,
        x = x.base + h * nx,
        y = val + h * ny
      )
  }

  ##################################################################
  # Get coordinates to maximize fixed coordinates (square)
  ##################################################################
  # Combine all X and Y coordinates from every layer to find the true bounding box
  theoreticalquantiles <- qdist.w(ppoints(nrow(df)))
  all.coords.x <- c(
    theoreticalquantiles,
    hist.polygons$final.x,
    theoretical.density$x,
    kde.curve$x
  )
  all.coords.y <- c(
    df$x,
    hist.polygons$final.y,
    theoretical.density$y,
    kde.curve$y
  )
  limits <- range(c(all.coords.x, all.coords.y), na.rm = TRUE)

  ##################################################################
  # Plot it!
  ##################################################################
  if (!color) {
    histoqq.plot <- ggplot(df) +
      # Histogram
      geom_polygon(
        data = hist.polygons,
        aes(x = final.x, y = final.y, group = bin.mid),
        color = "lightgrey",
        alpha = 0.5
      )
  } else {
    max.abs.dev <- max(abs(hist.polygons$deviation), na.rm = TRUE)
    color.bound <- max(ceiling(nrow(df) / 5), max.abs.dev)
    histoqq.plot <- ggplot(df) +
      # Histogram
      geom_polygon(
        data = hist.polygons,
        aes(x = final.x, y = final.y, group = bin.mid, fill = deviation),
        color = "lightgrey",
        alpha = 0.5,
        show.legend = FALSE
      ) +
      scale_fill_gradient(
        low = "grey30",
        high = "#b2182b",
        name = "Deviation\n(Obs - Exp)",
        limits = c(0, color.bound),
        oob = scales::squish
      )
  }
  if (is.discrete) {
    histoqq.plot <- histoqq.plot +
      # Theoretical Density
      geom_segment(
        data = theoretical.density,
        aes(
          x = x.base,
          y = y.base,
          xend = x,
          yend = y,
          linetype = "Theoretical"
        )
      ) +
      # Empirical Density
      geom_segment(
        data = kde.curve,
        aes(x = x.base, y = y.base, xend = x, yend = y, linetype = "Empirical")
      )
  } else {
    histoqq.plot <- histoqq.plot +
      # Theoretical Density
      geom_path(
        data = theoretical.density,
        aes(x = x, y = y, linetype = "Theoretical")
        #linewidth = 1
      ) +
      # Empirical Density
      geom_path(
        data = kde.curve,
        aes(x = x, y = y, linetype = "Empirical") #,
      )
  }
  histoqq.plot <- histoqq.plot +
    # Q-Q Plot
    stat_qq_line(
      aes(sample = x),
      distribution = qdist,
      dparams = user.params,
      color = "red",
      linewidth = 1,
      alpha = 0.75
    ) +
    stat_qq(
      aes(sample = x),
      distribution = qdist,
      dparams = user.params,
      alpha = 0.75
    ) +
    # Themes and Labels
    theme_bw() +
    labs(x = "Theoretical Quantiles", y = "Observed Quantiles") +
    scale_linetype_manual("", values = c("twodash", "solid"))

  if (square) {
    histoqq.plot <- histoqq.plot +
      coord_cartesian(xlim = limits, ylim = limits, expand = TRUE)
  } else {
    histoqq.plot <- histoqq.plot +
      coord_cartesian()
  }
  (histoqq.plot)
}
