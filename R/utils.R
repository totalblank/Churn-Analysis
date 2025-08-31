# utility functions

#' Output-aware font theme for ggplot (PDF and HTML)
#' Usage: ggplot(...) + latex_font_theme(base_size = 14, family = "Symbola")
font_theme <- function(
  # PDF / LaTeX settings
  base_size   = 14,
  family      = "Symbola",
  face        = "plain",
  title_mult  = 1.2,
  axis_mult   = 1.0,
  legend_mult = 0.7,
  
  # HTML settings (separate so screens can be larger)
  html_base_size   = 18,
  html_family      = "Inter",      # e.g. "Inter" (NULL = ggplot default)
  html_face        = "plain",
  html_title_mult  = 1.25,
  html_axis_mult   = 1.0,
  html_legend_mult = 0.85,
  
  # Control detection / verbosity
  mode    = c("auto", "latex", "html", "none"),  # force behavior if needed
  verbose = FALSE
) {
  mode <- match.arg(mode)
  
  # Detect output if auto
  is_pdf  <- isFALSE(mode != "auto") && tryCatch(knitr::is_latex_output(), error = function(...) FALSE)
  is_html <- isFALSE(mode != "auto") && tryCatch(knitr::is_html_output(),  error = function(...) FALSE)
  
  if (mode == "latex") { is_pdf <- TRUE;  is_html <- FALSE }
  if (mode == "html")  { is_pdf <- FALSE; is_html <- TRUE  }
  if (mode == "none")  { is_pdf <- FALSE; is_html <- FALSE }
  
  if (is_pdf) {
    if (verbose) message("latex_font_theme: applying PDF/LaTeX theme")
    return(ggplot2::theme(
      text         = ggplot2::element_text(size = base_size, family = family, face = face),
      plot.title   = ggplot2::element_text(size = base_size * title_mult, face = "bold"),
      axis.title   = ggplot2::element_text(size = base_size * axis_mult,  face = face),
      axis.text    = ggplot2::element_text(size = base_size * (axis_mult * 0.9), face = face),
      legend.title = ggplot2::element_text(size = base_size * legend_mult,      face = face),
      legend.text  = ggplot2::element_text(size = base_size * (legend_mult * 0.9), face = face)
    ))
  }
  
  if (is_html) {
    if (verbose) message("latex_font_theme: applying HTML theme")
    return(ggplot2::theme(
      text         = ggplot2::element_text(size = html_base_size, family = html_family, face = html_face),
      plot.title   = ggplot2::element_text(size = html_base_size * html_title_mult, face = "plain"),
      axis.title   = ggplot2::element_text(size = html_base_size * html_axis_mult,  face = html_face),
      axis.text    = ggplot2::element_text(size = html_base_size * (html_axis_mult * 0.9), face = html_face),
      legend.title = ggplot2::element_text(size = html_base_size * html_legend_mult,      face = html_face),
      legend.text  = ggplot2::element_text(size = html_base_size * (html_legend_mult * 0.9), face = html_face)
    ))
  }
  
  if (verbose) message("latex_font_theme: no special output detected -> returning empty theme()")
  ggplot2::theme()  # no-op (e.g., when not knitting)
}

summarize_df <- function(df) {
  df %>%
    select(where(is.numeric)) %>%
    map_df(~ {
      vals <- na.omit(.x)
      tibble(
        mean   = mean(vals),
        median = median(vals),
        sd     = sd(vals),
        min    = min(vals),
        Q1     = quantile(vals, 0.25),
        Q2     = quantile(vals, 0.50),
        Q3     = quantile(vals, 0.75),
        Q4     = quantile(vals, 1.00),
        max    = max(vals)
      )
    }, .id = "variable")
}

print_table <- function(
    df,
    caption = NULL,
    digits = NULL,           # NULL, single number, or vector
    align = NULL,            # if NULL: numeric -> 'r', else 'l'
    small = TRUE,
    longtable = TRUE,
    scale_down = TRUE,
    landscape = FALSE,
    escape = TRUE,
    col_names = NULL,        # default: use names(df) then format via name_style
    rownames = NA,
    italic_header = FALSE,   # italicize headers
    name_style = c("none", "title", "pretty") # auto-format headers
) {
  is_pdf  <- knitr::is_latex_output()
  is_html <- knitr::is_html_output()
  name_style <- match.arg(name_style)
  
  # ----- Guard: empty df -----
  if (ncol(df) == 0) {
    return(knitr::asis_output(if (is_html) "<em>(no columns)</em>" else "\\emph{(no columns)}"))
  }
  
  # ----- Column names -----
  if (is.null(col_names)) col_names <- names(df)
  
  # Apply name_style
  if (name_style != "none") {
    col_names <- switch(
      name_style,
      title = tools::toTitleCase(col_names),
      pretty = tools::toTitleCase(gsub("[_.]", " ", col_names)),
      col_names
    )
  }
  
  if (length(col_names) != ncol(df)) {
    stop("`col_names` must have length equal to number of columns (", ncol(df), ").")
  }
  
  # ----- Digits vector -----
  make_digits <- function(df, digits) {
    if (is.null(digits)) {
      vapply(df, function(x) if (is.numeric(x)) 2L else NA_integer_, integer(1))
    } else if (length(digits) == 1L) {
      rep(as.integer(digits), ncol(df))
    } else {
      if (length(digits) != ncol(df))
        stop("`digits` length must equal number of columns (", ncol(df), ").")
      as.integer(digits)
    }
  }
  digits_vec <- make_digits(df, digits)
  
  # ----- Alignment -----
  if (is.null(align)) {
    align <- vapply(df, function(x) if (is.numeric(x)) "r" else "l", character(1))
  } else if (length(align) == 1L) {
    align <- rep(align, ncol(df))
  } else if (length(align) != ncol(df)) {
    stop("`align` length must equal number of columns (", ncol(df), ").")
  }
  
  # ----- Build kable -----
  kb <- knitr::kable(
    df,
    format     = if (is_pdf) "latex" else "html",
    caption    = caption,
    digits     = digits_vec,
    align      = align,
    escape     = escape,
    col.names  = col_names,
    row.names  = rownames,
    booktabs   = is_pdf,
    longtable  = is_pdf && longtable
  )
  
  # Italicize header if requested
  if (italic_header) {
    kb <- kableExtra::row_spec(kb, 0, italic = TRUE)
  }
  
  # ----- HTML path -----
  if (is_html) {
    out <- kb |>
      kableExtra::kable_styling(
        full_width = FALSE,
        bootstrap_options = c("striped", "hover")
      )
    # use kableExtra's scroll_box instead of manual div (adds proper html deps)
    if (ncol(df) > 8) {
      out <- kableExtra::scroll_box(out, width = "100%", height = "auto")
    }
    return(htmltools::browsable(out))  # <- key: ensures knitr treats it as HTML
  }
  
  # ----- PDF path -----
  latex_opts <- c("hold_position")
  if (longtable) latex_opts <- c(latex_opts, "repeat_header")
  if (scale_down && ncol(df) > 8) latex_opts <- c(latex_opts, "scale_down")
  
  out <- kb |>
    kableExtra::kable_styling(
      latex_options = latex_opts,
      full_width = FALSE,
      font_size = if (small) if (ncol(df) > 8) 8 else 9 else 10
    )
  
  if (landscape || ncol(df) > 12) {
    out <- kableExtra::landscape(out)
  }
  
  out
}

make_skew_plot <- function(df, feature) {
  v <- df[[feature]]
  d <- data.frame(value = v)

  m   <- mean(v, na.rm = TRUE)
  med <- median(v, na.rm = TRUE)
  sdv <- sd(v, na.rm = TRUE)

  skew <- if (is.na(sdv) || sdv == 0) NA_real_ else 3 * (m - med) / sdv
  dir  <- if (m > med) "Right-skewed" else if (m < med) "Left-skewed" else "Symmetric"
  label <- if (is.na(skew)) dir else sprintf("%s\nSkew ≈ %.2f", dir, skew)

  lines <- data.frame(
    stat_type  = c("Mean", "Median"),
    xintercept = c(m, med)
  )

  ggplot(d, aes(value)) +
    geom_density(fill = "#4C78A8", alpha = 0.3) +
    geom_vline(
      data = lines,
      aes(xintercept = xintercept, color = stat_type, linetype = stat_type),
      linewidth = 0.9
    ) +
    annotate("text", x = Inf, y = Inf, label = label, hjust = 1.05, vjust = 1.3) +
    labs(title = feature, x = NULL, y = NULL,
         color = "Statistic", linetype = "Statistic") +
    scale_color_manual(values = c(Mean = "red", Median = "blue")) +
    scale_linetype_manual(values = c(Mean = "dashed", Median = "dotted")) +
    theme(legend.position = "bottom", plot.margin = margin(10, 20, 10, 10))
}

# Wilson CI (no name clashes with outer columns)
prop_ci_wilson <- function(success, n, conf.level = 0.95) {
  z <- qnorm(1 - (1 - conf.level)/2)
  phat <- success / n
  denom <- 1 + z^2/n
  centre <- phat + z^2/(2*n)
  half <- z * sqrt(phat*(1 - phat)/n + z^2/(4*n^2))
  tibble(
    p    = phat,
    p_lo = pmax(0, (centre - half)/denom),
    p_hi = pmin(1, (centre + half)/denom)
  )
}