custom_theme <- function(
    family = "Symbola",
    path   = "/usr/share/fonts/TTF/Symbola.ttf",
    base_size = 12
) {
  if (!requireNamespace("showtext", quietly = TRUE))
    stop("Please install.packages('showtext')")
  sysfonts::font_add(family = family, regular = path)
  showtext::showtext_auto()
  ggplot2::theme_minimal(base_size = base_size, base_family = family)
}

use_custom_theme <- function(
    family = "Symbola",
    path   = "/usr/share/fonts/TTF/Symbola.ttf",
    base_size = 12
) {
  # Load namespaces lazily
  if (!requireNamespace("showtext", quietly = TRUE))
    stop("Please install.packages('showtext')")
  
  # Register font & enable showtext
  sysfonts::font_add(family = family, regular = path)
  showtext::showtext_auto()
  
  # Global theme with base_family set
  ggplot2::theme_set(
    ggplot2::theme_minimal(base_size = base_size, base_family = family)
  )
  
  # Make text geoms default to this family (safe if some packages not installed)
  safe_update <- function(geom) {
    if (geom %in% names(getNamespaceExports("ggplot2"))) {
      ggplot2::update_geom_defaults(geom, list(family = family))
    }
  }
  purrr::walk(c("text", "label"), safe_update)
  
  # If using ggrepel, also set these (won't error if ggrepel’s not installed)
  if (requireNamespace("ggrepel", quietly = TRUE)) {
    ggplot2::update_geom_defaults("label_repel", list(family = family))
    ggplot2::update_geom_defaults("text_repel",  list(family = family))
  }
  
  invisible(TRUE)
}

