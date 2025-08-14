# R/utils_schema.R
# -------------------------------------------------------------------
# Robust schema lock + check utilities for tabular datasets
# - Normalizes column names before comparison (encoding/spacing/case)
# - Optional allow_reorder to ignore column order differences
# - Records useful stats (n_missing, n_unique, ranges, sample_levels)
# - Includes file content hash for drift visibility
# -------------------------------------------------------------------

# Null coalescing helper
`%||%` <- function(a, b) if (is.null(a)) b else a

# Normalize column names to a canonical form before comparison
.norm_names <- function(x) {
  # ensure UTF-8
  x <- enc2utf8(x)
  # replace non-breaking space with regular space
  x <- gsub("\u00A0", " ", x, perl = TRUE)
  # trim whitespace
  x <- trimws(x)
  # lower-case
  x <- tolower(x)
  # collapse any non [a-z0-9_] to underscore
  x <- gsub("[^a-z0-9_]+", "_", x, perl = TRUE)
  # collapse duplicate underscores
  x <- gsub("_+", "_", x, perl = TRUE)
  # strip leading/trailing underscores
  x <- gsub("^_+|_+$", "", x, perl = TRUE)
  x
}

# Compute a stable file fingerprint (data changes -> new hash)
.file_hash <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (!requireNamespace("digest", quietly = TRUE)) utils::install.packages("digest")
  digest::digest(file = path, algo = "xxhash64")
}

# Infer a rich column schema from a data.frame
infer_schema <- function(df, max_levels = 50) {
  stopifnot(is.data.frame(df))
  cols <- names(df)
  classes <- vapply(df, function(x) class(x)[1], character(1))
  ex_vals <- vapply(df, function(x) {
    ex <- x[!is.na(x)][1]
    if (length(ex)) as.character(ex) else NA_character_
  }, character(1))
  n_missing  <- vapply(df, function(x) sum(is.na(x)), integer(1))
  n_unique   <- vapply(df, function(x) length(unique(stats::na.omit(x))), integer(1))

  levels_cap <- lapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      lv <- unique(stats::na.omit(as.character(x)))
      utils::head(lv, max_levels)
    } else NULL
  })
  rng <- lapply(df, function(x) {
    if (is.numeric(x)) {
      c(min = suppressWarnings(min(x, na.rm = TRUE)),
        max = suppressWarnings(max(x, na.rm = TRUE)))
    } else NULL
  })

  Map(function(col, cls, ex, miss, unq, lv, rg) {
    lst <- list(
      column = col,
      type   = cls,
      example_value = ex,
      n_missing = miss,
      n_unique  = unq
    )
    if (!is.null(lv)) lst$sample_levels <- lv
    if (!is.null(rg)) lst$range <- rg
    lst
  }, cols, classes, ex_vals, n_missing, n_unique, levels_cap, rng)
}

# Lock (write) a schema.json from a data file
lock_schema <- function(data_path,
                        schema_path = "data/schema.json",
                        read_fun = NULL,
                        metadata = list(source = "raw")) {
  if (is.null(read_fun)) {
    if (!requireNamespace("readr", quietly = TRUE)) utils::install.packages("readr")
    read_fun <- function(p) readr::read_csv(p, show_col_types = FALSE)
  }
  stopifnot(file.exists(data_path))
  df <- read_fun(data_path)

  schema <- list(
    version = 2,
    generated_at = as.character(Sys.time()),
    data_path = data_path,
    file_hash = .file_hash(data_path),
    n_rows = nrow(df),
    n_cols = ncol(df),
    # Preserve original names in the lock, but also store normalized forms
    columns = Map(function(col, info) {
      info$column_original   <- col
      info$column_normalized <- .norm_names(col)
      info
    }, names(df), infer_schema(df)),
    metadata = metadata
  )

  if (!requireNamespace("jsonlite", quietly = TRUE)) utils::install.packages("jsonlite")
  jsonlite::write_json(schema, schema_path, pretty = TRUE, auto_unbox = TRUE)
  message("✅ Schema locked to: ", schema_path)
  invisible(schema)
}

# Read a previously locked schema
read_schema <- function(schema_path = "data/schema.json") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) utils::install.packages("jsonlite")
  jsonlite::read_json(schema_path, simplifyVector = FALSE)
}

# Compare current data with locked schema
check_schema <- function(data_path,
                         schema_path = "data/schema.json",
                         read_fun = NULL,
                         strict_names = TRUE,
                         strict_types = TRUE,
                         allow_reorder = FALSE,
                         warn_on_levels = TRUE,
                         verbose = TRUE) {
  if (is.null(read_fun)) {
    if (!requireNamespace("readr", quietly = TRUE)) utils::install.packages("readr")
    read_fun <- function(p) readr::read_csv(p, show_col_types = FALSE)
  }
  stopifnot(file.exists(schema_path), file.exists(data_path))

  current <- read_fun(data_path)
  sch <- read_schema(schema_path)

  # Normalize both sides
  cur_names_raw <- names(current)
  cur_names     <- .norm_names(cur_names_raw)

  # Support legacy schema v1 (without column_normalized)
  locked_names_raw <- vapply(sch$columns, function(x) x$column_original %||% x$column, "")
  locked_names_norm <- vapply(sch$columns, function(x) {
    x$column_normalized %||% .norm_names(x$column %||% x$column_original)
  }, "")

  # ---- Name checks ----
  if (strict_names) {
    same_order <- identical(cur_names, locked_names_norm)
    same_set   <- setequal(cur_names, locked_names_norm)
    if (!(same_order || (allow_reorder && same_set))) {
      stop(
        "❌ Column names changed.\n",
        "Current (normalized): ", paste(cur_names, collapse = ", "),
        "\nLocked  (normalized): ", paste(locked_names_norm, collapse = ", "),
        "\n\nCurrent (raw): ", paste(cur_names_raw, collapse = ", "),
        "\nLocked  (raw): ", paste(locked_names_raw, collapse = ", ")
      )
    }
  }

  # ---- Type checks ----
  if (strict_types) {
    cur_types <- vapply(current, function(x) class(x)[1], character(1))

    # Reorder current types to match locked column order if allowed
    if (allow_reorder) {
      # build index mapping by normalized names
      idx <- match(locked_names_norm, cur_names)
      if (anyNA(idx)) {
        stop("❌ Could not align columns by normalized names during type check.")
      }
      cur_types <- cur_types[idx]
    }

    locked_types <- vapply(sch$columns, `[[`, "", "type")

    if (!identical(cur_types, locked_types)) {
      stop(
        "❌ Column types changed.\n",
        "Current: ", paste(cur_types, collapse = ", "),
        "\nLocked:  ", paste(locked_types, collapse = ", ")
      )
    }
  }

  # ---- Optional: warn for category explosion ----
  if (warn_on_levels) {
    cur_uniques <- vapply(current, function(x) length(unique(stats::na.omit(x))), integer(1))
    locked_uniques <- vapply(sch$columns, function(x) x$n_unique %||% NA_integer_, integer(1))
    names(locked_uniques) <- locked_names_norm

    # align to locked name order
    if (!setequal(names(cur_uniques), names(locked_uniques))) {
      # align by normalized names where possible
      names(cur_uniques) <- cur_names
      cur_uniques <- cur_uniques[locked_names_norm]
    }

    spike <- names(cur_uniques)[is.finite(locked_uniques) &
                                  (cur_uniques > pmax(locked_uniques * 5L, locked_uniques + 50L))]
    if (length(spike)) {
      warning("⚠️  Potential category explosion for: ", paste(spike, collapse = ", "))
    }
  }

  # Informational: file hash drift
  cur_hash <- .file_hash(data_path)
  if (!is.null(sch$file_hash) && !is.na(sch$file_hash) && !identical(cur_hash, sch$file_hash) && verbose) {
    message("ℹ️  File content hash differs from locked schema (data likely updated).")
  }

  if (verbose) message("✅ Dataset matches locked schema.")
  invisible(TRUE)
}
