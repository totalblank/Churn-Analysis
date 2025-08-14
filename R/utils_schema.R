infer_schema <- function(df, max_levels = 50) {
  stopifnot(is.data.frame(df))
  cols <- names(df)
  classes <- vapply(df, function(x) class(x)[1], character(1))
  ex_vals <- vapply(df, function(x) {
    ex <- x[!is.na(x)][1]
    if (length(ex)) as.character(ex) else NA_character_
  }, character(1))
  
  # Simple stats that help catch accidental type/shape changes
  n_missing  <- vapply(df, function(x) sum(is.na(x)), integer(1))
  n_unique   <- vapply(df, function(x) length(unique(stats::na.omit(x))), integer(1))
  levels_cap <- lapply(df, function(x) {
    if (is.factor(x) || is.character(x)) {
      lv <- unique(stats::na.omit(x))
      lv <- as.character(utils::head(lv, max_levels))
      return(lv)
    }
    NULL
  })
  
  # Numeric ranges (quick sanity)
  rng <- lapply(df, function(x) {
    if (is.numeric(x)) c(min = suppressWarnings(min(x, na.rm = TRUE)),
                         max = suppressWarnings(max(x, na.rm = TRUE))) else NULL
  })
  
  # Build list of columns
  cols_list <- Map(function(col, cls, ex, miss, unq, lv, rg) {
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
  
  cols_list
}

# Compute a stable file fingerprint (data changes -> new hash)
.file_hash <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  if (!requireNamespace("digest", quietly = TRUE)) install.packages("digest")
  digest::digest(file = path, algo = "xxhash64")
}

lock_schema <- function(data_path, schema_path = "data/schema.json",
                        read_fun = NULL, metadata = list(source = "raw")) {
  if (is.null(read_fun)) {
    read_fun <- function(p) readr::read_csv(p, show_col_types = FALSE)
  }
  stopifnot(file.exists(data_path))
  df <- read_fun(data_path)
  
  schema <- list(
    version = 1,
    generated_at = as.character(Sys.time()),
    data_path = data_path,
    file_hash = .file_hash(data_path),
    n_rows = nrow(df),
    n_cols = ncol(df),
    columns = infer_schema(df),
    metadata = metadata
  )
  
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
  jsonlite::write_json(schema, schema_path, pretty = TRUE, auto_unbox = TRUE)
  message("✅ Schema locked to: ", schema_path)
  invisible(schema)
}

read_schema <- function(schema_path = "data/schema.json") {
  if (!requireNamespace("jsonlite", quietly = TRUE)) install.packages("jsonlite")
  jsonlite::read_json(schema_path, simplifyVector = FALSE)
}

check_schema <- function(data_path, schema_path = "data/schema.json",
                         read_fun = NULL,
                         strict_names = TRUE,
                         strict_types = TRUE,
                         allow_reorder = FALSE,
                         warn_on_levels = TRUE) {
  if (is.null(read_fun)) {
    read_fun <- function(p) readr::read_csv(p, show_col_types = FALSE)
  }
  stopifnot(file.exists(schema_path), file.exists(data_path))
  current <- read_fun(data_path)
  sch <- read_schema(schema_path)
  
  # Names
  cur_names <- names(current)
  locked_names <- vapply(sch$columns, `[[`, "", "column")
  if (strict_names) {
    same_order <- identical(cur_names, locked_names)
    same_set   <- setequal(cur_names, locked_names)
    if (!(same_order || (allow_reorder && same_set))) {
      stop("❌ Column names changed. Current: ",
           paste(cur_names, collapse = ", "),
           " | Locked: ",
           paste(locked_names, collapse = ", "))
    }
  }
  
  # Types
  if (strict_types) {
    cur_types <- vapply(current, function(x) class(x)[1], character(1))
    locked_types <- vapply(sch$columns, `[[`, "", "type")
    # Align order for comparison if reordering allowed
    if (allow_reorder) {
      locked_map <- setNames(locked_types, locked_names)
      cur_types <- cur_types[locked_names]
    }
    if (!identical(cur_types, locked_types)) {
      stop("❌ Column types changed.\nCurrent: ",
           paste(cur_types, collapse = ", "),
           "\nLocked:  ",
           paste(locked_types, collapse = ", "))
    }
  }
  
  # Optional: warn if categorical level explosion
  if (warn_on_levels) {
    # find factors/characters with a lot more unique values than before
    cur_uniques <- vapply(current, function(x) length(unique(stats::na.omit(x))), integer(1))
    locked_uniques <- vapply(sch$columns, function(x) x$n_unique %||% NA_integer_, integer(1))
    names(locked_uniques) <- vapply(sch$columns, `[[`, "", "column")
    spike <- names(cur_uniques)[cur_uniques > (locked_uniques[names(cur_uniques)] * 5)]
    if (length(spike)) {
      warning("⚠️  Potential category explosion for: ", paste(spike, collapse = ", "))
    }
  }
  
  # File hash drift (informational)
  cur_hash <- .file_hash(data_path)
  if (!is.null(sch$file_hash) && !is.na(sch$file_hash) && !identical(cur_hash, sch$file_hash)) {
    message("ℹ️  File content hash differs from locked schema (expected updates or new data).")
  }
  
  message("✅ Dataset matches locked schema.")
  invisible(TRUE)
}

# Helper for NULL coalescing in base R
`%||%` <- function(a, b) if (is.null(a)) b else a
