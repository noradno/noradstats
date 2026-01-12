# Internal helpers for SharePoint API access (not exported)
#
# This file contains the SharePoint-specific logic used by the
# SharePoint results pipeline (create_sharepoint_results_to_db()).
#
# Important distinction:
# - This code accesses SharePoint Lists via the Microsoft Graph API
#   (i.e. online, authenticated access).
# - It is separate from local filesystem access (e.g. get_duckdb_path()).
#
# Why a cache?
# - Several make_*() functions call read_sp_list() during one pipeline run.
# - Microsoft365R::get_sharepoint_site() can trigger repeated auth/log messages
#   and setup work if called many times.
# - We therefore cache the SharePoint "site" object for the duration of the R session.
#
# Why an environment?
# - Package namespaces are locked after load; top-level variables cannot be
#   reassigned with <<- inside an installed package.
# - Environments are mutable, so cached values can be safely updated.

.sp_cache <- new.env(parent = emptyenv())

get_sp_site_cached <- function(site_url) {
  stopifnot(is.character(site_url), length(site_url) == 1, nzchar(site_url))
  
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop(
      "Package 'Microsoft365R' is required to read SharePoint lists.\n",
      "Install it to run create_sharepoint_results_to_db().",
      call. = FALSE
    )
  }
  
  # Read cached values (if present)
  cached_url <- if (exists("site_url", envir = .sp_cache, inherits = FALSE)) {
    get("site_url", envir = .sp_cache, inherits = FALSE)
  } else {
    NULL
  }
  
  has_site <- exists("site", envir = .sp_cache, inherits = FALSE)
  
  # Create or refresh cache if empty or if the site URL changed
  if (is.null(cached_url) || !identical(cached_url, site_url) || !has_site) {
    site <- Microsoft365R::get_sharepoint_site(site_url = site_url)
    assign("site_url", site_url, envir = .sp_cache)
    assign("site", site, envir = .sp_cache)
  }
  
  get("site", envir = .sp_cache, inherits = FALSE)
}

# Reset the cached SharePoint site object
#
# Internal helper, mainly useful during development or debugging
# if authentication state becomes inconsistent.
reset_sharepoint_cache <- function() {
  rm(list = ls(envir = .sp_cache, all.names = TRUE), envir = .sp_cache)
  invisible(TRUE)
}

# Read a SharePoint List via the Microsoft Graph API
#
# This function is used internally by make_*() functions in the
# SharePoint results pipeline.
read_sp_list <- function(
    listname,
    site_url = "https://noradno.sharepoint.com/sites/Norad-Avd-Kunnskap"
) {
  stopifnot(is.character(listname), length(listname) == 1, nzchar(listname))
  
  if (!requireNamespace("janitor", quietly = TRUE)) {
    stop("Package 'janitor' is required.", call. = FALSE)
  }
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required.", call. = FALSE)
  }
  
  sp  <- get_sp_site_cached(site_url)
  lst <- sp$get_list(listname)
  
  lst$list_items() |>
    janitor::clean_names() |>
    tibble::as_tibble()
}
