#' Update Diviana
#'
#' This function installs the latest development version of Diviana, together with some or
#' all supporting pedsuite packages.
#'
#' The proposed changes are shown before asking for confirmation. If needed, run
#' `install.packages("pak")` or `install.packages("remotes")` first.
#'
#' @param pkgs Character vector of supporting packages to update. By default, all pedsuite
#'   packages used by Diviana.
#' @param method Installation method: `"pak"` (default) or `"remotes"`.
#'
#' @return Invisibly `NULL`.
#'
#' @export
updateDiviana = function(pkgs = NULL, method = c("pak", "remotes")) {
  method = match.arg(method)
  if(!interactive())
    stop("Run this function in the R console to confirm the update.", call. = FALSE)
  for(pkg in c(method, "jsonlite")) {
    if(!requireNamespace(pkg, quietly = TRUE))
      stop("First run install.packages('", pkg, "'), then try again.", call. = FALSE)
  }

  deps =  if(!is.null(pkgs)) pkgs else
    c("pedtools", "pedmut", "pedprobr", "pedFamilias", "ribd", "forrel",
      "norSTR", "verbalisr", "dvir")

  repos = "https://cloud.r-project.org"
  cat("Checking for Diviana updates...\n")
  av = utils::available.packages(repos = repos, type = "source")

  plan = lapply(deps, function(pkg) {
    ver = if(pkg %in% rownames(av)) av[pkg, "Version"] else NA_character_
    info = suppressWarnings(tryCatch(utils::packageDescription(pkg),
                                      error = function(e) NULL))
    old = if(is.list(info)) info$Version else NA_character_
    sha = if(is.list(info)) info$RemoteSha else NULL

    # Compare GitHub changes with the latest released version
    url = sprintf("https://api.github.com/repos/magnusdv/%s/compare/v%s...master", pkg, ver)
    cmp = if(is.na(ver)) NULL else
      tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
    checked = !is.null(cmp$ahead_by)
    dev = !checked || cmp$ahead_by > 0L

    # If no matching release tag, check the latest GitHub version directly
    latestSha = if(checked) cmp$head_commit$sha else {
      url = sprintf("https://api.github.com/repos/magnusdv/%s/commits/master", pkg)
      tryCatch(jsonlite::fromJSON(url)$sha, error = function(e) NULL)
    }
    if(is.null(latestSha))
      stop("Could not check updates for ", pkg, ".", call. = FALSE)

    # Skip packages already at the selected version
    update = if(dev) !identical(sha, latestSha) else
      is.na(old) || utils::compareVersion(old, ver) < 0L
    list(pkg = pkg, old = old, version = ver, dev = dev, update = update,
         checked = checked)
    })

  cat("\nProposed changes:\n")
  for(p in Filter(\(p) p$update, plan)) {
    installed = if(is.na(p$old)) "not installed" else paste("installed", p$old)
    status = if(!p$checked)
      "get latest version (release comparison unavailable)" else if(p$dev)
      "newer version available" else
      paste("update to released version", p$version)
    cat(sprintf("  %-13s %s; %s\n", p$pkg, installed, status))
  }
  cat("  diviana       install newest version\n")

  cat("\nIt is strongly recommended to restart R before updating packages.\n",
      "If needed, abort the update and run this function again after restarting R.\n",
      sep = "")

  answer = tolower(trimws(readline("Continue with the update? [y/N]: ")))
  if(!answer %in% c("y", "yes")) {
    cat("Update cancelled. No packages were changed.\n")
    return(invisible(NULL))
  }

  selected = Filter(function(p) p$update, plan)
  if(length(selected)) {
    cran = vapply(Filter(function(p) !p$dev, selected), `[[`, "", "pkg")
    gh = vapply(Filter(function(p) p$dev, selected), `[[`, "", "pkg")
    if(method == "pak") {
      pkgs = c(cran, paste0("magnusdv/", gh, "?reinstall"))
      pak::pkg_install(pkgs, upgrade = TRUE, ask = FALSE)
    }
    else {
      if(length(cran))
        remotes::install_cran(cran, repos = repos, upgrade = "always")
      if(length(gh))
        remotes::install_github(paste0("magnusdv/", gh), upgrade = "always", force = TRUE)
    }
  }

  # Install Diviana last without changing the selected supporting packages
  if(method == "pak")
    pak::pkg_install("magnusdv/diviana?reinstall", dependencies = NA,
                     upgrade = TRUE, ask = FALSE)
  else
    remotes::install_github("magnusdv/diviana", dependencies = NA,
                            upgrade = "always", force = TRUE)

  message("Update finished. Restart R again before starting Diviana.")
  invisible(NULL)
}
