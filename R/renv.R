restore <- function(project  = NULL,
                    ...,
                    library  = NULL,
                    lockfile = NULL,
                    packages = NULL,
                    rebuild  = FALSE,
                    repos    = NULL,
                    clean    = FALSE,
                    prompt   = interactive())
{
  
  renv_consent_check()
  renv_scope_error_handler()
  renv_dots_check(...)

  project  <- renv_project_resolve(project)
  renv_scope_lock(project = project)

  # resolve library, lockfile arguments
  libpaths <- renv_libpaths_resolve(library)
  lockfile <- lockfile %||% renv_lockfile_load(project = project)
  lockfile <- renv_lockfile_resolve(lockfile)

  # activate the requested library (place at front of library paths)
  library <- nth(libpaths, 1L)
  ensure_directory(library)
  renv_scope_libpaths(libpaths)

  # perform Python actions on exit
  on.exit(renv_python_restore(project), add = TRUE)

  # resolve the lockfile
  if (is.character(lockfile))
    lockfile <- renv_lockfile_read(lockfile)

  # inject overrides (if any)
  lockfile <- renv_lockfile_override(lockfile)

  # repair potential issues in the lockfile
  lockfile <- renv_lockfile_repair(lockfile)

  # override repositories if requested
  repos <- repos %||% config$repos.override() %||% lockfile$R$Repositories

  if (length(repos))
    renv_scope_options(repos = convert(repos, "character"))

  # set up Bioconductor repositories
  biocversion <- lockfile$Bioconductor$Version
  if (!is.null(biocversion)) {
    biocversion <- package_version(biocversion)
    renv_scope_options(renv.bioconductor.version = biocversion)
  }

  # get records for R packages currently installed
  current <- snapshot(project  = project,
                      library  = libpaths,
                      lockfile = NULL,
                      type     = "all")

  # compare lockfile vs. currently-installed packages
  diff <- renv_lockfile_diff_packages(current, lockfile)

  # don't remove packages unless 'clean = TRUE'
  diff <- renv_vector_diff(diff, if (!clean) "remove")

  # only remove packages from the project library
  difflocs <- map_chr(names(diff), function(package) {
    find.package(package, lib.loc = libpaths, quiet = TRUE) %||% ""
  })

  exclude <- diff == "remove" & dirname(difflocs) != library
  diff <- diff[!exclude]

  # don't take any actions with ignored packages
  ignored <- renv_project_ignored_packages(project = project)
  diff <- diff[renv_vector_diff(names(diff), ignored)]

  # only take action with requested packages
  diff <- diff[intersect(names(diff), packages %||% names(diff))]

  if (!length(diff)) {
    name <- if (!missing(library)) "library" else "project"
    vwritef("* The %s is already synchronized with the lockfile.", name)
    return(invisible(diff))
  }

  if (!renv_restore_preflight(project, libpaths, diff, current, lockfile, prompt)) {
    message("* Operation aborted.")
    return(FALSE)
  }

  if (prompt || renv_verbose())
    renv_restore_report_actions(diff, current, lockfile)

  if (prompt && !proceed()) {
    message("* Operation aborted.")
    return(invisible(diff))
  }
  
  # perform the restore
  records <- renv_restore_run_actions(project, diff, current, lockfile, rebuild)
  invisible(records)
}

renv_restore_run_actions <- function(project, actions, current, lockfile, rebuild) {
  
  packages <- names(actions)

  renv_scope_restore(
    project  = project,
    library  = renv_libpaths_default(),
    records  = renv_records(lockfile),
    packages = packages,
    rebuild  = rebuild
  )

  # first, handle package removals
  removes <- actions[actions == "remove"]
  enumerate(removes, function(package, action) {
    renv_restore_remove(project, package, current)
  })

  # next, handle installs
  installs <- actions[actions != "remove"]
  packages <- names(installs)
  
  # perform the install
  records <- renv_retrieve(packages)
  status <- renv_install(records)
  
  # detect dependency tree repair
  diff <- renv_lockfile_diff_packages(renv_records(lockfile), records)
  diff <- diff[diff != "remove"]
  if (!empty(diff)) {
    renv_pretty_print_records(
      records[names(diff)],
      "The dependency tree was repaired during package installation:",
      "Call `renv::snapshot()` to capture these dependencies in the lockfile."
    )
  }
  
  # check installed packages and prompt for reload if needed
  renv_install_postamble(names(records))

  # return status
  invisible(records)

}