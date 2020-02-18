
# Create back-ups ------------------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Create backup file.
#'
#' @param file (character) The name of file to backup.
#' @param backup_subdir (character) The name of subdirectory in the directory
#'        of backups.
#' @param backup_stamp (character) Usually the result of `get_backup_stamp()`.
#' @param of_what  (character) String to be used in message. Usually word "file"
#'        or the name of `backup_subdir`.
#'
#' @export
#'
create_backup_copy <- function(file = NULL, backup_subdir = "",
  of_what = ifelse(backup_subdir == "", "file(s)", backup_subdir),
  backup_stamp = get_backup_stamp()) {

  f_exist <- fs::file_exists(file)

  if (any(f_exist)) {
    current_files_e <- file[f_exist]
    backup_files    <-
      construct_backup_path(
        current_files_e,
        backup_subdir = backup_subdir,
        backup_stamp = backup_stamp
      )

    create_backup_dir(backup_subdir)

    fs::file_copy(current_files_e, backup_files)

    usethis::ui_done(paste0(
      "Back up copy of {crayon::green(of_what)} was created ",
      "in {usethis::ui_path(unique(fs::path_dir(backup_files)))}"
    ))
    # FIXME: verify that the back-up was successful

    TRUE

  } else {
    usethis::ui_info("No {crayon::green(of_what)} to back-up.")
    TRUE
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Construct path to backup file.
#'
#' @inheritParams create_backup_copy
#'
#' @return The string with path to backup file.
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' construct_backup_path("backup.txt")
#'
#' }}
construct_backup_path <- function(file = NULL, backup_subdir = "",
  backup_stamp = get_backup_stamp()) {

  base0 <- fs::path_file(file)
  ext   <- fs::path_ext(base0)
  base  <- fs::path_ext_remove(base0)

  fs::path(
    get_path_backup_dir(backup_subdir),
    fs::path_ext_set(paste0(base, backup_stamp), ext = ext)
  )
}


# Directory for back-ups -----------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Path to backup directory.
#'
#' - `get_path_backup_dir()` gets string with path to directory for backup files.
#' - `create_backup_dir()` creates directory for backup files.
#' -
#'
#' @param ... The name of subtirectory in backup directory.
#'            Passed to to [fs::path()].
#'
#' @return
#' - `get_path_backup_dir()` returns string with path to directory.
#'    Default is `"~/.R/_backup/"`.
#'
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' get_path_backup_dir()
#'
#' }}
get_path_backup_dir <- function(...) {
  backup_dir <- Sys.getenv("R_SETTINGS_BACKUP_DIR")

  if (backup_dir == "") {
    # The default is "~/.R/_backup/"
    backup_dir <- fs::path_expand_r("~/.R/_backup")

    # # This code might not work on Unix:
    # backup_dir <- fs::path(Sys.getenv("R_USER"), ".R", "_backup")
  }

  fs::path(backup_dir, ...)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' @rdname get_path_backup_dir
#' @export
create_backup_dir <- function(...) {
  b_path <- get_path_backup_dir(...)
  if (!fs::dir_exists(b_path)) {
    fs::dir_create(b_path)
  }
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Get stamp for backup file names.
#'
#' @return Character, e.g., `"__backup_200217_215436"`.
#' @export
#'
#' @examples
#' \dontrun{\donttest{
#'
#' get_backup_stamp()
#'
#' }}
get_backup_stamp <- function() {
  format(Sys.time(), "__backup_%y%m%d_%H%M%OS0")
}
