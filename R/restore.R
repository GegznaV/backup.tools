# Restore from back up  ------------------------------------------------------

remove_backup_id <- function(str) {
  stringr::str_remove(str, "__backup_\\d{6}_\\d{6}")
}

extract_filename_to_restore <- function(path = NULL) {
  # path <- "D:/Dokumentai/.R-backup/keybindings/addins__backup_200211_223335.json"
  path %>%
    fs::path_file() %>%
    remove_backup_id()
}

restore_from_backup <- function(..., backup = TRUE) {
  # FIXME: not implemented
  stop("not implemented")
  # original_name <- extract_filename_to_restore()
}