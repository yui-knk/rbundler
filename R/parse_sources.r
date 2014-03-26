#' Parses the sources information from DESCRIPTION file.
#' @param sources character vector of sources from the DESCRIPTION file
#' @return data.frame with columns name, type, uri, and branch
parse_sources <- function(sources) {
  if (sources_empty(sources)) {
    return()
  } 
  
  stopifnot(is.character(sources), length(sources) == 1)

  source_str <- split_sources(sources)

  names <- parse_names(source_str)
  
  source_str[!have_source(source_str)] <- NA

  types <- parse_types(source_str)
  uris <- parse_uris(source_str)
  branches <- parse_branches(source_str)

  parsed_sources <- data.frame(name = names, type = types,
    uri = uris, branch = branches, stringsAsFactors = FALSE)
}

#' Checks that the sources field in the DESCRIPTION file is empty
#' @param sources character vector of sources from the DESCRIPTION file
#' @return TRUE if sources field is empty
sources_empty <- function(sources) {
  is.null(sources) || grepl("^\\s*$", sources)
}

#' Splits sources into a vector of source strings for processing
#' @param sources from DESCRIPTION file
#' @return vector of source strings
split_sources <- function(sources) {
  strsplit(sources, ",\\s*\\\n")[[1]]  
}

#' Parses package names from source string 
#' @param string of sources
#' @return list of package names
parse_names <- function(source_str) {
  names <- gsub("\\s*\\(.*?\\)", "", source_str)
  gsub("^\\s+|\\s+$", "", names) 
}

#' Parses package types from source string 
#' @param string of sources
#' @return list of package types
parse_types <- function(source_str) {
  sub(".*\\((\\S+?)=.*\\)", "\\1", source_str)
}

#' Parses package uris from source string 
#' @param string of sources
#' @return list of package uris
parse_uris <- function(source_str) {
  sub(".*\\(\\S+=(.*?)[,\\)].*", "\\1", source_str)
}

#' Parses package branches from source string. Returns NA if there is no branch 
#' @param string of sources
#' @return list of package branches.
parse_branches <- function(source_str) {
  ifelse(
    branch_present(source_str),
    trim_branch(source_str),
    NA
  )
}

#' Checks if branch is present
#' @param string of sources
#' @return TRUE is branch is present, FALSE otherwise
branch_present <- function(source_str) {
  grepl(",\\s*branch=", source_str)
}

#' Trims branch from source
#' @param string of sources including a branch
#' #returns branch name
trim_branch <- function(source_str) {
  sub(".*\\(.*branch=(.*?)\\)", "\\1", source_str)
}

#' Checks that a source is present
#' @param string of sources
#' @return TRUE is source is present, FALSE otherwise
have_source <- function(source_str) {
  grepl("\\(.*\\)", source_str)  
}
