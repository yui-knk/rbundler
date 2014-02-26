#' Installs a package from either the environment repositories 
#' or from the sources list, if a source for the package is given
#' @param package list package information as returned by parse_deps
#' @param dependencies vector of package dependencies to install
#' @param sources data.frame of sources information as returned by parse_sources
#' @param .install_from_repos function to install package from repositories. defaults to install_from_repos
#' @param .install_from_sources function to install package from sources. defaults to install_from_sources
install_from_repos_or_sources <- function(package, dependencies, sources, .install_from_repos=install_from_repos, .install_from_sources=install_from_sources) {
  ifelse(
    is.null(sources) || !(package['name'] %in% sources$name),
    .install_from_repos(package=package, dependencies=dependencies),
    .install_from_sources(package=package, sources=sources)
  )  
}

#' Installs a package from the environment repositories 
#' @param package list package information as returned by parse_deps
#' @param dependencies vector of package dependencies to install
#' @param .install_version function to install proper version from repos. defaults to install_version
install_from_repos <- function(package, dependencies, .install_version=install_version) {
  .install_version(
    package=package['name'], version=package['version'], compare=package['compare'],
    dependencies = dependencies
  )
}

#' Installs a package from the repository in the sources list
#' @param package list package information as returned by parse_deps
#' @param sources data.frame of sources information as returned by parse_sources
#' @param .install_git function to install from git. defaults to install_git
#' @param .install_url function to install from url. defaults to install_url
#' @param .install_file function to install from file. defaults to install_local
#' @importFrom devtools install_git
#' @importFrom devtools install_url
#' @importFrom devtools install_local
install_from_sources <- function(package, sources, .install_git=install_git, .install_url=install_url, .install_file=install_local) {
  source <- sources[sources$name == package['name'], ]
  switch(source$type,
    'git'={
      ifelse(
        is.na(source$branch),
        .install_git(git_url=source$uri),
        .install_git(git_url=source$uri, branch=source$branch)
      )
    },
    'url'={
      .install_url(url=source$uri)
    },  
    'file'={
      .install_file(path=source$uri)
    },            
    {
      stop(source$type, " for package ", package['name'], " is an unrecognized source.")
    }
  ) 
}
