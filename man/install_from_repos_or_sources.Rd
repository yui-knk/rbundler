\name{install_from_repos_or_sources}
\alias{install_from_repos_or_sources}
\title{Installs a package from either the environment repositories
or from the sources list, if a source for the package is given}
\usage{
install_from_repos_or_sources(package, dependencies, sources,
  .install_from_repos = install_from_repos,
  .install_from_sources = install_from_sources)
}
\arguments{
  \item{package}{list package information as returned by
  parse_deps}

  \item{dependencies}{vector of package dependencies to
  install}

  \item{sources}{data.frame of sources information as
  returned by parse_sources}

  \item{.install_from_repos}{function to install package
  from repositories. defaults to install_from_repos}

  \item{.install_from_sources}{function to install package
  from sources. defaults to install_from_sources}
}
\description{
Installs a package from either the environment repositories
or from the sources list, if a source for the package is
given
}

