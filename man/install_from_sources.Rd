\name{install_from_sources}
\alias{install_from_sources}
\title{Installs a package from the repository in the sources list}
\usage{
install_from_sources(package, sources, .install_git = install_git,
  .install_url = install_url, .install_file = install_local)
}
\arguments{
  \item{package}{list package information as returned by
  parse_deps}

  \item{sources}{data.frame of sources information as
  returned by parse_sources}

  \item{.install_git}{function to install from git.
  defaults to install_git}

  \item{.install_url}{function to install from url.
  defaults to install_url}

  \item{.install_file}{function to install from file.
  defaults to install_local}
}
\description{
Installs a package from the repository in the sources list
}

