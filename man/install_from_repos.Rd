\name{install_from_repos}
\alias{install_from_repos}
\title{Installs a package from the environment repositories}
\usage{
install_from_repos(package, dependencies, .install_version = install_version)
}
\arguments{
  \item{package}{list package information as returned by
  parse_deps}

  \item{dependencies}{vector of package dependencies to
  install}

  \item{.install_version}{function to install proper
  version from repos. defaults to install_version}
}
\description{
Installs a package from the environment repositories
}

