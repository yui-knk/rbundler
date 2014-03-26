#' Creates a series of mock packages, useful for testing and experimentation.
#' @param path the path in which to create the mock packages
#' @param dependency the dependency to create in the mock packages
#' @param repos the repositories to use for the contrib.url path
#' @return a list of named packages, each of which corresponds to the devtools `as.package` object
#' @export
#' @examples
#' path <- tempdir()
#' repos <- 'http://cran.rstudio.com'
#' dependency <- mock_dependency('RCurl', repos)
#' create_mock_packages(path, dependency, repos)
create_mock_packages <- function(path, dependency, repos=getOption('repos')) {

  list(
    nodependencies = create_package(
      name='nodependencies',
      title='A mock package for testing that a package with no dependencies can be bundled.',
      dependencies=data.frame(),
      path=path
    ),
    simpledependencies = create_package(
      name='simpledependencies',
      title='A mock package for testing that a package with basic dependencies can be bundled.',
      dependencies=data.frame(type=c('Depends'), package=c(dependency$name), compare=c(NA), version=c(NA)),
      path=path
    ),
    versioneddependencies= create_package(
      name='versioneddependencies',
      title='A mock package for testing that a package with versioned dependencies can be bundled.',
      dependencies=data.frame(type=c('Depends'), package=c(dependency$name), compare=c('=='), version=c(dependency$version)),
      path=path
    ),
    gitdependencies = create_package(
      name='gitdependencies',
      title='A mock package for testing that a package with git sources can be bundled.',
      dependencies=data.frame(type=c('Depends'), package=c(dependency$name), compare=c(NA), version=c(NA)),
      # TODO Don't hard code the URI!
      sources=data.frame(package=c(dependency$name), type=c('git'), uri=c('https://github.com/christophsax/tempdisagg.git')),
      path=path
    ),
    gitbranchdependencies = create_package(
      name='gitbranchdependencies',
      title='A mock package for testing that a package with git branch sources can be bundled.',
      dependencies=data.frame(type=c('Depends'), package=c(dependency$name), compare=c(NA), version=c(NA)),
      # TODO Don't hard code the URI!
      sources=data.frame(package=c(dependency$name), type=c('git'), uri=c('https://github.com/christophsax/tempdisagg.git'), branch=c('development')),
      path=path
    ),
    urldependencies = create_package(
      name='urldependencies',
      title='A mock package for testing that a package with git sources can be bundled.',
      dependencies=data.frame(type=c('Depends'), package=c(dependency$name), compare=c(NA), version=c(NA)),
      # TODO Don't hard code the URI!
      sources=data.frame(package=c(dependency$name), type=c('url'), 
        uri=c('http://cran.r-project.org/src/contrib/tempdisagg_0.23.tar.gz')),
      path=path
    ),
    filedependencies = create_package(
      name='filedependencies',
      title='A mock package for testing that a package with git sources can be bundled.',
      dependencies=data.frame(type=c('Depends'), package=c(dependency$name), compare=c(NA), version=c(NA)),
      # TODO Don't hard code the URI!
      sources=data.frame(package=c(dependency$name), type=c('file'), 
        uri=c(file.path(path, 'lib/tempdisagg_0.23.tar.gz'))),
      path=path
    )
  )
}

#' Creates a mock dependency - corresponding to a real package - for use in testing and experimentation.
#' @param name the name of the package dependency
#' @param repos the repositories to use for the contrib.url path
#' @return list with the name and version of the dependency
#' @export
mock_dependency <- function(name='tempdisagg', repos=getOption('repos')) {
  available_versions <- find_available_versions(name, repos = repos)
  # Grab the next-to-latest version of the package.
  version <- available_versions[length(available_versions) - 1, 'version']
  list(name=name, version=version)
}
