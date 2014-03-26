#' Test double for mocks and stubs
#' Checks function is called with list of arguments and values
#' Will fail in the context of test_that if passed arguments do not match list of arguments
#' Will pass in the contect of test_that if passed arguments match list of arguments
#' @param list of argument names and the expected value to be passed
#' @return function double for the function under test
#' @examples
#'\dontrun{
#' print_param <- function(param, print_function = print) {
#'   print_function(x=param)
#' }
#'
#' test_that("print_param is called with value passed in param", {
#'   print_param("Hello!", print_function=test_double(x="Hello!"))
#' })
#'}

package <- list(name='mockpackage', version='1.0', compare='==')
dependencies = c("Depends", "Imports", "LinkingTo", "Suggests")

test_double <- function(...) {
  expects_args <- list(...)
  function(...) expect_equal(list(...), expects_args)
}

#' Ensures that a package is installed from sources
#' @param desc a description of the test
#' @param package package information
#' @param type the source type to install from 
#' @param uri the uri of the source
#' @param branch the source branch, defaults to NA
test_install_from_sources <- function(desc, name=package['name'], type='git', uri='git@github.com:example/mockpackage.git', branch=NA) {
  test_that(
    desc, 
    {
      sources <- data.frame(name=package['name'], type=type, uri=uri, branch=branch, stringsAsFactors=FALSE)
      switch(type,
        'git'={
          ifelse(
            is.na(branch),
            install_from_sources(package, sources, .install_git=test_double(git_url=uri)),
            install_from_sources(package, sources, .install_git=test_double(git_url=uri, branch=branch))
          )
        },
        'url'={
          install_from_sources(package, sources, .install_url=test_double(url=uri))
        },
        'file'={
          install_from_sources(package, sources, .install_file=test_double(path=uri))
        }  
      )
    }
  )
}

#' Ensures that a package is installed from repos or sources
#' @param desc a description of the test
#' @param name name of the package
#' @param type the source type to install from 
#' @param uri the uri of the source
#' @param branch the source branch, defaults to NA
test_install_from_repos_or_sources <- function(desc, name=package['name'], type='git', uri='git@github.com:example/mockpackage.git', branch=NA) {  
  test_that(
    desc, 
    {
      sources <- data.frame(name=package['name'], type=type, uri=uri, branch=branch, stringsAsFactors=FALSE)
      install_from_sources(package, sources, .install_git=test_double(git_url=uri))
    }
  )
}


context('Test install_from_sources')

test_install_from_sources(
  "install_git is called for git sources with source uri", 
  type='git', 
  uri='git@github.com:example/mockpackage.git'
)

test_install_from_sources(
  "install_git is called for git sources with source uri and branch",
  type='git', 
  uri='git@github.com:example/mockpackage.git', 
  branch='dev'
)

test_install_from_sources(
  "install_url is called for url sources with source uri", 
  type='url', 
  uri='http://example.com/mockpackage.tar.gz'
)

test_install_from_sources(
  "install_file is called for file sources with source uri", 
  type='file', uri='./lib/mockpagage.tar.gz'
)

context('Test install_from_repos')

test_that("install_version is called for package with dependencies", {
  install_from_repos(package, dependencies, .install_version=test_double(
      package=package['name'], version=package['version'], compare=package['compare'], dependencies=dependencies
  ))
})

context('Test install_from_repos_or_sources')

test_that("install_from_repos is called if no sources is NULL", {
  install_from_repos_or_sources(package, dependencies, NULL, .install_from_repos=test_double(
      package=package, dependencies=dependencies
  ))
})

test_install_from_repos_or_sources(
  "install_from_repos is called if package name not in sources",
  name='anotherPackage'
)

test_install_from_repos_or_sources("install_from_sources is called if package name is in sources")
