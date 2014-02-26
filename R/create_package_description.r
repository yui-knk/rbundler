#' A Utility function for creating rbundler scenarios.
#' @param name the name of the package to create
#' @param title the title of the package to create
#' @param dependencies a data.frame with dependency type, package, compare, version set.
#' @param sources a data.frame with source package, type, uri, and branch (optional).
#' @export
#' @examples
#'
#' name <- 'simpledependency'
#' title <- 'A mock package with a single dependency.'
#' dependencies <- data.frame(type = c('Depends', 'Suggests'), package=c('foo', 'bar'),
#'                            compare=c(NA, '=='), version=c(NA, '1'))
#' description <- create_package_description(name, title, dependencies)
#'
#' write(description, file='') # Write the output to the console
create_package_description <- function(name, title, dependencies, sources = data.frame()) {
  header <- sprintf("Package: %s
Title: %s
License: GPL-2
Description:
Author: Foo Bar <foo.bar@gmail.com>
Maintainer: Foo Bar <foo.bar@gmail.com>
Version: 0.1",
    name,
    title
  )

  footer <- sprintf("Collate:
    ''
")

  output <- sprintf("%s", header)
  
  if(nrow(dependencies) != 0) {
      output <- sprintf("%s\n%s",
      output,
      dependency_clauses(dependencies)
    )
  }

  if(nrow(sources) != 0) {
      output <- sprintf("%s\n%s",
      output,
      source_clauses(sources)
    )
  }
  
  sprintf("%s\n%s", output, footer)
}

#' Creates the `Depends:` clause by concatenating individual packages and adding their compare clauses.
#' @param dependencies a data.frame with dependency package, compare, and version set.
dependency_clauses <- function(dependencies) {
  clauses <- c()
  for(type in unique(dependencies$type)) {
      type_dependencies <- dependencies[dependencies$type == type, ]
      clauses <- c(
        clauses,
        sprintf("%s:\n%s",
        type,
        paste(
          ifelse(
            is.na(type_dependencies$version),
            sprintf('    %s', type_dependencies$package),
            sprintf('    %s (%s %s)', type_dependencies$package, type_dependencies$compare, type_dependencies$version)
          ),
          collapse=',\n'
        )
      )
    )
  }

  paste(clauses, collapse='\n')

}

#' Creates the `Sources:` clause by concatenating individual packages and adding their source clauses.
#' @param sources a data.frame with source package, type, uri, and optional branch set.
source_clauses <- function(sources) {

  if (!('branch' %in% names(sources)) ) {
    sources$branch <- ''
  }
  
  sprintf("Sources:\n%s",
    paste(
      ifelse(
        sources$branch == '',
        sprintf('    %s (%s=%s)', sources$package, sources$type, sources$uri),
        sprintf('    %s (%s=%s, branch=%s)', sources$package, sources$type, sources$uri, sources$branch)
      ),
      collapse=',\n'
    )
  )
}
