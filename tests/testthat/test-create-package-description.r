context("rbundler can create a package description dynamically")

test_that('we can create a dependency clause for the package description',
  {
    dependencies <- data.frame(type=c('Depends', 'Suggests'), package=c('foo', 'bar'),compare=c(NA, '=='), version=c(NA, '1'))
    clause <- dependency_clauses(dependencies)
    expect_equal(clause, "Depends:\n    foo\nSuggests:\n    bar (== 1)")
  }
)

test_that('we can create a simple package description with no dependencies',
  {
  expected <- "Package: nodependencies
Title: A mock package with no dependencies.
License: GPL-2
Description:
Author: Foo Bar <foo.bar@gmail.com>
Maintainer: Foo Bar <foo.bar@gmail.com>
Version: 0.1
Collate:
    ''
"
    name <- 'nodependencies'
    title <- 'A mock package with no dependencies.'
    dependencies <- data.frame(type = c(), package=c(), compare=c(), version=c())
    actual <- create_package_description(name=name, title=title, dependencies=dependencies)
    expect_equal(actual, expected)
  }
)

test_that('we can create a simple package description with versioned and unversioned dependencies',
  {
  expected <- "Package: simpledependency
Title: A mock package with a single dependency.
License: GPL-2
Description:
Author: Foo Bar <foo.bar@gmail.com>
Maintainer: Foo Bar <foo.bar@gmail.com>
Version: 0.1
Depends:
    foo
Suggests:
    bar (== 1)
Collate:
    ''
"
    name <- 'simpledependency'
    title <- 'A mock package with a single dependency.'
    dependencies <- data.frame(type = c('Depends', 'Suggests'), package=c('foo', 'bar'), compare=c(NA, '=='), version=c(NA, '1'))
    actual <- create_package_description(name=name, title=title, dependencies=dependencies)
    expect_equal(actual, expected)
  }
)

test_that('we can create a simple package description with sourced dependencies',
  {
  expected <- "Package: sourceddependency
Title: A mock package with a sourced dependency.
License: GPL-2
Description:
Author: Foo Bar <foo.bar@gmail.com>
Maintainer: Foo Bar <foo.bar@gmail.com>
Version: 0.1
Depends:
    foo
Suggests:
    bar (== 1)
Sources:
    foo (git=git://github.com/bar/foo.git, branch=baz),
    bar (url=http://my.package.repo.com/foo.tar.gz)
Collate:
    ''
"
    name <- 'sourceddependency'
    title <- 'A mock package with a sourced dependency.'
    dependencies <- data.frame(type = c('Depends', 'Suggests'), package=c('foo', 'bar'), compare=c(NA, '=='), version=c(NA, '1'))
    sources <- data.frame(package=c('foo', 'bar'), type=c('git', 'url'), uri=c('git://github.com/bar/foo.git', 'http://my.package.repo.com/foo.tar.gz'), branch=c('baz', ''))
    actual <- create_package_description(name=name, title=title, dependencies=dependencies, sources=sources)
    expect_equal(actual, expected)
  }
)
