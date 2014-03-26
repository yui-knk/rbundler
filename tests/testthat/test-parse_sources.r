context("Sources can be read from the DESCRIPTION file.")

test_path <- file.path(tempdir(), sprintf('bundle-test-%s', as.numeric(Sys.time())))
dir.create(test_path)
repos <- 'http://cran.rstudio.com'
dependency <- mock_dependency(repos=repos)
mock_packages <- create_mock_packages(test_path, dependency, repos)

test_that('parse_sources returns NULL for if given a NULL source string', 
  expect_equal(parse_sources(NULL), NULL) 
)

test_that('parse_sources returns NULL if given a source string consisting of whitespace characters',
  expect_equal(parse_sources('    \t\n'), NULL)
)

test_parse_sources <- function(desc, package, expected_sources) {
  test_that(
    desc,
    {
      package <- mock_packages[[package]]
      pkg <- as.package(package$path)
      if (!is.null(expected_sources)) {
        expected_sources <- as.data.frame(expected_sources, stringsAsFactors = FALSE)
      }
      expect_equal(parse_sources(pkg$sources), expected_sources)
    }
  )
}

test_parse_sources(
  desc = 'can parse a package with no dependencies',
  package = 'nodependencies',
  expected_sources = NULL
)

test_parse_sources(
  desc = 'can parse a package with simple dependencies',
  package = 'simpledependencies',
  expected_sources = NULL
)

test_parse_sources(
  desc = 'can parse a package with version dependencies',
  package = 'versioneddependencies',
  expected_sources = NULL
)

test_parse_sources(
  desc = 'can parse a package with git dependencies',
  package = 'gitdependencies',
  expected_sources = list(name=dependency['name'], type='git', uri='https://github.com/christophsax/tempdisagg.git', branch=NA)
)

test_parse_sources(
  desc = 'can parse a package with git and branch dependencies',
  package = 'gitbranchdependencies',
  expected_sources = list(name=dependency['name'], type='git', uri='https://github.com/christophsax/tempdisagg.git', branch='development')
)

test_parse_sources(
  desc = 'can parse a package with url dependencies',
  package = 'urldependencies',
  expected_sources = list(name=dependency['name'], type='url', uri='http://cran.r-project.org/src/contrib/tempdisagg_0.23.tar.gz', branch=NA)
)

test_parse_sources(
  desc = 'can parse a package with file dependencies',
  package = 'filedependencies',
  expected_sources = list(name=dependency['name'], type='file', uri=file.path(test_path, 'lib/tempdisagg_0.23.tar.gz'), branch=NA)
)
