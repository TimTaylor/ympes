doc:
	R -s -e "roxygen2::roxygenize('pkg', load_code = roxygen2::load_pkgload)"

pkg: doc
	rm -f *.tar.gz
	R CMD build pkg

install: pkg
	R CMD INSTALL *.tar.gz

check: pkg
	R CMD check *.tar.gz

cran: pkg
	R CMD check --as-cran *.tar.gz

test: doc
	R -s -e "tinytest::build_install_test('pkg')"

#test: doc
#	R -s -e "devtools::test('pkg')"

manual: doc
	R CMD Rd2pdf --force -o manual.pdf ./pkg

revdep: pkg
	rm -rf revdep
	mkdir revdep
	mv *.tar.gz revdep
	R -s -e "out <- tools::check_packages_in_dir('revdep',reverse=list(which='most')); print(summary(out)); saveRDS(out, file='revdep/output.RDS')"

clean:
	rm -rf *.Rcheck
	rm -rf revdep
	rm -f *.tar.gz
	rm -f manual.pdf
