install: check
	Rscript -e "devtools::install()"

check: document
	Rscript -e "devtools::check()"

document: 
	Rscript -e "devtools::document()"

test:
	Rscript -e "devtools::test()"
