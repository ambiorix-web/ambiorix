install: check
	Rscript -e "devtools::install()"

check: docs
	Rscript -e "devtools::check()"

docs: document
	Rscript docs/docify.R

document: 
	Rscript -e "devtools::document()"
