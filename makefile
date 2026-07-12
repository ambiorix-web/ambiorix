install:
	Rscript -e "devtools::install()"

check:
	Rscript -e "devtools::check()"

document: 
	Rscript -e "devtools::document()"

test:
	Rscript -e "devtools::test()"

dev:
	Rscript test.R

# Vendored Swagger UI assets (see inst/COPYRIGHTS).
# Bump SWAGGER_UI_VERSION and run `make update-swagger` to refresh.
SWAGGER_UI_VERSION = 5.32.8

update-swagger:
	curl -fsSL -o inst/swagger-ui/swagger-ui.css https://cdn.jsdelivr.net/npm/swagger-ui-dist@$(SWAGGER_UI_VERSION)/swagger-ui.css
	curl -fsSL -o inst/swagger-ui/swagger-ui-bundle.js https://cdn.jsdelivr.net/npm/swagger-ui-dist@$(SWAGGER_UI_VERSION)/swagger-ui-bundle.js
	curl -fsSL -o inst/swagger-ui/swagger-ui-bundle.js.LICENSE.txt https://cdn.jsdelivr.net/npm/swagger-ui-dist@$(SWAGGER_UI_VERSION)/swagger-ui-bundle.js.LICENSE.txt
	curl -fsSL -o inst/swagger-ui/LICENSE https://raw.githubusercontent.com/swagger-api/swagger-ui/v$(SWAGGER_UI_VERSION)/LICENSE
	sed -i 's|/\*# sourceMappingURL=swagger-ui.css.map\*/||' inst/swagger-ui/swagger-ui.css
