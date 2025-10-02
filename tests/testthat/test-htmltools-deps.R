test_that("copy_htmltools_dependencies works", {
  static_dir <- tempdir()
  options(
    "ambiorix.htmltools_inline" = FALSE,
    "ambiorix.static_path" = static_dir
  )

  library(bslib)
  library(htmltools)
  library(sass)

  theme <- bs_theme(
    version = 5,
    bootswatch = "quartz",
    primary = "#007bff",
    secondary = "#6c757d"
  )

  page <- page_navbar(
    theme = theme,
    title = "My App",
    nav_panel(
      "Home",
      card(
        card_header("Welcome"),
        card_body("This is the home page content")
      )
    ),
    nav_panel(
      "About",
      card(
        card_header("About Us"),
        card_body("This is the about page content")
      )
    )
  )
  resp <- Response$new()$send(tags$html(page))

  expect_true(
    grepl(paste0(static_dir, "/bs3compat/bs3compat.js"), resp$body),
    info = "Dependencies included as files"
  )
})
