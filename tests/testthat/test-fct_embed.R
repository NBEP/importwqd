test_that("embed_quarto works", {
  html_in <- paste0(
    "<!DOCTYPE html>",
    '<html xmlns="http://www.w3.org/1999/xhtml" lang="en" xml:lang="en"><head>',
    '<meta charset="utf-8"><meta name="generator" content="quarto-1.7.32">',
    '<meta name="viewport" content="width=device-width, initial-scale=1.0, user-scalable=yes">',
    "<title>About</title><style>code{white-space: pre-wrap;}",
    "span.smallcaps{font-variant: small-caps;}",
    "div.columns{display: flex; gap: min(4vw, 1.5em);}",
    "div.column{flex: auto; overflow-x: auto;}",
    "div.hanging-indent{margin-left: 1.5em; text-indent: -1.5em;}",
    "ul.task-list{list-style: none;}",
    'ul.task-list li input[type="checkbox"] {width: 0.8em;',
    "margin: 0 0.8em 0.2em -1em; vertical-align: middle;}",
    ".display.math{display: block; text-align: center; margin: 0.5rem auto;}",
    '</style></head><body><h1 id="test-header">test header</h1>',
    "<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>",
    '<p><a href="https://www.nbep.org/">',
    '<img src="images/NBEP_logo_square.png" alt="NBEP"></a></p></body></html>'
  )

  html_out <- paste0(
    '<body><h1 id="test-header">test header</h1>',
    "<p>Lorem ipsum dolor sit amet, consectetur adipiscing elit.</p>",
    '<p><a href="https://www.nbep.org/">',
    '<img src="www/images/NBEP_logo_square.png" alt="NBEP"></a></p></body>'
  ) |>
    shiny::HTML()

  expect_equal(
    embed_quarto(html_in),
    html_out
  )
})
