if (dir.exists("web/.library")) .libPaths(c(normalizePath("web/.library"), .libPaths()))
# Foundation-only HTTP bootstrap check; no scientific or database side effects.
shiny::runApp(shiny::shinyApp(
  bslib::page_fluid(title = "SR-MA foundation", shiny::h2("Application foundation ready")),
  function(input, output, session) {}), host = "127.0.0.1", port = 3839, launch.browser = FALSE)
