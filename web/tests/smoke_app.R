test_library <- Sys.getenv("SRMA_WEB_LIBRARY", "web/.library")
if (dir.exists(test_library)) .libPaths(c(normalizePath(test_library), .libPaths()))
# Foundation-only HTTP bootstrap check; no scientific or database side effects.
shiny::runApp(shiny::shinyApp(
  bslib::page_fluid(title = "SR-MA foundation", shiny::h2("Application foundation ready")),
  function(input, output, session) {}), host = "127.0.0.1", port = 3839, launch.browser = FALSE)
