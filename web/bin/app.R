source("web/load.R")
source("web/ui/app.R")
settings <- web_settings()
# Local-only identity boundary. Bind loopback; no remote/shared deployment in Block A.
actor <- Sys.getenv("SRMA_LOCAL_ACTOR", "local-researcher")
options(shiny.sanitize.errors = TRUE)
shiny::runApp(web_app(settings, actor), host = "127.0.0.1", port = as.integer(Sys.getenv("SRMA_WEB_PORT", "3838")), launch.browser = FALSE)
