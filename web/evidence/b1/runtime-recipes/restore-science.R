loadNamespace("renv",lib.loc="/workspace/scratch/f22ae3d8c5eb/runtime/renv-bootstrap")
options(timeout=120)
root <- "/workspace/scratch/f22ae3d8c5eb/runtime/scientific-b1"
lib <- renv::paths$library(project=root)
.libPaths(c(lib,.Library),include.site=FALSE)
renv::restore(project=root,library=lib,lockfile=file.path(root,"renv.lock"),prompt=FALSE)
