#' @include model-mixed.R
#' @include model-gamma-mixed.R
#' @include model-logit-mixed.R
#' @include model-ls-mixed.R
#' @include model-poisson-mixed.R
#' @include model-probit-mixed.R

#library(jsonlite)


createJSONzeligmixed <- function() {
  
  z5gammamixed <- zgammamixed$new()
  z5gammamixed$toJSON()
  
  z5logitmixed <- zlogitmixed$new()
  z5logitmixed$toJSON()
  
  z5lsmixed <- zlsmixed$new()
  z5lsmixed$toJSON()
  
  z5bpoissonmixed <- zpoissonmixed$new()
  z5bpoissonmixed$toJSON()
  
  z5probitmixed <- zprobitmixed()
  z5probitmixed$toJSON()
  
  zeligmixedmodels <- list(zelig5mixedmodels = list("gammamixed" = z5gammamixed$ljson,
                                                    "logitmixed" = z5logitmixed$ljson,
                                                    "lsmixed" = z5lsmixed$ljson,
                                                    "poissonmixed" = z5bpoissonmixed$ljson,
                                                    "probitmixed" = z5probitmixed$ljson))
  
  # cat(jsonlite::toJSON(zeligchoicemodels, pretty = TRUE),
  #     file = file.path("inst/JSON", "zelig5choicemodels.json"))
  
  cat(toJSON(zeligmixedmodels, pretty = TRUE), file = file.path("zelig5mixedmodels.json"))
  file.rename(from = file.path("zelig5mixedmodels.json"),
              to = file.path("inst", "JSON", "zelig5mixedmodels.json"))
  file.remove(file.path("zelig5mixed.json"))
  
  return(TRUE)
}
