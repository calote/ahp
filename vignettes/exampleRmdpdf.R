## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------------------------------------
options(width=120)
#opts_chunk$set(comment = "", warning = FALSE, message = FALSE, echo = TRUE, tidy = FALSE, size="small")
knitr::opts_chunk$set(warning = FALSE)
#read_chunk("some/script/I/want/to/load.R")

## ---------------------------------------------------------------------------------------------------------------------
library(formattable)
library(htmltools)
library(webshot)
export_formattable <- function(f, file, width = "100%", height = NULL, 
                               background = "white", delay = 0.2)
{
  w <- formattable::as.htmlwidget(f, width = width, height = height)  
  path <- htmltools::html_print(w, background = background, viewer = NULL)
  url <- paste0("file:///", gsub("\\\\", "/", normalizePath(path)))
  webshot::webshot(url,
          file = file,
          selector = ".formattable_widget",
          delay = delay)
}

## ---------------------------------------------------------------------------------------------------------------------
library(ahp)
ahpFile <- system.file("extdata", "car.ahp", package="ahp")
carAhp <- Load(ahpFile)

## ---------------------------------------------------------------------------------------------------------------------
library(data.tree)
print(carAhp, filterFun = isNotLeaf)

## ---------------------------------------------------------------------------------------------------------------------
Calculate(carAhp)
print(carAhp, priority = function(x) x$parent$priority["Total", x$name])

## ---------------------------------------------------------------------------------------------------------------------
Visualize(carAhp)

## ---------------------------------------------------------------------------------------------------------------------
export_formattable(AnalyzeTable(carAhp), file = "table01.png")

