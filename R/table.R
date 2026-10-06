


#' Displays the AHP analysis in form of an html table, with gradient
#' colors and nicely formatted.
#' 
#' @param weightColor The name of the color to be used to emphasize weights of categories. See \code{color} for a list of possible colors.
#' @param consistencyColor The name of the color to be used to highlight bad consistency
#' @param alternativeColor The name of the color used to highlight big contributors to alternative choices.
#' @return AnalyzeTable returns a \code{\link{formattable}} data.frame object which, in most environments, will be displayed as an HTML table
#' 
#' @rdname Analyze
#' @importFrom formattable formattable formatter style csscolor gradient is.formattable icontext
#' @export 
AnalyzeTable <- function(ahpTree,
                         decisionMaker = "Total",
                         variable = c("weightContribution", "priority", "score"),
                         sort = c("priority", "totalPriority", "orig"),
                         pruneFun = function(node, decisionMaker) TRUE,
                         weightColor = "honeydew3",
                         consistencyColor = "wheat2",
                         alternativeColor = "thistle4") {
  
                           

  df <- GetDataFrame(ahpTree = ahpTree, 
                     decisionMaker = decisionMaker, 
                     variable = variable, 
                     sort = sort, 
                     pruneFun = pruneFun)
  df <- df[ , -1]
  
  #alternatives <- names(df)[-c(1:3, ncol(df))]
  alternatives <- GetAlternativesNames(ahpTree)
  dfw <- df[ , alternatives, drop = FALSE]
  
  dfw[is.na(dfw)] <- 1
  #cols <- 2 * dfw/max(dfw) + dfw/df[ , 3]
  #cols <- df[,-(1:4)]
  if (variable[1] == "weightContribution") cols <- dfw/max(dfw)
  else cols <- dfw/apply(dfw, MARGIN = 1, FUN = max)
  
  cols$zero <- 0
  cols$max <- max(cols)
  cols <- t(apply(cols, MARGIN = 1, function(x) csscolor(gradient(x, "white", alternativeColor))))
  cols <- cols[,1:(ncol(cols)-2), drop = FALSE]
  
  names(df)[1] <- " "
  
  myFormatters <- vector("list", length(alternatives))
  names(myFormatters) <- alternatives
  alternativesFormatter <- percent1
  if (variable[1] == "score") alternativesFormatter <- identity
  for(a in alternatives) myFormatters[[a]] <- ColorTileRowWithFormatting(cols[,a], alternativesFormatter)
  
  
  myFormatters[[colnames(df)[3]]] <- ColorTileWithFormatting("white", weightColor, percent1)
  myFormatters$Inconsistency <- ConsistencyFormatter("white", consistencyColor, percent1)
  myFormatters$` ` <- formatter("span", 
                                style = style(`white-space` = "nowrap",
                                              `text-align` = "left",
                                              float = "left",
                                              `font-weight` = "bold",
                                              `text-indent` = paste0((df$level-1), "em")
                                ))
  
  
  
  
  # NOTE: pass myFormatters positionally (unnamed), not as `formatters =`.
  # formattable stores ... verbatim in attr(x, "formattable")$format, and its
  # knit_print does `format$format`, which partially matches an element named
  # "formatters" and breaks with 'length > 1 in coercion to logical(1)' on
  # modern R. The rendered HTML is identical either way.
  formattable(df[ , -2], myFormatters)

}


#' Displays the AHP analysis as a \code{\link[tinytable:tt]{tinytable}} table,
#' with the same gradient colors as \code{AnalyzeTable} but rendered natively
#' in each output format (HTML, LaTeX/PDF and Typst), without screenshots.
#'
#' @param fontsize Font size in em units, passed to
#'   \code{\link[tinytable:style_tt]{style_tt}}. Useful to fit wide tables on
#'   a Typst page (e.g. \code{0.8}). \code{NULL} keeps the default size.
#' @return AnalyzeTableTiny returns a \code{\link[tinytable:tt]{tinytable}}
#'   object. Note that, unlike \code{AnalyzeTable}, no warning icon is shown
#'   for high inconsistency (only the background color); values are otherwise
#'   identical.
#'
#' @rdname Analyze
#' @export
AnalyzeTableTiny <- function(ahpTree,
                             decisionMaker = "Total",
                             variable = c("weightContribution", "priority", "score"),
                             sort = c("priority", "totalPriority", "orig"),
                             pruneFun = function(node, decisionMaker) TRUE,
                             weightColor = "honeydew3",
                             consistencyColor = "wheat2",
                             alternativeColor = "thistle4",
                             fontsize = NULL) {

  if (!requireNamespace("tinytable", quietly = TRUE)) {
    stop("Package 'tinytable' is required for AnalyzeTableTiny(). Please install it with install.packages(\"tinytable\").")
  }

  df <- GetDataFrame(ahpTree = ahpTree,
                     decisionMaker = decisionMaker,
                     variable = variable,
                     sort = sort,
                     pruneFun = pruneFun)
  df <- df[ , -1]

  alternatives <- GetAlternativesNames(ahpTree)
  dfw <- df[ , alternatives, drop = FALSE]

  dfw[is.na(dfw)] <- 1
  if (variable[1] == "weightContribution") cols <- dfw/max(dfw)
  else cols <- dfw/apply(dfw, MARGIN = 1, FUN = max)

  cols$zero <- 0
  cols$max <- max(cols)
  cols <- t(apply(cols, MARGIN = 1, function(x) formattable::csscolor(formattable::gradient(x, "white", alternativeColor))))
  cols <- cols[,1:(ncol(cols)-2), drop = FALSE]

  displevel <- df$level
  names(df)[1] <- " "
  disp <- df[ , -2]

  # NOTE: the weight column is called "Weight" or "Priority" depending on
  # `variable` (AnalyzeTable uses it positionally as well). Never assume
  # df$Weight exists: with variable = "priority" it is NULL.
  weightCol <- colnames(df)[3]
  numcols <- setdiff(names(disp), " ")
  bg <- cbind(formattable::csscolor(formattable::gradient(df[[weightCol]], "white", weightColor)),
              cols[ , alternatives, drop = FALSE],
              Inconsistency = formattable::csscolor(formattable::gradient(pmin(df$Inconsistency, 0.1), "white", consistencyColor)))
  colnames(bg)[1] <- weightCol
  bg <- bg[ , numcols, drop = FALSE]

  # NOTE: tinytable's `background` only accepts a single color per style_tt()
  # call (a vector fails at render time), so background colors are applied
  # cell by cell. `background` must also be the last style applied: any later
  # style_tt() call with selectors re-validates the stored backgrounds.
  if (variable[1] == "score") fmt <- function(x) ifelse(is.na(x), "NA", as.character(x))
  else fmt <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f%%", 100*x))
  disp[ , numcols] <- lapply(disp[ , numcols], fmt)

  t <- tinytable::tt(disp, width = 1)
  if (!is.null(fontsize)) t <- tinytable::style_tt(t, fontsize = fontsize)
  t <- tinytable::style_tt(t, j = 1, bold = TRUE)
  t <- tinytable::style_tt(t, j = numcols, align = "r")
  for (i in seq_len(nrow(disp))) t <- tinytable::style_tt(t, i = i, j = 1, indent = displevel[i]-1)
  for (j in seq_len(ncol(bg))) for (i in seq_len(nrow(bg))) {
    t <- tinytable::style_tt(t, i = i, j = match(colnames(bg)[j], names(disp)), background = bg[i, j])
  }

  t

}


#' @param node the \code{Node}
#' @param minWeight prunes the nodes whose weightContribution is smaller than the minWeight
#' 
#' @return the Prune methods must return \code{TRUE} for nodes to be kept, \code{FALSE} for nodes to be pruned
#' 
#' @rdname Analyze
#' @export
PruneByCutoff <- function(node, decisionMaker, minWeight = 0) {
  res <- sum(node$weightContribution[decisionMaker, ]) >= minWeight
  return (res)
}



#' @param levelsToPrune cuts the \code{n} hightest levels of the ahp tree 
#' 
#' @rdname Analyze
#' @export
PruneLevels <- function(node, decisionMaker, levelsToPrune = 0) {
  return (node$level <= (node$root$height - levelsToPrune - 1))
}



#' @importFrom formattable percent
percent1 <- function(x) {
  if (all(is.na(x))) return (x)
  percent(x, digits = 1)
}

ColorTileWithFormatting <- function(c1, c2, format) {
  
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px", 
              `background-color` = csscolor(gradient(x, c1, c2))),
            x ~ format(x)
  )
}


ColorTileRowWithFormatting <- function(cols, format) {
  
  formatter("span", 
            style = style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px", 
              `background-color` = cols),
            x ~ format(x)
  )
}



ConsistencyFormatter <- function(c1, c2, format) {
  
  formatter("span", 
            style = x ~ style(
              display = "block", 
              padding = "0 4px", 
              `border-radius` = "4px", 
              `background-color` = csscolor(gradient(pmin(x, 0.1), c1, c2))),
            x ~ icontext(ifelse(x > 0.1 , "exclamation-sign", NA), format(x))
  )
}
