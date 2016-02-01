# updated by PB to include return, various copy edits

#' @title Vmerge
#'
#' @description More verbose merge function
#'
#' @param x first data.frame to merge, same as in \code{\link[base]{merge}}.
#' @param y second data.frame to merge, same as in \code{\link[base]{merge}}.
#' @param by character vector of column names to merge by. When \code{by} is used, the column names
#'           must be the same in \code{x} and \code{y}. Silently overrides \code{by.x}
#'           and \code{by.y}
#' @param by.x character vector of column names on \code{x} to merge by. The resulting file will have 
#'             these names.
#' @param by.y character vector of column names on \code{y} to merge by.
#' @param all.x boolean indicating if unmerged rows from \code{x} should be included in the output.
#' @param all.y boolean indicating if unmerged rows from \code{y} should be included in the output.
#' @param all boolean indicating if unmerged rows from \code{x} and \code{y} should be included in the output.
#'            Silently overrides \code{all.x} and \code{all.y}.
#' @param order character string from "sort", "unsorted", "x", and "y".
#'              Specifies the order of the output. Setting this to "sort"
#'              gives the same result as \code{\link[base]{merge}} with sort=TRUE.
#'              unsorted gives the same result as sort=FALSE. "x" and "y" sort by the incoming
#'              sort order of \code{x} and \code{y}, respectively. 
#' @param fast boolean indicating if \code{data.table} should be used to do the merge. 
#' @param merge.type.colname character indicating the column name of the resulting merge type column.
#'                           See description.
#' @param return.list boolean indicating if the merged data.frame and verbose output should be
#'                    returned as elements of a list. Defaults to FALSE where the function
#'                    simply returns a data.frame.
#' @param verbose booliean indicating if output should be reported. Defaults to TRUE. Useful for testing.
#' @param ... additional parameters passed to merge.
#'
#' @details 
#' This is a wrapper for the base package merge function that prints out verbose information 
#' about the merge, including the merge type (one/many to one/many), the overlapping column
#' names that will have suffixes applied, the number of rows and the number of unique
#' keys that are in each dataset and in the resulting dataset. 
#'
#' Also gives more detailed errors when, e.g. the columns named in the \code{by} argument are
#' not on the \code{x} or \code{y} data.frames.
#' 
#' @return
#' depends on the value of \code{return.list}.
#'
#' When \code{return.list} is \code{FALSE}, returns a \code{data.frame}. 
#'
#' When \code{return.list} is \code{TRUE}, returns a list with two elements. The first is the same \code{data.frame} result. The second
#' is a list with the values that were printed out. Elements include merge.type with two elements, each "one" or "many" indicating the
#' merge type for \code{x} and \code{y}, respectively; inBoth, the list of column names in both merged data.frames; and merge.matrix
#' the matrix printed out by this function.
#'
#' @seealso \code{\link[base]{merge}}
#' 
#' @examples
#' #you're on your own, bucko
#' 
#' @export
mergev <- function(x, y, 
                   by=NULL, by.x=NULL, by.y=NULL,
                   all.x=NULL, all.y=NULL, all=FALSE,
                   order=c("sort", "unsorted", "x","y"),
                   fast=FALSE,
                   merge.type.colname="merge.type",
                   return.list=FALSE,
                   verbose=TRUE,
                   ...) {
  
  # get the names x and y were called with
  x.arg.name <- paste(deparse(substitute(x)), collapse="")
  y.arg.name <- paste(deparse(substitute(y)), collapse="")
  
  genVarNames <- function(x, df) {
    while(TRUE) {
      prev <- substr(x, 2, nchar(x))
      y <- paste0(x, prev, sample(9,1))
      if(!y %in% names(df)) {
        break
      }
    }
    y
  }

  # similar to merge.default
  x <- as.data.frame(x)
  y <- as.data.frame(y)

  if(!inherits(x, "data.frame")) {
    stop(paste0(sQuote("x"), " must be a data.frame or something that can be cast as a data.frame"))
  }

  if(!inherits(y, "data.frame")) {
    stop(paste0(sQuote("y"), " must be a data.frame or something that can be cast as a data.frame"))
  }


  lst <- list()

  # RFE: this will not deal with the situation where a suffix is the 0 length string
  if( sum(merge.type.colname == c(names(x), names(y))) == 1 ) {
    warning(paste0(sQuote("merge.type.colname"), " is on x or y. It will be overwritten."))
  }
  
  if(fast & is.null(by)){
    stop(paste0("You must specify ",sQuote("by"), " if you set ",sQuote("fast"), " to ",sQuote("TRUE")))
  }

  params <- list(...)
  if('sort' %in% names(params)) {
    if(length(order)!=4) {
      if(params$sort==TRUE & order!="sort") {
        stop(paste0("You need to set order to ", sQuote("sort"), "if sort is set to ",sQuote(TRUE)))
      }  
    }
  }
  
  if(is.null(by) & (is.null(by.x) & is.null(by.y))) {
    # note, merge will allow this and return the Cartesian product.
    # there is not so much verbose output for such a thing.
    stop(paste0("For mergev, you need to specify either ", sQuote("by"), " or both ", sQuote("by.x"), "and ",sQuote("by.y"), ". If you want the Cartesian product, try the merge function in the base package."))
  }
  
  if(!is.null(by)) {
    by.x <- by
    by.y <- by
  }
  
  # fix logical by values. These are resolved seperately on x and y in merge
  if(inherits(by.x, "logical")) {
    by.x <- names(x)[by.x]
  }

  if(inherits(by.y, "logical")) {
    by.y <- names(x)[by.y]
  }

  # numeric by.x
  rn.x <- NULL
  num.x <- FALSE
  by.x.original <- by.x
  if(is.numeric(by.x)) {
    if(0 %in% by.x) {
      num.x <- TRUE
      rn.x <- genVarNames("rn", x)
      x[,rn.x] <- rownames(x)
      by.x[!by.x %in% 0] <- by.x[!by.x %in% 0]
      by.x[by.x %in% 0] <- ncol(x)
    }
    by.x <- names(x)[by.x]
  }
  
  # numeric by.y
  rn.y <- NULL
  num.y <- FALSE
  by.y.original <- by.y
  if(is.numeric(by.y)) {
    if(0 %in% by.y) {
      num.y <- TRUE
      rn.y <- genVarNames("rny", y)
      y[,rn.y] <- rownames(y)
      by.y[!by.y %in% 0] <- by.y[!by.y %in% 0]
      by.y[by.y %in% 0] <- ncol(y)
    }
    by.y <- names(y)[by.y]
  }
  
  if(length(by.x) != length(by.y)) {
    stop(paste0("You need to specify same number of ", sQuote("X"), " and ", sQuote("Y"), " variables"))
  }
  
  if(is.null(all.x)) {
    all.x <- all
  } 

  if(is.null(all.y)) {
    all.y <- all
  }

  if(nrow(x) == 0) {
    stop(paste0("No rows in ", x.arg.name, "."))
  }
  
  if(nrow(y) == 0) {
    stop(paste0("No rows in ", y.arg.name, "."))
  }
  
  if(sum(by.x %in% names(x)) != length(by.x)) {
    stop(paste0("Not all by variables in ", x.arg.name,", the following are missing:", paste(by.x[!by.x %in% names(x)], collapse=", ") ) )
  }
  if(sum(by.y %in% names(y)) != length(by.y)) {
    stop(paste0("Not all by variables in ", y.arg.name,", the following are missing:", paste(by.y[!by.y %in% names(y)], collapse=", ") ) )
  }
  
  for(i in 1:length(by.x)) {
    nx <- sum(is.na(x[,by.x[i]]))
    ny <- sum(is.na(y[,by.y[i]]))
    if(nx >= 1 | ny >= 1) {
      warning("For by variable, '", by.x[i], "' There are ",nx," NA values on ", x.arg.name," and ", ny," NA values on ", y.arg.name,".")
    }
    if(class(x[,by.x[i]]) == "factor" & class(y[,by.y[i]]) == "factor")  {
      if(mean( levels(x[,by.x[i]]) %in% levels(y[,by.y[i]])) + mean( levels(y[,by.y[i]]) %in% levels(x[,by.x[i]])) ==2)  {
        # do nothing, should work
      } else {
        # convert to character
        #warning("by variables, ", sQuote(by.x[i]), " and ", sQuote(by.y[i]) ," are factors in both data sets but the levels disagree. Converting to character.")
        #x[,by.x[i]] <- as.character(x[,by.x[i]])
        #y[,by.y[i]] <- as.character(y[,by.y[i]])
      }
    } else {
      if(class(x[,by.x[i]]) %in% c("factor", "character")) {
        if(class(y[,by.y[i]]) == "numeric") {
          #warning(paste0("by variable, ", sQuote(by.x[i]), " is a ",class(x[,by.x[i]])," in ", x.arg.name," but ", sQuote(by.y[i]), " is not in ", y.arg.name,". Converting to numeric."))
          #x[,by.x[i]] <- as.numeric(x[,by.x[i]])
        } else {
          if(class(x[,by.x[i]]) != "character") {
            #warning("by variable, ", sQuote(by.x[i]), " is a factor in ", x.arg.name," but ", sQuote(by.y[i])," is not in ", y.arg.name,". Converting to character.")
            #x[,by.x[i]] <- as.character(x[,by.x[i]])
          }
        }
      }
      if(class(y[,by.y[i]]) %in% c("factor", "character")) {
        if(class(x[,by.x[i]]) == "numeric") {
          #warning("by variable, ", sQuote(by.y[i]), " is a ",class(y[,by.y[i]])," in ", y.arg.name," but ", sQuote(by.x), " is not in ", x.arg.name,". Converting to numeric.")
          #y[,by.y[i]] <- as.numeric(y[,by.y[i]])
        } else {
          if(class(x[,by.x[i]]) != "character") {
            #warning("by variable, ", sQuote(by.y[i]), " is a factor in ", y.arg.name," but ",sQuote(by.x[i]), " not in ", x.arg.name,". Converting to character.")
            #y[,by.y[i]] <- as.character(y[,by.y[i]])
          }
        }
      }
      
    }
    
  }
  
  order <- tolower(order[[1]])
  if(!order %in% c("unsorted", "sort", "x","y")) {
    stop(paste0(sQuote("order"), " must be one of ", sQuote("x"), ", ", sQuote("y"), ", ", sQuote("sort"), ", or ", sQuote("unsorted"),"."))
  }
  if(order %in% c("x", "y")) {
    order.var <- "order"
    while(order.var %in% c(names(x), names(y))) {
      order.var <- paste0(order.var, sample(LETTERS,1))
    }
    if(order=="x") {
      x[,order.var] <- 1:nrow(x)
    } else {
      y[,order.var] <- 1:nrow(y)
    }
  } else {
    if(order == "sort") {
      sort <- TRUE
    } else {
      sort <- FALSE
    }
  }
  

  tempx <- genVarNames("x", x)
  tempy <- genVarNames("y", y)
  
  x[,tempx] <- c(1:nrow(x))
  y[,tempy] <- c(1:nrow(y))
  
  v1 <- names(x)
  v2 <- names(y)
  # create the verbose output matrix
  rn <- c("Rows in", "Unique keys in", "Rows out", "Unique keys out")
  
  mat <- matrix(0, nrow=length(rn), ncol=3, dimnames=list(rn,c(x.arg.name, y.arg.name, "total")))
  mat[1,1] <- nrow(x)
  mat[1,2] <- nrow(y)
  mat[1,3] <- mat[1,1] + mat[1,2]
  mat[2,1] <- nrow(ux <- as.data.frame(unique(x[,by.x]), stringsAsFactors=FALSE))
  mat[2,2] <- nrow(uy <- as.data.frame(unique(y[,by.y]), stringsAsFactors=FALSE))
  names(ux) <- by.x
  names(uy) <- by.x
  mat[2,3] <- nrow(as.data.frame(unique(rbind(ux,uy))))
  ri <- 3
  
  
  merge_type <- ""
  not.one.to.one <- FALSE
  if(mat[2,1] < mat[1,1]) {
    lst$merge.type <- c("many:")
    merge_type <- "many:"
    not.one.to.one <- TRUE
  } else {
    lst$merge.type <- c("one:")
    merge_type <- "one:"
  }
  
  if(mat[2,2] < mat[1,2]) {
    lst$merge.type <- c(lst$merge.type,"many")
    merge_type <- paste0(merge_type, "many")
    not.one.to.one <- TRUE
  } else {
    lst$merge.type <- c(lst$merge.type,"one")
    merge_type <- paste0(merge_type, "one")
  }
  
  # make output
  v1 <- v1[!v1%in%by]
  col <- v1[v1%in%v2]
  if(length(col)>0) {
     if(verbose) {
       cat("Variables in both ", x.arg.name," and ", y.arg.name,":\n")
     }
     lst <- c(lst, list(inBoth=col))
     if(verbose) {
       print(col)
     }
  }
  
  if(verbose) {
    cat("Merge type is ",merge_type,"\n")
  }
  # clear off the rn.x and rn.y variables. merge will add the proper variables
  if(!is.null(rn.x)) {
    x[,rn.x] <- NULL
    if(num.x) {
      by.x <- by.x.original
    }
  }
  if(!is.null(rn.y)) {
    y[,rn.y] <- NULL
    if(num.y) {
      by.y <- by.y.original
    }
  }

  # do the merge
  if(fast) {
    mg <- as.data.frame(merge(as.data.table(x),
                              as.data.table(y),
                              by.x=by.x,
                              by.y=by.y,
                              all.x=all.x,
                              all.y=all.y,
                              ...))
  } else {
    mg <- merge(x,y, by.x=by.x, by.y=by.y, all.x=all.x, all.y=all.y, ...)
  }
  if(nrow(mg) == 0) {
    warning("No rows on resulting data.frame.")
    mat[3,1] <- 0 
    mat[3,2] <- 0 
    mat[3,3] <- 0
    mat[4,1] <- 0
    mat[4,2] <- 0
    mat[4,3] <- 0
    mg[,tempx] <- NULL
    mg[,tempy] <- NULL
    if(verbose) {
      print(mat)
    }
    if(return.list) {
      lst$merge.matrix <- mat
      return(list(data=mg, list=lst))
    }
    
    return(mg)
  }
  if(order %in% c("x", "y")){
    mg[is.na(mg[,order.var]),] <- Inf
    mg <- mg[order(mg[,order.var]),]
    mg[,order.var] <- NULL
  }
  mat[3,1] <- nrow(mg[!is.na(mg[,tempx]),]) 
  mat[3,2] <- nrow(mg[!is.na(mg[,tempy]),]) 
  mat[3,3] <- nrow(mg)

  flag <- FALSE
  if(is.null(merge.type.colname)) {
    flag <- TRUE
    merge.type.colname <- genVarNames("merge",mg)
  }

  if(nrow(mg) != 0) {
    mg[,merge.type.colname] <- NA
    types <- c('x only', 'y only', 'matched')
    mg[,merge.type.colname][is.na(mg[,tempx])] <- 2
    mg[,merge.type.colname][is.na(mg[,tempy])] <- 1
    mg[,merge.type.colname][is.na(mg[,merge.type.colname])] <- 3
    mg[,merge.type.colname] <- lfactor(mg[,merge.type.colname], levels = 1:3, labels=types)
    if(verbose) {
      print(table(mg[,merge.type.colname]))
    }
    if(return.list) {
        lst$merge.type.table <- table(mg[,merge.type.colname])
    }

  }
  
  # these lines should be unnecessary
  #if(!all.y) {
  #  mg <- subset(mg, mg[,merge.type.colname]!="y only")
  #}

  #if(!all.x) {
  #  mg <- subset(mg, mg[,merge.type.colname]!="x only")
  #}

  # bug, this needs to use merge_type
  # This is by.x because by.y might not be defined for mg
  mat[4,1] <- nrow(as.data.frame(unique(mg[mg[,tempx],by.x])))
  mat[4,2] <- nrow(as.data.frame(unique(mg[mg[,tempy],by.x])))
  mat[4,3] <- nrow(as.data.frame(unique(mg[,by.x])))
  
  mg[,tempx] <- NULL
  mg[,tempy] <- NULL

  lst$merge.matrix <- mat

  if(verbose) {
    print(mat)
  }
  
  if(flag) {
    mg[,merge.type.colname] <- NULL
  }
  # return
  if(return.list) {
    return(list(data=mg, list=lst))
  }
  mg
}

if(FALSE) {
  authors <- data.frame(
    surname = I(c("Tukey", "Venables", "Tierney", "Ripley", "McNeil")),
    nationality = c("US", "Australia", "US", "UK", "Australia"),
    deceased = c("yes", rep("no", 4)))
  books <- data.frame(
    name = I(c("Tukey", "Venables", "Tierney",
               "Ripley", "Ripley", "McNeil", "R Core")),
    title = c("Exploratory Data Analysis",
              "Modern Applied Statistics ...",
              "LISP-STAT",
              "Spatial Statistics", "Stochastic Simulation",
              "Interactive Data Analysis",
              "An Introduction to R"),
    other.author = c(NA, "Ripley", NA, NA, NA, NA,
                     "Venables & Smith"))

  m1 <- mergev(authors, books,fast=F, by.x = "surname", by.y = "name", sort=TRUE)
}
