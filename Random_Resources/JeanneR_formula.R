# Title: Jeanne R Formula
# 
# Author:
# Date created: Mon May 21 13:50:55 2018
# -------------
# Last modified:
#
#
# Remarks:
#
#
#

#import relevant libraries
library(tidyverse)
library(foreign)
library(rgdal)
library(rgeos)
library(sp)

## Function A: printf --> print object into quote
printf <- function(...) print(sprintf(...));

## Function B: from.df.to.list --> from data frame to list
from.df.to.list <- function(df) as.list(as.data.frame(t(df)));

## Function C: read.xlsx.no.factor --> read xlsx file without factor class
read.xlsx.no.factor <- function(file, ...) {
  
  outp <- read.xlsx(file, ...);
  for (v in colnames(outp)) {
    if (class(outp[,v])=="factor") {
      outp[,v] <- as.character(outp[,v]);
    }
  }
  return(outp);
  
}

## Function D: merge.recursive --> recursively merge all data  previously listed
merge.recursive <- function(data.list, by) {
  
  stopifnot(by %in% colnames(data.list[[1]]));
  
  first.dataset <- data.list[[1]];
  
  if (length(data.list) == 1) {
    return(data.list) 
  } else if (length(data.list) == 2) {
    return(merge(data.list[[1]], data.list[[2]], by=by));
  } else {
    data.list[[1]] <- NULL;
    return(merge(first.dataset, merge.recursive(data.list, by=by), by=by));
  } 
  
}


## Function E: rbind.recursive --> recursively append data
rbind.recursive <- function(data.list) {
  
  stopifnot(length(data.list)>0);
  
  first.dataset <- data.list[[1]];
  
  if (length(data.list) == 1) {
    return(data.list) 
  } else if (length(data.list) == 2) {
    return(rbind.fill(data.list[[1]], data.list[[2]]));
  } else {
    data.list[[1]] <- NULL;
    return(rbind.fill(first.dataset, rbind.recursive(data.list)));
  } 
  
}

## Function F: recursively sum variables
sum.recursive <- function(dta, sum.var, list.vars) {
  
  stopifnot(length(list.vars)>0);
  
  first.var <- list.vars[[1]];
  
  if (!(sum.var %in% names(dta))) {
    dta[sum.var] <- 0;
  }
  
  dta[, sum.var] <- dta[, sum.var] + dta[, first.var];
  
  if (length(list.vars)==1) {
    return(dta);
  } else {
    list.vars <- list.vars[2:length(list.vars)];
    return(sum.recursive(dta,sum.var,list.vars));
  }
  
}


## Function G: remove all factors from data frame
df.remove.factors <- function(x){
  for (v in names(x)) {
    if (class(x[,v])=="factor") {
      x[,v] <- as.character(x[,v])
    }
  }
  x
}

## Function H: transform characters into factors in data frame
df.make.factors <- function(x){
  for (v in names(x)) {
    if (class(x[,v])=="factor") {
      x[,v] <- factor(as.character(x[,v]))
    }
  }
  x
}


# Function Hb: transform numeric into integrals in dataframe
df.make.int <- function(x){
  for (v in names(x)) {
    if (class(x[,v])=="numeric") {
      x[,v] <- as.integer(x[,v])
    }
  }
  x
}


## Function I: transform numeric into characters in data frame
df.fix.ffdf <- function(x){
  for (v in names(x)) {
    if (class(x[,v])=="numeric") {
      x[,v] <- factor(as.character(x[,v]))
    }
  }
  x
}

## Function J: get all columns' classes
get.colclasses <- function(d) 
  return(unlist(lapply( names(d), function(v) class(d[,v]))));

## Function K: find out which objects are characters or factors
filter.characters.factors <- function(l) 
  unlist(lapply(l, function(x) ifelse(x=="character", "factor", x)));

## Function L: compact labels on classes
make.compact.classes <- function(list.types) 
  paste(revalue(list.types,
                c("character"="c",
                  "integer"="i",
                  "numeric"="d")), collapse= "");

## Function M: change CRS towards albers.
albers.crs <- CRS(paste("+proj=aea +lat_1=50 +lat_2=58.5",
                        "+lat_0=45 +lon_0=-126",
                        "+x_0=1000000 +y_0=0",
                        "+ellps=GRS80 +datum=NAD83 +units=m +no_defs",
                        sep=""));

## Function N: check that data frame contains certain variables
check.names <- function(dta, vars)
  sum(!(vars %in% names(dta)))

## Function P: Make sure fractions not absurb
winsorize <- function (x, fraction=.01, ...)
{
  if(length(fraction) != 1 || fraction < 0 ||
     fraction > 0.5) {
    stop("bad value for 'fraction'")
  }
  lim <- quantile(x, probs=c(fraction, 1-fraction), ...)
  x[ x < lim[1] ] <- lim[1]
  x[ x > lim[2] ] <- lim[2]
  x
}

## Function Q: some sapply specification
qapply <- function(...) sapply(..., USE.NAMES = TRUE, simplify = FALSE)

## Function R: simplified way of reading layers
quick.readOGR <- function(fn) 
  readOGR(fn, layer = ogrListLayers(fn)[1])

## Function R: ??
st_over <- function(x, y) {
  purrr::map_int(
    .x = sf::st_intersects(x = x, y = y),
    .f = function(z) {
      if (length(z) == 0) {
        NA_integer_
      } else {
        z[1]
      }
    }
  )
}
# Example:
# dhs_clusts$id <- fishnet_sf$id[st_over(x = dhs_clusts, y = fishnet_sf)]

## Function S: summarize regression statistiques
summarize_fstats <- function(list) {
  
  names(list) <- sapply(list, function(x) x$lhs)
  
  out <- purrr::map_dbl(
    .x = names(list),
    .f = function(x) {
      condfstat(list[[x]], quantiles = c(0.05, 0.95))
    }
  )
  
  names(out) <- sapply(list, function(x) x$lhs)
  
  out
  
}


## Function T: summarize models
summarize_models <- function(list, coef, model) {
  
  names(list) <- sapply(list, function(x) x$lhs)
  
  if (all(names(list)[1] == names(list))) {
    nms <- 1:length(list)
  } else {
    nms <- names(list)
    names(nms) <- nms
  }
  
  out <- purrr::map_dfr(
    .x = nms,
    .f = function(i) {
      
      message(paste(model, "-", i))
      
      x <- list[[i]]
      
      controls <- attr(x$terms, "term.labels") %>%
        paste(collapse = ", ")
      
      fe <- x$fe %>%
        purrr::map_chr(attr, which = "fnam") %>%
        paste(collapse = ", ")
      fe[fe == ""] <- "none"
      
      if ("clustervar" %in% names(x)) {
        cluster <- names(x$clustervar) %>%
          paste(collapse = ", ")
      } else {
        cluster <- "none"
      }
      
      dplyr::bind_cols(
        broom::tidy(x) %>%
          dplyr::filter(term == coef),
        broom::glance(x)
      ) %>%
        dplyr::mutate(
          term = gsub(x = coef, pattern = "`|\\(fit\\)`", replacement = ""),
          model = model,
          controls = controls,
          fe = fe,
          cluster = cluster,
          sig = cut(
            x = p.value,
            breaks = c(0, 0.001, 0.01, 0.05, 1),
            labels = c("***", "**", "*", ""),
            include.lowest = TRUE
          )
        )
    },
    .id = "y"
  ) %>%
    dplyr::select(
      model,
      y, x = term, coef = estimate, se = std.error,
      stat = statistic, p = p.value, sig, adjr2 = adj.r.squared, df,
      controls, fe, cluster
    )
  
  if (identical(1:length(list), nms)) {
    out %>%
      dplyr::mutate(y = names(list)[1])
  } else {
    out
  }
  
}


