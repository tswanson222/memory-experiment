##### Import local JSON data files
if(!require(jsonlite)){install.packages("jsonlite", dep = TRUE)} # v1.6
getJSONS <- function(path = ".", files = c("one", "two", "three")){
  path <- ifelse(gsub("/$", "", path) == "", ".", gsub("/$", "", path))
  j <- which(paste0(files, ".json") %in% dir(path))
  stopifnot(as.logical(length(j)))
  out <- lapply(j, function(z) readLines(paste0(path, "/", files, ".json")[z]))
  if(length(out) == 1){out <- out[[1]]}
  return(out)
}

# convert jsonDat into a dataframe or list of dataframes
getData <- function(jsonDat, matchPs = FALSE, id = FALSE){
  dataExtract <- function(jsonDat, id = FALSE){
    dat <- lapply(jsonDat, function(z){
      out <- jsonlite::fromJSON(z, flatten = T)[[(id * -1) + 2]]
      out <- as.list(out)
      out <- data.frame(do.call(cbind, out))
      return(out)
    })
    if(!id){
      for(j in 1:length(dat)){
        setups <- grep("setup", unlist(dat[[j]]$stimulus))
        sorts <- grep("sort_trial", unlist(dat[[j]]$stimulus))
        time_sequence <- finalLocations <- rep(NA, nrow(dat[[j]]))
        for(i in seq_along(setups)){
          time_sequence[setups[i]] <- paste0(dat[[j]][, "time_sequence"][[setups[i]]], collapse = ",")
          finalLocations[sorts[i]] <- paste0(dat[[j]][, "finalLocations"][[sorts[i]]], collapse = ",")
        }
        dat[[j]]$time_sequence <- as.list(time_sequence)
        dat[[j]]$finalLocations <- as.list(finalLocations)
        dat[[j]] <- data.frame(apply(dat[[j]], 2, unlist))
      }
    } else {
      dat <- unname(unlist(dat))
    }
    if(length(dat) == 1){dat <- dat[[1]]}
    return(dat)
  }
  if(is.list(jsonDat) & length(jsonDat) == 1){jsonDat <- jsonDat[[1]]}
  if(is.character(jsonDat)){
    dat <- lapply(jsonDat, dataExtract, id = id)
    if(id){dat <- unlist(dat)}
  } else if(all(sapply(jsonDat, class) == "character")){
    dat <- lapply(seq_along(jsonDat), function(z){
      out <- lapply(jsonDat[[z]], dataExtract, id = id)
      if(length(out) == 1){out <- out[[1]]}
      if(id){out <- unlist(out)}
      return(out)
    })
  }
  if(length(dat) == 1){dat <- dat[[1]]}
  if(is.null(dim(dat))){
    names(dat) <- paste0("dat", seq_along(dat))
    if(matchPs){
      n <- length(dat)
      if(n == 3){
        ids <- lapply(dat, function(z) sapply(z, getTurkID))
        ids2 <- data.frame(table(unlist(ids)))
        ids3 <- as.character(ids2[which(ids2$Freq == 3), 1])
        for(i in 1:n){
          dat[[i]] <- dat[[i]][which(ids[[i]] %in% ids3)]
          ids[[i]] <- ids[[i]][which(ids[[i]] %in% ids3)]
        }
        dat[[n]] <- dat[[n]][order(match(ids[[n]], ids[[1]]))]
        dat[[n - 1]] <- dat[[n - 1]][order(match(ids[[n - 1]], ids[[1]]))]
        dat <- lapply(dat, function(z){
          names(z) <- paste0("p", seq_along(z))
          return(z)
        })
      } else {
        warning('Cannot matchPs without all 3 datasets')
      }
    }
  }
  return(dat)
}

# Pull MTurk worker ID or attention check response
getTurkID <- function(x, var = c("workerID", "attn_check")){
  var <- match.arg(var)
  id <- which(x$stimulus == var)
  id <- ifelse(is.list(x$responses), x$responses[[id]], x$responses[id])
  id <- gsub('\"|[{]|[}]|.*:', "", id)
  if(var == "attn_check"){id <- id == unique(x$attn_word)}
  return(id)
}

# Get response matrix for all trials in a session (only final_locations currently)
getSorts <- function(dat, var = "final_locations"){
  var <- match.arg(var, c("final_locations", "init_locations"))
  stimulus <- ifelse(var == "final_locations", "sort_trial", "setup")
  sortData <- function(n, dat, var = c("final_locations", "init_locations")){
    var <- match.arg(var)
    xx <- dat[[var]][n]
    xx <- gsub('\"|\\[|\\]|\\{|\\}', "", xx)
    xx <- as.numeric(gsub(".*:", "", strsplit(xx, ",")[[1]]))
    xxx <- data.frame(matrix(NA, ncol = 3, nrow = length(xx)/3))
    for(i in 1:3){xxx[, i] <- xx[seq(i, length(xx), by = 3)]}
    colnames(xxx) <- c("cat", "src", "loc")
    xxx <- xxx + 1
    xxx$room <- ifelse(xxx$loc <= 3, 1, ifelse(xxx$loc <= 6, 2, 3))
    xxx$time <- 1:9
    colnames(xxx) <- paste0(colnames(xxx), '1')
    return(xxx)
  }
  out <- lapply(grep(stimulus, dat$stimulus), sortData, dat, var)
  correct <- as.numeric(dat[grep("sort_trial", dat$stimulus), "sort_correct"])
  condition <- as.numeric(dat[grep("setup", dat$stimulus), "condition"])
  ps <- list(trial = 1:length(out), condition = condition, correct = correct)
  ps <- do.call(cbind, lapply(ps, rep, each = unique(sapply(out, nrow))))
  out <- cbind.data.frame(ps, do.call(rbind, out))
  return(out)
}

# Get initial locations for stimuli
getInits <- function(dat, rmPractice = TRUE){
  gnum <- function(x, numbers = TRUE){
    x <- gsub(ifelse(numbers, '[^0-9]', '[0-9]'), '', x)
    if(numbers){x <- as.numeric(x)}
    return(x)
  }
  kk <- which(!is.na(dat$animation_sequence))
  kk <- strsplit(dat$animation_sequence[kk], ',[{]')
  if(length(kk) == 32 & isTRUE(rmPractice)){kk <- kk[-(1:2)]}
  kk <- lapply(kk, function(z){
    z1 <- setNames(data.frame(do.call(rbind, strsplit(z, ','))), c('cat', 'src', 'loc'))
    for(i in 1:3){z1[, i] <- gnum(z1[, i]) + 1}
    z1$room <- ifelse(z1$loc <= 3, 1, ifelse(z1$loc <= 6, 2, 3))
    z1$time <- 1:9
    return(z1)
  })
  kk <- data.frame(do.call(rbind, kk))
  colnames(kk) <- paste0(colnames(kk), '0')
  return(kk)
}

# Get data frame(s) of proportion correct per sort trial
getCorrect <- function(dat){
  if(all(sapply(dat, class) != "list")){dat <- list(dat)}
  out <- lapply(dat, function(z){
    data.frame(do.call(cbind, lapply(z, function(zz){
      as.numeric(na.omit(zz$sort_correct))/9}))
  )})
  if(length(out) == 1){out <- out[[1]]}
  return(out)
}

