##### Original Battig Dataset
if(!require(WordPools)){install.packages("WordPools", dep = TRUE)} # v1.1-1
data(Battig) # 5231 words, 56 categories
data <- Battig

##### Functions to apply constraints in selecting word stimuli
makeWordList <- function(data, nw = 20, letters = 7, exclude = TRUE, 
                         duplicates = NULL, ...){
  if(class(exclude) %in% c("logical", "character")){
    excludePlus <- NULL
    if(is.character(exclude)){
      if(length(exclude) == 1 & any(grepl("[+]", exclude))){
        excludePlus <- gsub("[+]", "", exclude)
        exclude <- TRUE
      }
    }
    if(is.logical(exclude)){if(exclude){
      exclude <- c("males first name", "girls first name", "college or university",
                   "city", "state", "bldg for religious servic", "country",
                   "military title", "elective office", "member of the clergy",
                   "chemical element", "type of ship", "kind of money",
                   "type of dance", "kind of cloth", "snake", "part of speech")
    }}
    if(!is.null(excludePlus)){exclude <- c(exclude, excludePlus)}
    if(is.character(exclude)){
      data <- data[!data$catname %in% exclude, ]
      data$catname <- factor(data$catname)
      data$catnum <- factor(data$catnum)
      levels(data$catnum) <- 1:length(levels(data$catnum))
    }
  }
  restrict <- function(data, letters = NULL, hyphens = FALSE, 
                       spaces = FALSE, periods = FALSE){
    if(!hyphens){data <- data[!grepl("-", data$word), ]}
    if(!periods){data <- data[!grepl("[.]", data$word), ]}
    if(!spaces | !is.null(letters)){data <- data[!grepl(" ", data$word), ]}
    if(!is.null(letters)){data <- data[nchar(data$word) <= letters, ]}
    data
  }
  data <- restrict(data, letters = letters, ...)
  words <- lapply(seq_along(levels(data$catname)), function(z){
    data[data$catnum == levels(data$catnum)[z], ]})
  if(!is.null(duplicates)){
    for(i in seq_along(words)){
      if(any(duplicates %in% words[[i]]$word)){
        words[[i]] <- words[[i]][-which(words[[i]]$word %in% duplicates), ]
      }
    }
  }
  nn <- sapply(words, nrow)
  cats <- unique(as.character(data$catname))
  names(words) <- cats
  words0 <- words[nn >= nw]
  out <- data.frame(matrix(NA, ncol = length(words0), nrow = nw))
  for(i in seq_along(words0)){out[, i] <- words0[[i]][1:nw, "word"]}
  colnames(out) <- paste0("V", 1:ncol(out))
  return(out)
}

makeWordList2 <- function(data, ...){
  out <- makeWordList(data, ...)
  words <- unname(unlist(out))
  dups <- words[duplicated(words)]
  while(any(duplicated(words))){
    out <- makeWordList(data, ..., duplicates = dups)
    words <- unname(unlist(out))
    dups <- c(dups, words[duplicated(words)])
  }
  out
}

##### Get final word stimuli
out <- makeWordList2(data, nw = 20, letters = 7, exclude = TRUE)

### CONSTRAINTS
# 17 categories removed entirely
# no more than 7 letters in a word
# no spaces, hyphens, or periods
# no duplicate words

### STIMULI
# All categories that met these constraints for at least 20 words
# The top 20 most common words in each of these categories were selected
# Final dataset: 680 words, 34 categories

################################################################################
################### WRITE WORD STIMULI TO EXPERIMENT.HTML FILE #################
################################################################################
z <- paste0(lapply(out, function(zz) paste0(zz, collapse = ",")), collapse = ";")
htmlFile <- readLines("public/views/experiment.html")
htmlFile2 <- strsplit(htmlFile, " ")
a1 <- a2 <- c()
for(i in 1:length(htmlFile2)){
  a1[i] <- "\tvar" %in% htmlFile2[[i]]
  a2[i] <- "allWords" %in% htmlFile2[[i]]
}
htmlFile[ifelse(length(which(a1 & a2)) == 1, which(a1 & a2), stop("More than one"))] <- paste0("\tvar allWords = \"", z, "\";") 
cat(paste0(paste0(htmlFile, collapse = "\n"), "\n"), file = "public/views/experiment.html")
