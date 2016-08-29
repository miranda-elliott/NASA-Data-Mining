#####################################################################################################
# Mining Altitude Data from Flight Incidence Reports
# Author: Miranda Elliott
# Date: 05/04/2016
#####################################################################################################


#####################################################################################################
# FUNCTIONS 
#####################################################################################################

### extract potential altitude values from input narrative ###
get_altitudes <- function(input) {
	
	n <- length(input)
	result <- rep(list(), n)
	
	for (i in 1:n) {
		
		# clean up input
		line <- input[i]
		line <- gsub(',', '', line)
		line <- iconv(line, 'latin1', 'ASCII', sub='')
		line <- tolower(line)
		
		# initialize altitude match list
		match <- list()
		
		# search for altitude values if non-empty narrative
		if (trimws(line) != '') {
			
			# check for feet measurement strings
			match.ft <- list()
			m <- regmatches(line, gregexpr('[0-9]+ ?feet', line))
			match.ft <- append(match.ft, m)
			m <- regmatches(line, gregexpr('[0-9]+ ?ft', line))
			match.ft <- append(match.ft, m)
			
			# extract feet values from strings 
			# add to match list
			match.ft <- unlist(match.ft)
			if (length(match.ft) > 0) {
				match.ft <- as.numeric(unique(unlist(regmatches(match.ft, gregexpr("[0-9]+", match.ft)))))
				match <- append(match, match.ft)
				match <- unlist(match)
			}
						
			# check for flight level measurement strings
			match.fl <- list()
			m <- regmatches(line, gregexpr('fl ?[0-9]+', line))
			match.fl <- append(match.fl, m)
			
			# extract flight level values from strings
			# convert flight level values to feet values
			# add to match list
			match.fl <- unlist(match.fl)
			if (length(match.fl) > 0) {
				match.fl <- as.numeric(unique(unlist(regmatches(match.fl, gregexpr("[0-9]+", match.fl)))))
				for (k in 1:length(match.fl)) {
					val <- match.fl[k] * 100
					match <- append(match, val)
				}
				match <- unlist(match)
			}
			
			# check for sentences with indicating words
			sentences <- strsplit(line, split='[\\.!?]+')[[1]]
			match.s <- list()
			for (k in 1:length(sentences)) {
				sentence <- sentences[k]
				match.s[[k]] <- list()
				# search for numeric values in sentences containing 'climb'
				if (grepl('climb', sentence)) {
					m <- regmatches(sentence, gregexpr(" [0-9]+", sentence))
					match.s[[k]] <- append(match.s[[k]], m)
				}
				# search for numeric values in sentences containing 'passing'
				if (grepl('passing', sentence)) {
					m <- regmatches(sentence, gregexpr(" [0-9]+", sentence))
					match.s[[k]] <- append(match.s[[k]], m)
				}
			}
			
			# extract numeric values from strings
			# add to match list
			match.s <- unlist(match.s)
			if (length(match.s) > 0) {
				match.s <- as.numeric(unique(unlist(match.s)))
				match <- append(match, match.s)
				match <- unlist(match)
			}
		}
		
		# add matches to result list
		match <- unique(unlist(match))
		match <- match
		if (length(match) > 0) {
			result[[i]] <- match[match > 100]
		}
	}
	
	# return potential altitude matches 
	return(result)
}

### combine two numeric lists of equal length by row ###
combine_lists <- function(list1, list2) {
	
	n <- length(list1)
	result <- rep(list(), n)
	
	for (i in 1:n) {
		result[[i]] <- list()
		result[[i]] <- append(result[[i]], list1[[i]])
		result[[i]] <- append(result[[i]], list2[[i]])
		result[[i]] <- sort(unique(unlist(result[[i]])), decreasing=FALSE)
	}
	
	return(result)
}

### return altitude value from list with highest frequency in text ###
get_most_frequent_altitude <- function(list, text) {
	
	# initialize frequency list 
	n <- length(list)
	freq <- rep(0, n)
	
	# clean up text 
	text <- gsub(',', '', text)
	text <- iconv(text, 'latin1', 'ASCII', sub='')
	text <- tolower(text)
	text <- strsplit(text, split=' ')[[1]]
	
	# convert flight level strings in text to feet measurements
	for (i in 1:length(text)) {
		if ((grepl('fl ?[0-9]+', text[i]))) {
			val <- as.numeric(regmatches(text[i], gregexpr("[0-9]+", text[i]))[[1]][1])
			val <- val * 100
			text[i] <- as.character(val)
		}
	}
	
	# count frequency of each altitude value in text
	for (i in 1:n) {
		term <- as.character(list[i])
		freq[i] <- length(grep(term, text))
	}
	
	# return value with highest frequency 
	result <- list[which.max(freq)]
	return(result)
}


#####################################################################################################
# ASRS DATA
#####################################################################################################

### import data ###
# import ASRS dataset
ASRS <- read.csv('ASRS_Jan_15-16.csv', header=TRUE)
# extract column I as altitude data 
ASRS.alt <- ASRS[,c(9)]
# extract columsn CN & CP as narrative data
ASRS.narr <- data.frame(ASRS[,c(92,94)])

### get predicted altitudes ###
# get predicted altitudes from narratives
matches.1 <- get_altitudes(ASRS.narr[,1])
matches.2 <- get_altitudes(ASRS.narr[,2])
altmatches <- combine_lists(matches.1, matches.2)

# get predicted altitude with highest frequency within narratives
n <- dim(ASRS)[1]
altmatches.mostfreq <- rep('', n)
for (i in 1:n) {
	list <- altmatches[[i]]
	if (length(list) > 1) {
		text <- paste(ASRS.narr[,1][i], ASRS.narr[,2][i], sep=' ')
		altmatches.mostfreq[i] <- get_most_frequent_altitude(list, text)
	} else if (length(list) == 1) {
		altmatches.mostfreq[i] <-  altmatches[[i]][1]
	}
}

### assess prediction accuracy ### 
# compare predicted altitudes from narrative columns to observed altitudes from altitude column
# key:
# 0: no values listed in altitude column and no values mined from narrative (no observed or predicted altitude)
# 1: no values listed in altitude column (no observed altitude)
# 2: no values mined from narrative, but values listed in altitude column (no predicted altitude)
# 3: no matches between values mined from narrative match and values listed in altitude column (incorrectly predicted altitude)
# 4: succesful match between values mined from narrative match and values listed in altitude column (correctly predicted altitude)
# 5: highest predicted altitude is observed altitude
n <- dim(ASRS)[1]
altmatches.bool <- rep(0, n)
for (i in 1:n) {
	obs <- trimws(ASRS.alt[i])
	pred.list <- altmatches[[i]]
	pred.mostfreq <- altmatches.mostfreq[i]
	noobs <- (trimws(ASRS.alt[i]) == '')
	nopred <- (length(altmatches[[i]]) == 0)
	
	if (noobs & nopred) {
		altmatches.bool[i] <- 0
	} else if (noobs) {
		altmatches.bool[i] <- 1
	} else if (nopred) {
		altmatches.bool[i] <- 2
	} else {
		if (pred.mostfreq == obs) {
			altmatches.bool[i] <- 5
		} else {
			for (j in 1:length(pred.list)) {
				if (pred.list[j] == obs) {
					altmatches.bool[i] <- 4
				}
			}
			if (altmatches.bool[i] != 4) {
				altmatches.bool[i] <- 3
			}
		}
	}
}

# percent of data with no observed or predicted altitude
sum(altmatches.bool==0)/length(altmatches.bool)
# percent of data with no observed altitude (but with predicted altitude)
sum(altmatches.bool==1)/length(altmatches.bool)
# percent of data with no predicted altitude (but with observed altitude)
sum(altmatches.bool==2)/length(altmatches.bool)
# percent of data with all incorrectly predicted altitude
sum(altmatches.bool==3)/length(altmatches.bool)
# percent of data with atleast one correctly predicted altitude
(sum(altmatches.bool==4) + sum(altmatches.bool==5))/length(altmatches.bool)
# percent of data with correctly predicted altitude as highest frequency altitude
sum(altmatches.bool==5)/length(altmatches.bool)

# percent of data with predicted altitude
sum(altmatches.mostfreq != '')/length(altmatches)
# percent of predicted altitudes that are incorrect
sum(altmatches.bool==3)/sum(altmatches.mostfreq != '')
# percent of predicted altitudes that are correct
(sum(altmatches.bool==4) + sum(altmatches.bool==5))/sum(altmatches.mostfreq != '')
# percent of predicted altitudes that have the correct highest frequency prediction
sum(altmatches.bool==5)/(sum(altmatches.bool==4) + sum(altmatches.bool==5))

# investigate narratives that yielded correctly predicted altitudes 
# output to csv to find patterns 
int.4 <- which(altmatches.bool == 4)
int.5 <- which(altmatches.bool == 5)
altmatches.correct <- list()
for (i in 1:length(c(int.4, int.5))) {
	altmatches.correct[i] <- paste(altmatches[c(int.4,int.5)][[i]], collapse=', ')
}
matched <- data.frame(ASRS.narr[c(int.4,int.5),], as.character(altmatches.correct), as.character(altmatches.mostfreq[c(int.4,int.5)]), ASRS.alt[c(int.4,int.5)], (altmatches.bool[c(int.4,int.5)] == 5))
names(matched) <- c('Report1', 'Report2', 'PredAlt', 'HighFreqPredAlt', 'ObsAlt', 'HighFreqPredMatchesObsAlt')
write.csv(matched, 'ASRS_correct_pred_alt.csv')

# investigate narratives that yielded incorrectly predicted altitudes 
# check if narratives contained observed altitude 
int.3 <- which(altmatches.bool == 3)
n <- length(int.3)
contains <- rep(FALSE, n)
altmatches.incorrect <- list()
for (i in 1:n) {
	obs <- as.character(trimws(ASRS.alt[int.3[i]]))
	c1 <- grepl(obs, ASRS.narr[int.3[i],1])
	c2 <- grepl(obs, ASRS.narr[int.3[i],2])
	contains[i] <- ((c1 == TRUE) | (c2 == TRUE))
	
	altmatches.incorrect[i] <- paste(altmatches[int.3][[i]], collapse=', ')
}
# output to csv to find patterns 
unmatched <- data.frame(ASRS.narr[int.3,], as.character(altmatches.incorrect), ASRS.alt[int.3], contains)
names(unmatched) <- c('Report1', 'Report2', 'PredAlt', 'ObsAlt', 'NarrContainsObsAlt')
write.csv(unmatched, 'ASRS_incorrect_pred_alt.csv')

# investigate narratives that yielded no predicted altitudes
# check if narratives contained observed altitude 
int.2 <- which(altmatches.bool == 2)
n <- length(int.2)
contains <- rep(FALSE, n)
for (i in 1:n) {
	obs <- as.character(trimws(ASRS.alt[int.2[i]]))
	c1 <- grepl(obs, ASRS.narr[int.2[i],1])
	c2 <- grepl(obs, ASRS.narr[int.2[i],2])
	contains[i] <- ((c1 == TRUE) | (c2 == TRUE))
}
# output to csv to find patterns 
nomatches <- data.frame(ASRS.narr[int.2,], ASRS.alt[int.2], contains)
names(nomatches) <- c('Report1', 'Report2', 'ObsAlt', 'NarrContainsObsAlt')
write.csv(nomatches, 'ASRS_no_pred_alt.csv')

### output predictions ###
# output full dataset with predicted altitudes, boolean if atleast one predicted altitude matches observed altitude
altmatches.all <- list()
for (i in 1:length(altmatches)) {
	altmatches.all[i] <- paste(altmatches[[i]], collapse=', ')
}
allmatches <- data.frame(ASRS, as.character(altmatches.all), altmatches.mostfreq, ((altmatches.bool == 4) | (altmatches.bool == 5)), (altmatches.bool == 5))
names(allmatches) <- append(names(ASRS), c('PredAlt', 'HighFreqPredAlt', 'PredContainsObsAlt', 'HighFreqPredMatchesObsAlt'))
write.csv(allmatches, 'ASRS_with_predictions.csv')


#####################################################################################################
# NTSB DATA
#####################################################################################################

### import data ###
# import NTSB dataset
NTSB <- read.csv('NTSB_Ersin_Turbulence.csv', header=TRUE, fileEncoding='latin1')
# extract BL & BN columns as narrative data
NTSB.narr <- data.frame(NTSB[,c(64,66)])

### get predicted altitudes ###
# get predicted altitudes from narratives
matches.1 <- get_altitudes(NTSB.narr[,1])
matches.2 <- get_altitudes(NTSB.narr[,2])
altmatches <- combine_lists(matches.1, matches.2)

# get predicted altitude with highest frequency within narratives
n <- dim(NTSB)[1]
altmatches.mostfreq <- rep('', n)
for (i in 1:n) {
	list <- altmatches[[i]]
	if (length(list) > 1) {
		text <- paste(NTSB.narr[,1][i], NTSB.narr[,2][i], sep=' ')
		altmatches.mostfreq[i] <- get_most_frequent_altitude(list, text)
	} else if (length(list) == 1) {
		altmatches.mostfreq[i] <-  altmatches[[i]][1]
	}
}

# percent of data with predicted altitude
sum(altmatches.mostfreq != '')/length(altmatches)

### output predictions ###
# output full dataset with predicted altitudes
altmatches.all <- list()
for (i in 1:length(altmatches)) {
	altmatches.all[i] <- paste(altmatches[[i]], collapse=', ')
}
allmatches <- data.frame(NTSB, as.character(altmatches.all), altmatches.mostfreq)
names(allmatches) <- append(names(NTSB), c('PredAlt', 'HighFreqPredAlt'))
write.csv(allmatches, 'NTSB_with_predictions.csv')
