rm(list = ls())
options(scipen = 99, digits = 5)
#######################
## calling libraries ##
#######################

library(data.table)
library(optparse)
library(parallel)
library(zoo)
library(gdata)
########################
## Generic Workhorses ##
########################

time.nw <- paste0(unlist(strsplit(as.character(Sys.time()), " ")), collapse="_")
cat(paste0("Execution started at time: ", time.nw, "\n"))
option_list = list(
    make_option(c("-d", "--data"),
                action="store",
                default="",
                type='character',
                help="Input data"),
    make_option(c("-i", "--identifier"),
                action="store",
                default="n",
                type='character',
                help="Read the file in xlsx or csv format"),
    make_option(c("-m", "--mes"),
                action="store",
                default=1,
                type='integer',
                help="Toggle for displaying messages"),
    make_option(c("-r", "--result"),
                action="store",
                default=".Rdata",
                type='character',
                help="Results should be in Rdata or csv format"),
    make_option(c("-p", "--path"),
                action="store",
                default=paste0("results",time.nw,".Rdata"),
                type='character',
                help="Path to results folder"),
    make_option(c("-c", "--cores"),
                action="store",
                default=1,
                type='integer',
                help="Number of system cores to use"),
    make_option(c("-w", "--weight"),
                action="store",
                default="3,3,4",
                type='character',
                help="Weights assigned to trainer feedback, peer feedback and knowledge sufficiency"))

opt <- parse_args(OptionParser(option_list = option_list))

dp <- opt$d
ft  <- opt$i
m  <- opt$m
rf <- opt$r
p  <- opt$p
c  <- opt$c
w  <- opt$w

## dp <- "/home/gary/Work/AWARATHON/SRC/DATA/FRG_DEMO"
## ft <- "xls"
## m <- 1
## rf <- "csv"
## p <- "/home/gary/Work/AWARATHON/SRC/RESULTS/FRG_DEMO"
## c <- 1
## w <- "3,3,4"


fl <- list.files(dp)
df <- tolower(fl)

print(fl)

wgt <- as.numeric(unlist(strsplit(w, ",")))

if(m == 1) {
    cat("Checking folder integrity\n")
}

print(dp)

feedback.pre <- fl[intersect(grep("feedback", df), grep("pre", df))]
feedback.post <- fl[intersect(grep("feedback", df), grep("post", df))]
knowledge.pre <- fl[intersect(grep("knowledge", df), grep("pre", df))]
knowledge.post <- fl[intersect(grep("knowledge", df), grep("post", df))]
trainer.pre <- fl[intersect(grep("trainer", df), grep("pre", df))]
trainer.post <- fl[intersect(grep("trainer", df), grep("post", df))]

dff <- list.files(dp,full.names = TRUE)

if(m == 1) {
    cat("Calculating the pre knowledge scores\n")
}


                                        # pre knowledge module
k.pre <- paste0(dp, "/", knowledge.pre)

if(ft == "xlsx" || ft == "xls") {
    dat <- read.xls(k.pre)
    dat <- data.table(dat)
} else if(ft == "csv") {
    dat <- fread(k.pre)
    dat <- data.table(dat)       
}

colnames(dat) <- c("id",
                   "name",
                   "email",
                   "mobile",
                   "question_id",
                   "question",
                   "correct_answer",
                   "user_answer",
                   "module_title",
                   "play_start",
                   "play_end",
                   "play_time",
                   "response")

dat <- dat[order(dat$id), ]
dat$question_person <- paste0(dat$id, sep = "-", dat$question_id)
dat <- dat[!duplicated(dat$question_person), ]
dat$question_person <- NULL

dat.sc <- dat[order(dat$id), c("id", "name", "response"), with = FALSE]
us.pre <- data.table(id = dat.sc$id[!duplicated(dat.sc$id)],
                     name = dat.sc$name[!duplicated(dat.sc$name)],
                     total = tapply(dat.sc$response,
                                    dat.sc$id,
                                    length),
                     pre_score = tapply(dat.sc$response,
                                        dat.sc$id,
                                         function(x) length(which(x
                                                                  == "Correct"))))

                                        # knowledge post

if(m == 1) {
    cat("Calculating the post knowledge scores\n")
}

k.post <- paste0(dp, "/", knowledge.post)

if(ft == "xlsx" || ft == "xls") {
    dat <- read.xls(k.post)
    dat <- data.table(dat)
} else if(ft == "csv") {
    dat <- fread(k.post)
    dat <- data.table(dat)       
}

colnames(dat) <- c("id",
                   "name",
                   "email",
                   "mobile",
                   "question_id",
                   "question",
                   "correct_answer",
                   "user_answer",
                   "module_title",
                   "play_start",
                   "play_end",
                   "play_time",
                   "response")

dat <- dat[order(dat$id, dat$question_id), ]
dat$question_person <- paste0(dat$id, sep = "-", dat$question_id)
dat <- dat[!duplicated(dat$question_person), ]
dat$question_person <- NULL

dat.sc <- dat[order(dat$id), c("id", "name",
                               "response"), with = FALSE]
us.post <- data.table(id = dat.sc$id[!duplicated(dat.sc$id)],
                      name = dat.sc$name[!duplicated(dat.sc$name)],
                      total = tapply(dat.sc$response,
                                     dat.sc$id,
                                     length),
                      post_score = tapply(dat.sc$response,
                                          dat.sc$id,
                                          function(x) length(which(x
                                                                   == "Correct"))))



## us.post$knowledge <-us.post$post_score/us.post$total  
## us.post$rank <- rank(-us.post$knowledge,ties.method="first")

                                        # Post feedback module

if(m == 1) {
    cat("Calculating the post feedback module scores\n")
}


f.post <- paste0(dp, "/", feedback.post)

if(ft == "xlsx" || ft == "xls") {
    dat <- read.xls(f.post)
    dat <- data.table(dat)
} else if(ft == "csv") {
    dat <- fread(f.post)
    dat <- data.table(dat)       
}

colnames(dat) <- c("id",
                   "name",
                   "email",
                   "mobile",
                   "module_title",
                   "module_name",
                   "question",
                   "option",
                   "play_start",
                   "play_end",
                   "play_time",
                   "response")


dat <- dat[order(dat$id, dat$question), ]
dat$question_person <- paste0(dat$id, sep = "-", dat$question)
dat <- dat[!duplicated(dat$question_person), ]
dat$question_person <- NULL


skill <- dat[, c("id", "name", "question", "option"), with = FALSE]
skill$option <- tolower(skill$option)
skill$feedback_option <- NA
skill$feedback_option[which(skill$option == "excellent")
                      ] <- 4
skill$feedback_option[which(skill$option == "very good")
                      ] <- 3
skill$feedback_option[which(skill$option == "good")
                      ] <- 2
skill$feedback_option[which(skill$option == "average")
                      ] <- 1
skill$feedback_option[which(skill$option == "strongly agree")
                      ] <- 4
skill$feedback_option[which(skill$option == "agree")
                      ] <- 3
skill$feedback_option[which(skill$option == "disagree")
                      ] <- 2
skill$feedback_option[which(skill$option == "strongly disagree")
                      ] <- 1

skill <- skill[!is.na(skill$feedback_option), ]
skill <- skill[order(skill$id), ]

skill$name <- as.character(skill$name)
skill$question <- as.character(skill$question)


units <- unlist(strsplit(skill$name, " "))
units <- units[units != "Player"]
codes <- units[which(nchar(units) == 1)]
if(length(codes) == nrow(skill)) {
    skill$codes <- codes
}

skill$receiver <- substr(sapply(skill$question, function(x) {
    if(length(grep("Participant", unlist(strsplit(x, " ")))) > 0) {
        
        return(unlist(strsplit(x, " "))[grep("Participant",
                                             unlist(strsplit(x, " "))) + 1])
    } else {
        stop("Keyword 'Participant' not found. Please check the data.")
    }
}), 1, 1)

r <- sort(unique(skill$codes))
c <- sort(unique(skill$receiver))

skill <- skill[skill$codes != skill$receiver, ]
skill <- skill[order(skill$code), ]

st <- seq_along(skill$id)[!duplicated(skill$id)]
en <- seq_along(skill$id)[!duplicated(skill$id, fromLast = TRUE)]
inx <- paste0(sep = "c(", st, sep = ":", en, sep = ")")



matrix.post <- do.call('rbind', lapply(inx, function(i){
    d <- skill[eval(parse(text = i)), ]
    scores <- d[, c("receiver", "feedback_option"), with = FALSE]
    scores <- scores[order(scores$receiver), ]
    scores <- data.table(receiver =
                             names(tapply(scores$feedback_option,
                                          scores$receiver, mean)),
                         mean_score = tapply(scores$feedback_option,
                                             scores$receiver, mean)
                         )
    ref <- data.table(receiver = c)
    ref <- merge(ref, scores, by = c("receiver"), all.x = TRUE)
    ref <- ref[order(ref$receiver), ]
    return(ref$mean_score)
}))

rownames(matrix.post) <- r
colnames(matrix.post) <- 

    if(m == 1) {
    cat("Calculating the post feedback peer weights\n")
}


weight.val <- apply(matrix.post, 2, function(x) {
    x <- x[!is.na(x)]
    return(mean(x))
})

names(weight.val) <- c

weight <- data.table(codes = c,
                     w = weight.val)


if(m == 1) {
    cat("Calculating the pre feedback scores\n")
}


                                        # pre feedback

f.pre <- paste0(dp, "/", feedback.pre)

if(ft == "xlsx" || ft == "xls") {
    dat <- read.xls(f.pre)
    dat <- data.table(dat)
} else if(ft == "csv") {
    dat <- fread(f.pre)
    dat <- data.table(dat)       
}

colnames(dat) <- c("id",
                   "name",
                   "email",
                   "mobile",
                   "module_title",
                   "module_name",
                   "question",
                   "option",
                   "play_start",
                   "play_end",
                   "play_time",
                   "response")


dat <- dat[order(dat$id, dat$question), ]
dat$question_person <- paste0(dat$id, sep = "-", dat$question)
dat <- dat[!duplicated(dat$question_person), ]
dat$question_person <- NULL


skill <- dat[, c("id", "name", "question", "option"), with = FALSE]
skill$option <- tolower(skill$option)
skill$feedback_option <- NA
skill$feedback_option[which(skill$option == "excellent")
                      ] <- 4
skill$feedback_option[which(skill$option == "very good")
                      ] <- 3
skill$feedback_option[which(skill$option == "good")
                      ] <- 2
skill$feedback_option[which(skill$option == "average")
                      ] <- 1
skill$feedback_option[which(skill$option == "strongly agree")
                      ] <- 4
skill$feedback_option[which(skill$option == "agree")
                      ] <- 3
skill$feedback_option[which(skill$option == "disagree")
                      ] <- 2
skill$feedback_option[which(skill$option == "strongly disagree")
                      ] <- 1

skill <- skill[!is.na(skill$feedback_option), ]
skill <- skill[order(skill$id), ]

skill$name <- as.character(skill$name)
skill$question <- as.character(skill$question)


units <- unlist(strsplit(skill$name, " "))
units <- units[units != "Player"]
codes <- units[which(nchar(units) == 1)]
if(length(codes) == nrow(skill)) {
    skill$codes <- codes
}


skill$receiver <- substr(sapply(skill$question, function(x) {
    if(length(grep("Participant", unlist(strsplit(x, " ")))) > 0) {
        
        return(unlist(strsplit(x, " "))[grep("Participant",
                                             unlist(strsplit(x, " "))) + 1])
    } else {
        stop("Keyword 'Participant' not found. Please check the data.")
    }
}), 1, 1)

r <- sort(unique(skill$codes))
c <- sort(unique(skill$receiver))

skill <- skill[skill$codes != skill$receiver, ]
skill <- skill[order(skill$code), ]

st <- seq_along(skill$id)[!duplicated(skill$id)]
en <- seq_along(skill$id)[!duplicated(skill$id, fromLast = TRUE)]
inx <- paste0(sep = "c(", st, sep = ":", en, sep = ")")



matrix.pre <- do.call('rbind', lapply(inx, function(i){
    d <- skill[eval(parse(text = i)), ]
    scores <- d[, c("receiver", "feedback_option"), with = FALSE]
    scores <- scores[order(scores$receiver), ]
    scores <- data.table(receiver =
                             names(tapply(scores$feedback_option,
                                          scores$receiver, mean)),
                         mean_score = tapply(scores$feedback_option,
                                             scores$receiver, mean)
                         )
    ref <- data.table(receiver = c)
    ref <- merge(ref, scores, by = c("receiver"), all.x = TRUE)
    ref <- ref[order(ref$receiver), ]
    return(ref$mean_score)
}))

rownames(matrix.pre) <- r
colnames(matrix.pre) <- c


if(m == 1) {
    cat("Calculating the pre trainer feedback scores\n")
}


t.pre <- paste0(dp, "/", trainer.pre)

if(ft == "xlsx" || ft == "xls") {
    dat <- read.xls(t.pre)
    dat <- data.table(dat)
} else if(ft == "csv") {
    dat <- fread(t.pre)
    dat <- data.table(dat)       
}

colnames(dat) <- c("id", "name", "trainer_score")
ts.pre <- dat
ts.pre$trainer_scale <- ts.pre$trainer_score*4/10

if(m == 1) {
    cat("Calculating the post trainer feedback scores\n")
}


t.post <- paste0(dp, "/", trainer.post)

if(ft == "xlsx" || ft == "xls") {
    dat <- read.xls(t.post)
    dat <- data.table(dat)
} else if(ft == "csv") {
    dat <- fread(t.post)
    dat <- data.table(dat)       
}

colnames(dat) <- c("id", "name", "trainer_score")
ts.post <- dat
ts.post$trainer_scale <- ts.post$trainer_score*4/10

if(m == 1) {
    cat("Scaling all scores to scale of 4\n")
}


us.pre$knowledge_scale  <- us.pre$pre_score/us.pre$total*4
us.post$knowledge_scale <- us.post$post_score/us.post$total*4

us.pre$name <- as.character(us.pre$name)
us.post$name <- as.character(us.post$name)


if(m == 1) {
    cat("Doing geeky stuff\n")
}


units <- unlist(strsplit(us.pre$name, " "))
units <- units[units != "Player"]
codes <- units[which(nchar(units) == 1)]
if(length(codes) == nrow(us.pre)) {
    us.pre$codes <- codes
}
units <- unlist(strsplit(us.post$name, " "))
units <- units[units != "Player"]
codes <- units[which(nchar(units) == 1)]
if(length(codes) == nrow(us.post)) {
    us.post$codes <- codes
}

if(m == 1) {
    Sys.sleep(2)
    cat("Some more geeky stuffs\n")
}


ts.pre$name <- as.character(ts.pre$name)
ts.post$name <- as.character(ts.post$name)

units <- unlist(strsplit(ts.pre$name, " "))
units <- units[units != "Player"]
codes <- units[which(nchar(units) == 1)]
if(length(codes) == nrow(ts.pre)) {
    ts.pre$codes <- codes
}
units <- unlist(strsplit(ts.post$name, " "))
units <- units[units != "Player"]
codes <- units[which(nchar(units) == 1)]
if(length(codes) == nrow(ts.post)) {
    ts.post$codes <- codes
}

if(m == 1) {
    Sys.sleep(2)
    cat("Still some more geeky stuffs\n")
}


                                        # Adjust skill score pre

if(m == 1) {
    cat("Calculating deltas\n")
}

if(rf == "csv") {
    write.csv(us.pre, file = paste0(p, "/", "knowledge_scores_pre.csv"))
} else {
    save(us.pre, file = paste0(p, "/", "knowledge_scores_pre.rdata"))
}

if(rf == "csv") {
    write.csv(us.post, file = paste0(p, "/", "knowledge_scores_post.csv"))
} else {
    save(us.post, file = paste0(p, "/", "knowledge_scores_post.rdata"))
}

if(rf == "csv") {
    write.csv(matrix.pre, file = paste0(p, "/", "peer_scores_matrix_pre.csv"))
} else {
    save(us.pre, file = paste0(p, "/", "peer_scores_matrix_pre.rdata"))
}

if(rf == "csv") {
    write.csv(matrix.post, file = paste0(p, "/", "peer_scores_matrix_post.csv"))
} else {
    save(us.pre, file = paste0(p, "/", "peer_scores_matrix_post.rdata"))
}


if(rf == "csv") {
    write.csv(ts.pre, file = paste0(p, "/", "trainer_scores_pre.csv"))
} else {
    save(ts.pre, file = paste0(p, "/", "trainer_scores_pre.rdata"))
}

if(rf == "csv") {
    write.csv(ts.post, file = paste0(p, "/", "trainer_scores_post.csv"))
} else {
    save(ts.post, file = paste0(p, "/", "trainer_scores_post.rdata"))
}

pre.skill <- apply(matrix.pre, 2, function(x) {
    dat <- data.table(codes = c,
                      score = x)
    k <- us.pre[, c("codes", "knowledge_scale"), with = FALSE]
    t <- ts.pre[, c("codes", "trainer_scale")]
    dat <- Reduce(function(x, y) merge(x, y, by = "codes"),
                  list(dat, k, t))
    dat$weight <- dat$knowledge_scale*dat$trainer_scale
    dat <- dat[!is.na(dat$score)]
    denom <- sum(dat$weight)
    dat$weighted_score <- dat$score*dat$weight
    return(sum(dat$weighted_score)/denom)
    
})

names(pre.skill) <- c

post.skill <- apply(matrix.post, 2, function(x) {
    dat <- data.table(codes = c,
                      score = x)
    k <- us.pre[, c("codes", "knowledge_scale"), with = FALSE]
    t <- ts.pre[, c("codes", "trainer_scale")]
    wv <- data.table(codes = names(weight.val),
                     peer_scale = weight.val)
    dat <- Reduce(function(x, y) merge(x, y, by = "codes"),
                  list(dat, k, t, wv))
    dat$weight <- dat$knowledge_scale*dat$trainer_scale*dat$peer_scale
    dat <- dat[!is.na(dat$score)]
    denom <- sum(dat$weight)
    dat$weighted_score <- dat$score*dat$weight
    return(sum(dat$weighted_score)/denom)
    
})

names(post.skill) <- c

pre.skill <- pre.skill[order(names(pre.skill))]
post.skill <- post.skill[order(names(post.skill))]

peer.delta <- (post.skill - pre.skill)/pre.skill

us.pre <- us.pre[order(us.pre$codes)]
us.post <- us.post[order(us.post$codes)]

knowledge.delta <- us.post$knowledge_scale - us.pre$knowledge_scale

ts.pre <- ts.pre[order(ts.pre$codes)]
ts.post <- ts.post[order(ts.post$codes)]

trainer.delta <- ts.post$trainer_scale - ts.pre$trainer_scale

retval <- data.table(codes = c,
                     peer_delta = peer.delta,
                     knowledge_delta = knowledge.delta,
                     trainer_delta = trainer.delta)

cat("Storing results\n")

retval.denom <- sum(wgt)
retval$skill_delta <- ((retval$peer_delta*wgt[1]) +
                       (retval$knowledge_delta*wgt[2]) +
                       (retval$trainer_delta*wgt[3]))*100/retval.denom


if(rf == "csv") {
    write.csv(retval, file = paste0(p, "/", "participant_results.csv"))
} else {
    save(retval, file = paste0(p, "/", "participant_results.rdata"))
}

cat("Execution finished successfully. Terminating caches\n")
