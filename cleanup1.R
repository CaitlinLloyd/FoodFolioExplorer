#recode age
dat <- dat %>%  dplyr::mutate(age2 = as.numeric(as.factor(dat$age))) 

Obj_info <-read.csv("nutrient_info.csv")
Obj_info$stimulus <- gsub(".jpg", "", Obj_info$Food_image)
t <- merge(dat, Obj_info, by="stimulus")

# code whether a SC trial at all - also count out of trial with neutral response (note distinction between sc and sc_fac 
# fac is for the healthiness/tastiness factors, not the ratings)
ref <- subset(t, is.na(t$choice.rating))
ref <- ref %>% dplyr::select("subjectId", "stimulus",starts_with("rating"),"Tastiness","Healthiness","Umami", ends_with("cal"),ends_with("_g"))
colnames(ref)[2:29] <- paste0("neut.",colnames(ref)[2:29])

dat <- merge(dat, ref,by="subjectId")

dat$sc_bin<-NULL
dat$sc_bin[(dat$rating.tasty > dat$neut.rating.tasty & dat$rating.healthy < dat$neut.rating.healthy) | (dat$rating.tasty < dat$neut.rating.tasty & dat$rating.healthy >  dat$neut.rating.healthy)] <- 1
dat$sc_bin[(dat$rating.tasty > dat$neut.rating.tasty & dat$rating.healthy > dat$neut.rating.healthy) | (dat$rating.tasty < dat$neut.rating.tasty & dat$rating.healthy < dat$neut.rating.healthy)] <- 0


# use of self-control (1 vs 0) - NOT expanded bins 
dat$sc<-NULL
dat$sc[(dat$rating.tasty > dat$neut.rating.tasty & dat$rating.healthy < dat$neut.rating.healthy & dat$choice.rating <5)  | (dat$rating.tasty < dat$neut.rating.tasty & dat$rating.healthy > dat$neut.rating.healthy & dat$choice.rating >5)] <- 1
dat$sc[(dat$rating.tasty > dat$neut.rating.tasty & dat$rating.healthy < dat$neut.rating.healthy & dat$choice.rating >5)  | (dat$rating.tasty < dat$neut.rating.tasty & dat$rating.healthy > dat$neut.rating.healthy & dat$choice.rating <5)] <- 0
dat$sc[dat$choice.rating == 5] <- NA
dat$sc[dat$sc_bin == 0] <- NA

dat$sc_bin_fac<-NULL
dat$sc_bin_fac[(dat$Tastiness > dat$neut.Tastiness & dat$Healthiness < dat$neut.Healthiness) | (dat$Tastiness < dat$neut.Tastiness & dat$Healthiness >  dat$neut.Healthiness)] <- 1
dat$sc_bin_fac[(dat$Tastiness > dat$neut.Tastiness & dat$Healthiness > dat$neut.Healthiness) | (dat$Tastiness < dat$neut.Tastiness & dat$Healthiness < dat$neut.Healthiness)] <- 0

# use of self-control (1 vs 0) - NOT expanded bins
dat$sc_fac<-NULL
dat$sc_fac[(dat$Tastiness > dat$neut.Tastiness & dat$Healthiness < dat$neut.Healthiness & dat$choice.rating <5)  | (dat$Tastiness < dat$neut.Tastiness & dat$Healthiness > dat$neut.Healthiness & dat$choice.rating >5)] <- 1
dat$sc_fac[(dat$Tastiness > dat$neut.Tastiness &  dat$Healthiness < dat$neut.Healthiness & dat$choice.rating >5)  | (dat$Tastiness < dat$neut.Tastiness & dat$Healthiness > dat$neut.Healthiness & dat$choice.rating <5)] <- 0
dat$sc_fac[dat$choice.rating == 5] <- NA
dat$sc_fac[dat$sc_bin_fac == 0] <- NA

dat$sc_bin_fac_approach_health <-NULL
dat$sc_bin_fac_approach_health[dat$sc_bin_fac == 1] <- 0
dat$sc_bin_fac_approach_health[(dat$Tastiness < dat$neut.Tastiness & dat$Healthiness >  dat$neut.Healthiness)] <- 1


dat$sc_bin_fac_avoid_taste <-NULL
dat$sc_bin_fac_avoid_taste[dat$sc_bin_fac == 1] <- 0
dat$sc_bin_fac_avoid_taste[(dat$Tastiness > dat$neut.Tastiness & dat$Healthiness < dat$neut.Healthiness)] <- 1


# use of self-control (1 vs 0) - NOT expanded bins
dat$sc_fac_approach_health <-NULL
dat$sc_fac_approach_health[dat$sc_bin_fac_approach_health==1] <- 0
dat$sc_fac_approach_health[(dat$Tastiness < dat$neut.Tastiness & dat$Healthiness > dat$neut.Healthiness & dat$choice.rating >5)] <- 1
dat$sc_fac_approach_health[dat$choice.rating == 5] <- NA
dat$sc_fac_approach_health[dat$sc_bin_fac_approach_health == 0] <- NA

dat$sc_fac_avoid_taste <-NULL
dat$sc_fac_avoid_taste[dat$sc_bin_fac_avoid_taste==1] <- 0
dat$sc_fac_avoid_taste[(dat$Tastiness > dat$neut.Tastiness & dat$Healthiness < dat$neut.Healthiness & dat$choice.rating < 5)] <- 1
dat$sc_fac_avoid_taste[dat$choice.rating == 5] <- NA
dat$sc_fac_avoid_taste[dat$sc_bin_fac_avoid_taste == 0] <- NA

# this calculates n self-control opps and usage per participant
sc <- dat %>% subset(dat$sc_bin==1) %>% dplyr::select(subjectId, sc_bin, sc) %>% group_by(subjectId,sc) %>% tally() %>% spread(sc, n) %>% dplyr::select(-4)
sc$sc.rat <- (rowSums(sc[,c("0", "1")], na.rm=TRUE))
sc$sc.rat <- sc$`1`/sc$sc.rat
sc <- as.data.frame(sc[,c(1,4)])
colnames(sc) <- c("subjectId", "sc.rat")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)



sc <- dat %>% subset(dat$sc_bin_fac==1) %>% dplyr::select(subjectId, sc_bin_fac, sc_fac) %>% group_by(subjectId,sc_fac) %>% tally () %>% spread(sc_fac, n) %>% dplyr::select(-4)
sc$sc.rat <- (rowSums(sc[,c("0", "1")], na.rm=TRUE))
sc$sc.rat <- sc$`1`/sc$sc.rat
sc <- as.data.frame(sc[,c(1,4)])
colnames(sc) <- c("subjectId", "sc.rat.factor")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)


sc <- dat %>% subset(dat$sc_bin_fac==1) %>% dplyr::select(subjectId, sc_bin_fac, sc_fac) %>% group_by(subjectId,sc_fac) %>% tally () %>% spread(sc_fac, n) %>% dplyr::select(-4)
sc$sc.rat <- (rowSums(sc[,c("0", "1")]))

sc <- as.data.frame(sc[,c(1,3)])
colnames(sc) <- c("subjectId", "sc.factor.success.n")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)


sc <- dat %>% subset(dat$sc_bin_fac_approach_health==1) %>% dplyr::select(subjectId, sc_bin_fac_approach_health, sc_fac_approach_health) %>% group_by(subjectId,sc_fac_approach_health) %>% tally () %>% spread(sc_fac_approach_health, n) %>% dplyr::select(-4)
sc$sc.rat <- (rowSums(sc[,c("0", "1")], na.rm=TRUE))
sc$sc.rat <- sc$`1`/sc$sc.rat
sc <- as.data.frame(sc[,c(1,4)])
colnames(sc) <- c("subjectId", "sc.rat.factor.approach.health")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)

sc <- dat %>% subset(dat$sc_bin_fac_avoid_taste==1) %>% dplyr::select(subjectId, sc_bin_fac_avoid_taste, sc_fac_avoid_taste) %>% group_by(subjectId,sc_fac_avoid_taste) %>% tally () %>% spread(sc_fac_avoid_taste, n) %>% dplyr::select(-4)
sc$sc.rat <- (rowSums(sc[,c("0", "1")], na.rm=TRUE))
sc$sc.rat <- sc$`1`/sc$sc.rat
sc <- as.data.frame(sc[,c(1,4)])
colnames(sc) <- c("subjectId", "sc.rat.factor.avoid.taste")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)


dat$choice.rt <- ifelse(dat$choice.rt < 200,NA, dat$choice.rt)
dat$choice.rt <- ifelse(dat$choice.rt > 5000,NA, dat$choice.rt)

dat$choice.rt.unscale <-  dat$choice.rt
dat <- dat %>% group_by(subjectId) %>%
  mutate(choice.rt = scale(choice.rt)) 
dat$choice.rt <- (dat$choice.rt)[,1]

sc <- dat %>% subset(dat$sc_bin==1) %>% dplyr::select(subjectId, sc_bin) %>% group_by(subjectId,sc_bin) %>% tally() 
sc <- sc[,c(1,3)]
colnames(sc) <- c("subjectId", "n.sctrial")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)

sc <- dat %>% subset(dat$sc_bin_fac==1) %>% dplyr::select(subjectId, sc_bin_fac) %>% group_by(subjectId,sc_bin_fac) %>% tally() 
sc <- sc[,c(1,3)]
colnames(sc) <- c("subjectId", "n.sctrial_fac")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)

sc <- dat %>% subset(dat$sc_bin_fac_approach_health==1) %>% dplyr::select(subjectId, sc_bin_fac_approach_health) %>% group_by(subjectId,sc_bin_fac_approach_health) %>% tally() 
sc <- sc[,c(1,3)]
colnames(sc) <- c("subjectId", "n.sctrial_fac_approach_health")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)

sc <- dat %>% subset(dat$sc_bin_fac_avoid_taste==1) %>% dplyr::select(subjectId, sc_bin_fac_avoid_taste) %>% group_by(subjectId,sc_bin_fac_avoid_taste) %>% tally() 
sc <- sc[,c(1,3)]
colnames(sc) <- c("subjectId", "n.sctrial_fac_avoid_taste")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)


sc <- dat %>% group_by(subjectId,sc_fac) %>% summarize_at("choice.rt.unscale",c(mean),na.rm=TRUE) %>% pivot_wider(1,names_from=2,values_from=3)
colnames(sc) <- c("subjectId", "sc.fail.rt","no.sc.rt","sc.success.rt")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)

sc <- dat %>% group_by(subjectId) %>% summarize_at("choice.rt.unscale",c(mean),na.rm=TRUE) 
colnames(sc) <- c("subjectId", "avg.rt")
dat <- merge(dat,sc, by.x="subjectId", by.y="subjectId", all.x = TRUE)


# change the demo codings
dat$ethnicity <- ifelse(dat$ethnicity=="White","White","Non-white")

dat$wt.cat <- cut(as.numeric(dat$bmi), breaks=c(0,18.5,25,30,50,80), labels=c("underweight", "health", "overweight", "obese", "super obese"), order=TRUE)
dat$rest.cat <- cut(as.numeric(dat$s.cognitive.restraint), breaks=c(0,33.3,44.4,61.11,100), labels=c("low restraint", "moderate restraint", "high restraint", "very high restraint"), order=TRUE)
dat$age.cat <- cut(as.numeric(dat$age2), breaks=c(0,2,5,8,11), labels=c("under 30", "30 to 45", "45 to 60", "60+"), order=TRUE)
dat$social.class <- dplyr::recode(dat$social.class, "Below the poverty level"=0, "Lower middle class"=1, "Middle class"=2, "Upper middle class"=3, "Upper class"=4)
dat$income <- dplyr::recode(dat$income, "Less than $20,000"=0, "$20,000 to $34,999"=1, "$35,000 to $49,999"=2, "$50,000 to $74,999"=3, "$75,000 to $99,999"=4, "Over $100,000"=5)

dat$income <- as.numeric(dat$income)
dat$social.class <- as.numeric(dat$social.class)

dat$education <- NA
dat$education <- ifelse(dat$degree == "Some college" | dat$degree == "High school", 0,dat$education)
dat$education <- ifelse(dat$degree == "Associates", 1,dat$education)
dat$education <- ifelse(dat$degree == "Bachelors", 2,dat$education)
dat$education <- ifelse(dat$degree == "Masters"| dat$degree == "Professional" | dat$degree == "Doctorate", 3,dat$education)
dat$education <- as.numeric(dat$education, order=TRUE)

dat$region <- as.character(dat$region)
dat$region <- ifelse(dat$region=="Fareast","Other",dat$region)
dat$region <- ifelse(dat$region=="Farwest","Other",dat$region)
dat$region <- ifelse(dat$region=="Hawaii/Alaska/US territories","Other",dat$region)
dat$region <- ifelse(dat$region=="Outside of the US","Other",dat$region)

dat <- distinct(dat, stimulus, subjectId, .keep_all= TRUE)


# Standardize the ratings (note this is already done for factors)
# cols = dat %>% dplyr::select(starts_with("rating")) %>% colnames() # Name of old columns
# 
# 
# setDT(dat)[, (cols) := lapply(.SD, function(x) as.vector(scale(x))) , by = subjectId, .SDcols= cols]

dat <- data.frame(dat)[,-(2)]




Obj_info <- read.csv("nutrient_info.csv")
Obj_info$stimulus <- gsub(".jpg", "", Obj_info$Food_image)
t <- merge(dat, Obj_info, by="stimulus")
t$yes <- ifelse(t$choice.rating > 5,1,0)
t$rt.bin <- ifelse(t$choice.rt <0,0,1)

t.1 <- t %>% dplyr::group_by(subjectId,HI_LO_fat) %>% tally(yes)
te <- pivot_wider(data = t.1,
                  id_cols = subjectId,
                  names_from = c(HI_LO_fat),
                  values_from = n)
colnames(te) <- c("subjectId", "lofat.yes", "hifat.yes")
t <- merge(t,te, by="subjectId", all.x = TRUE)




t.1 <- t %>% dplyr::group_by(subjectId,HI_LO_fat) %>% count()
te <- pivot_wider(data = t.1,
                  id_cols = subjectId,
                  names_from = c(HI_LO_fat),
                  values_from = n)

colnames(te) <- c("subjectId", "lofat", "hifat")
t <- merge(t,te, by="subjectId", all.x = TRUE)

t$prop.h.f <- t$hifat.yes/t$hifat
t$prop.l.f <- t$lofat.yes/t$lofat


t$choice_bin <- NULL

# now unhealthy choice is opposite of healthy
t$choice_bin <- ifelse(t$choice.rating > 5,1,0)
t$healthy_choice <- ifelse(t$Healthiness > t$neut.Healthiness & t$choice_bin==1 | t$Healthiness < t$neut.Healthiness & t$choice_bin==0,1,0)


t.1 <- t %>% dplyr::group_by(subjectId,healthy_choice) %>% tally()
te <- pivot_wider(data = t.1,
                  id_cols = subjectId,
                  names_from = c(healthy_choice),
                  values_from = n)
te$total <- te$`0` + te$`1` 
te$choice_healthy <- te$`1`/te$total


te <- te[,c(1,5)]
colnames(te) <- c("subjectId", "healthy.choice.prop")

t <- merge(t,te, by="subjectId", all.x = TRUE)


t.1 <- t %>% dplyr::group_by(subjectId,choice_bin) %>% tally()
te <- pivot_wider(data = t.1,
                  id_cols = subjectId,
                  names_from = c(choice_bin),
                  values_from = n)
te <- te[,c(1,3)]
te$`1`[is.na(te$`1`)] <- 0
colnames(te) <- c("subjectId", "choice.alternate")

t <- merge(t,te, by="subjectId", all.x = TRUE)

## summarize RT by trial type
t.1 <- t %>% dplyr::group_by(subjectId,sc) %>% summarize_at("choice.rt.unscale",c(mean),na.rm=TRUE)
te <- pivot_wider(data = t.1,
                  id_cols = subjectId,
                  names_from = c(sc),
                  values_from = choice.rt.unscale)

colnames(te) <- c("subjectId", "rt_no_sc","rt_no_sc_trial","rt_sc")

t <- merge(t,te, by="subjectId", all.x = TRUE)

t.1 <- t %>% dplyr::group_by(subjectId) %>% summarize(corr_fat=cor(Fat_pctKcal,rating.fat), corr_kcal=cor(Total.kcal,rating.calories),
                                                      corr_pro=cor(PRO_pctKcal,rating.protein),corr_carb=cor(CHO_pctKcal,rating.carbohydrates),
                                                      corr_fat_g=cor(Fat_g,rating.fat), corr_pro_g=cor(PRO_g,rating.protein),corr_carb_g=cor(CHO_g,rating.carbohydrates),
                                                      corr_fat_health_g=cor(Fat_g,Healthiness), corr_fat_health=cor(Fat_pctKcal,Healthiness),corr_kcal_health=cor(Total.kcal,Healthiness))

t <- merge(t,t.1, by="subjectId", all.x = TRUE)

health_select <- t

## info about ref item

health_select <- health_select[,c("subjectId", "healthy.choice.prop","prop.h.f","prop.l.f", "choice.alternate","rt_no_sc","rt_no_sc_trial","rt_sc","corr_fat", "corr_kcal",
                                  "corr_pro", "corr_carb", "corr_fat_health", "corr_kcal_health","corr_fat_g","corr_pro_g","corr_carb_g","corr_fat_health_g")]


## correlations between rated healthiness and energy density/fat content




# t.1 <- t %>% dplyr::group_by(subjectId,HI_LO_fat) %>% summarise_at(vars(c("Tastiness", "Healthiness","Umami", "rating.tasty", "rating.healthy", "choice.rating")), funs(mean(., na.rm=TRUE)))
# 
# single <- t[!(duplicated(t$subjectId)),]
# single$rt.bin <- as.factor(single$rt.bin)
# long <- single %>% dplyr::select (subjectId, age2, age.cat, bmi, wt.cat, gender, ethnicity, income, degree, education, rt.bin, rest.cat, starts_with("prop")) 
# long.hilo <- pivot_longer(long, starts_with("prop"), names_to="HI_LO_fat")
# long.hilo$HI_LO_fat <- gsub("prop.h.f", "high-fat", long.hilo$HI_LO_fat)
# long.hilo$HI_LO_fat <- gsub("prop.l.f", "low-fat", long.hilo$HI_LO_fat)
# long.hilo$prop <- long.hilo$value
# t.1$HI_LO_fat <-  gsub(1, "high-fat", t.1$HI_LO_fat)
# t.1$HI_LO_fat <-  gsub(0, "low-fat", t.1$HI_LO_fat)
# 
# long.hilo <- merge(long.hilo,t.1, by=c("subjectId", "HI_LO_fat"), all.x = TRUE)
# long.hilo_back <- long.hilo


