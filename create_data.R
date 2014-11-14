# 
# library(rqog)
# library(reshape2)
# # basic
# basic <- read_qog(which.data = "basic")
# df.long <- melt(basic, id.vars=c("ccode","cname","year","ccodealp","cname_year","ccodealp_year","ccodecow","ccodewb","version"))
# 
# ## poistetaan turhia ##
# df.long$ccode <- NULL
# df.long$ccodealp <- NULL
# df.long$ccodealp_year <- NULL
# df.long$cname_year <- NULL
# df.long$ccodecow <- NULL
# df.long$ccodewb <- NULL
# df.long$version <- NULL
# 
# basic <- df.long
# 
# ## ------------------------------------------##
# 
# # standard
# standard <- read_qog(which.data = "standard")
# df.long <- melt(standard, id.vars=c("ccode","cname","year","ccodealp","cname_year","ccodealp_year","ccodecow","ccodewb","version"))
# 
# ## poistetaan turhia ##
# df.long$ccode <- NULL
# df.long$ccodealp <- NULL
# df.long$ccodealp_year <- NULL
# df.long$cname_year <- NULL
# df.long$ccodecow <- NULL
# df.long$ccodewb <- NULL
# df.long$version <- NULL
# 
# standard <- df.long
# 
# ## ------------------------------------------##
# 
# # social policy
# social_policy <- read_qog(which.data = "social_policy")
# 
# social_policy$oecd <- NULL
# social_policy$eu27 <- NULL
# social_policy$eu15 <- NULL
# social_policy$eea <- NULL
# social_policy$ht_region <- NULL
# social_policy$ht_region2 <- NULL
# 
# df.long <- melt(social_policy, id.vars=c("ccode","cname","year","ccodealp","cname_year","ccodealp_year","ccodecow","ccodewb","version"))
# 
# ## poistetaan turhia ##
# df.long$ccode <- NULL
# df.long$ccodealp <- NULL
# df.long$ccodealp_year <- NULL
# df.long$cname_year <- NULL
# df.long$ccodecow <- NULL
# df.long$ccodewb <- NULL
# df.long$version <- NULL
# 
# ## jstain syystä ongelmia
# df.long$value <- factor(df.long$value)
# df.long$value <- as.numeric(levels(df.long$value))[df.long$value]
# social_policy <- df.long
# 
# ## ------------------------------------------- ## 
# ## lisätään kvalimuuttujia mun datasta
# library(RCurl)
# GHurl <- getURL("https://raw.githubusercontent.com/muuankarski/data/master/world/countries_continents.csv")
# dat <- read.csv(text = GHurl)
# 
# df_diko <- dat[c("cntryName","EU15","EU27","OECD","europe","asia","north_america","south_america","africa","oceania","nordic","postsos")]
# names(df_diko)[1] <- "cname"
# # df.long <- melt(df_diko, id.vars=c("cntryName"))
# # names(df.long)[1] <- "cname"
# 
# # dl <- data.frame()
# # for (i in 1946:2014) {
# #   dd <- df.long
# #   dd$year <- i
# #   dl <- rbind(dl,dd)
# # }
# 
# # dlb <- dl[dl$cname %in% unique(basic$cname),]
# # dls <- dl[dl$cname %in% unique(standard$cname),]
# # dlsp <- dl[dl$cname %in% unique(social_policy$cname),]
# # 
# # basic <- rbind(basic,dlb)
# # standard <- rbind(standard,dls)
# # social_policy <- rbind(social_policy,dlsp)
# 
# basic <- basic[!is.na(basic$value),]
# standard <- standard[!is.na(standard$value),]
# social_policy <- social_policy[!is.na(social_policy$value),]
# 
# basic <- merge(basic,df_diko,by="cname", all.x=TRUE)
# standard <- merge(standard,df_diko,by="cname", all.x=TRUE)
# social_policy <- merge(social_policy,df_diko,by="cname", all.x=TRUE)
# 
# basic <- basic[!duplicated(basic[c("cname","year","variable")]),]
# standard <- standard[!duplicated(standard[c("cname","year","variable")]),]
# social_policy <- social_policy[!duplicated(social_policy[c("cname","year","variable")]),]
# 
# save(basic, file="data/basic.rda")
# save(standard, file="data/standard.rda")
# save(social_policy, file="data/social_policy.rda")
# 
