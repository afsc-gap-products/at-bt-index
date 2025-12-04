#' Script for conducting initial data manipulation steps to get to binned 
#' acoustic data at-depth. These steps are taken from code written by Cole
#' Monnahan.

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(forcats)

# Set ggplot theme
if (!requireNamespace("ggsidekick", quietly = TRUE)) {
  devtools::install_github("seananderson/ggsidekick")
}
library(ggsidekick)
theme_set(theme_sleek())

# Set working directory
wd <- here("data", "at")

# Read in data, which was combined & cleaned in at_read_combine.R
dat <- readRDS(here(wd, "at_combined.rds"))

## whether to use 0.5-3m from the other data set and add this onto the 3-b2 stratum so it goes from 0.5-b2.
strata <- 3
b1 <- 3 ## !!do not change!!, this is **NOT** the lower
## breakpoint it has to do with the split AT data sets
b2 <- 16  # SNW: higher breakpoint?

## This is only tested for these two values but theoretically should work as long as b2>=0.
stopifnot(b2 %in% c(3, 16))
stopifnot(b1 == 3)

#' But some are still duplicates and Nate says to sum across duplicated keys. 
#' Due to the large size it may be better to break it into two chunks just for 
#' this calculation. I can't just group by key and sum density b/c I need to 
#' keep all the column and grouping by the other columns would be too slow.
dups <- dat$key[which(duplicated(dat$key))]
tmp <- dat[which(dat$key %in% dups), ]
tmp <- tmp[order(tmp$key), ]

dir.create(here(wd,"processing"), showWarnings = FALSE, recursive = TRUE)
write.csv(here(wd, "processing", "duplicated_keys.csv"), x = tmp)
dups.freq <- group_by(tmp, key) %>%  summarize(count = length(key))
write.csv(here(wd, "processing", "duplicated_keys_counts.csv"), x = dups.freq)

table(dups.freq$count)
d1 <- filter(dat, !(key %in% dups))     # unique
d2 <- filter(dat, key %in% dups)        # non unique
d3 <- group_by(d2, key) %>% summarize(year = year[1], 
                                      key2 = key2[1], 
                                      key3 = key3[1], 
                                      ground = ground[1], 
                                      surface = surface[1],
                                      lon = lon[1], 
                                      lon2 = mean(lon),
                                      lat = lat[1], 
                                      pollock = pollock[1],
                                      dist = dist[1], 
                                      bottom = bottom[1], 
                                      top = top[1], 
                                      duration = duration[1],
                                      density = sum(density))
## just check that the longs are the same then delete it
sum(d3$lon != d3$lon2)
d3$lon2 <- NULL
## Now recombine together and check no density was lost
datnew <- bind_rows(d1, d3)
stopifnot(abs(sum(datnew$density) - sum(dat$density)) < 1e-8)
## should now be no duplicated keys and we can drop the depth layer from the key
stopifnot(sum(duplicated(datnew$key)) == 0)
dat <- datnew
rm(d1,d2,d3, datnew, tmp, dups, dups.freq); gc()

## Create the vertical strata used in model by summing across layers.
b <- dat$bottom; t <- dat$top
## with(dat, plot(bottom, top-bottom))
stratum <- rep(NA, len = nrow(dat))
#' 0-3m; note this is unused b/c below we use the below3 data set. But it is 
#' used for some tows that are off the shelf where these would then be reliable 
#' b/c their height off surface is actually >3m.
stratum[b >= 0 & t <= b1] <- "stratum1"  # 0-3m (unreliable and unused)
stratum[b >= b1 & t <= b2] <- "stratum2" # 3-EFHm
stratum[b >= b2] <- "stratum3" # EFH-surface
dat$stratum <- stratum
## None of these are important, not sure why negative but they don't have pollock so doesn't matter.
# write.csv(filter(dat, is.na(stratum)), file='bad_strata.csv')
stopifnot(0 == mean(is.na(stratum) & dat$density > 0))
dat <- filter(dat, !(is.na(stratum))) ## drop the zero densities
# barplot(table(dat$year))
rm(stratum, t, b); gc()

#' The real trick here is that we can't simply drop the non-pollock and sum 
#' across stratum. For one, when there are intervals without pollock but with 
#' non-pollock, the data has the NP but not the pollock, so I need to back-fill 
#' those cases as zeroes for P. Another weird case is when the hit the shelf 
#' and the bottom is unknown, so that we have no reference point and the depths 
#' are incorrect. In this case I need to assume all those fish are in the top 
#' stratum and backfill zeroes for the bottom two strata. It is **very** 
#' important to maintain these zeroes as they are real and explicitly modeled in VAST.

#' There's two cases we should check. First  when the max depth is really big, 
#' which happens when the survey goes past the slope and cant find the bottom 
#' and so the software has weird depths. The second is really shallow water such 
#' that the three depth strata arent even covered.

#' So first sum across layers for all unique keys to get the three model strata 
#' for each location (interval and year) and pollock nonpollock
out <- group_by(dat, key3, stratum, pollock) %>%
  summarize(year = year[1],
            lon = lon[1], 
            lat = lat[1],
            dist = dist[1], 
            surface = surface[1],
            ground = ground[1],  
            bottom = bottom[1],
            density = sum(density))
## These arent consistent so I will need to fill in zeroes.
table(out$stratum)

## All possible combinations of each stratum for each location have zero
## density.
zeroes <- expand.grid(key4 = unique(out$key3), 
                      stratum = unique(out$stratum),
                      pollock = c(TRUE, FALSE), 
                      density2 = 0) 
zeroes$key4 <- with(zeroes, paste(key4, stratum, pollock, sep = "_"))
out$key4 <- with(out, paste(key3, stratum, pollock, sep = "_"))
## Merge the real densities into the one populated by zeroes, This is a left join.
tmp <- merge(out[, c("key4", "density")], zeroes, by = "key4", all.y = TRUE)
## Missing values in the real data are now density=NA
stopifnot(nrow(zeroes) - nrow(out) == sum(is.na(tmp$density)))
## Replace NA's with zeroes
tmp$density[is.na(tmp$density)] <- 0
tmp$density2 <- NULL
## check we didn't lose any biomass
stopifnot(isTRUE(all.equal(sum(tmp$density), sum(dat$density))))
#' Now tmp has a density for all combinations of strata and locations and 
#' pollock non pollock. We need to merge the location meta data into the new 
#' data frame so first create a summary DF.
meta <- out[!duplicated(out$key3), c("key3", "year", "lon", "lat",
                                     "dist", "surface", "ground")]
tmp$key3 <- plyr::ldply(strsplit(tmp$key4, split = "_s"), function(x) x[[1]])[, 1]
dat2 <- merge(meta, tmp, by = "key3") %>%
  ## Can finally drop the non-pollock stuff
  filter(pollock == TRUE) %>% select(-pollock, -key4)
## consistent now since backfilled with zeroes
x <- as.numeric(table(dat2$stratum))
stopifnot(isTRUE(all.equal(x[1], x[2], x[3])))
## Check we didn't lose any pollock density
stopifnot(isTRUE(all.equal(sum(dat2$density), sum(dat$density[dat$pollock]))))
rm(tmp, zeroes, x); gc()

## Now can deal with issue on the shelf. First make each stratum its own column
s1 <- subset(dat2, surface > 450 & stratum == "stratum1")
s2 <- subset(dat2, surface > 450 & stratum == "stratum2")
s3 <- subset(dat2, surface > 450 & stratum == "stratum3")
## All measured density assumed to be in the top stratum
if(b2 == 3) {
  ## There is no s2 in this case because s2 represents biomass in 3-3m.
  s3$density <- s1$density + s3$density
  s1$density <- 0
} else {
  s3$density <- s1$density + s2$density + s3$density
  ## so the other two are 0
  s1$density <- s2$density <- 0
}
## Now bind them back together
dat3 <- rbind(subset(dat2, surface <= 450), s1, s2, s3)
## should not have lost anything
stopifnot(nrow(dat2) == nrow(dat3))
stopifnot(sum(dat2$density) == sum(dat3$density))
rm(out, dat, dat2, meta, s3, s2, s1); gc()

#' now dat3 is the final data set once we drop the first stratum (0-3m) since 
#' that is added from a separate source below
above3 <- droplevels(filter(dat3, stratum != "stratum1")) %>%
  ## Add column for merging with below3 data below
  mutate(key = paste(year, round(lat, 3), round(lon, 3))) %>%
  ## Melt it to have stratum as columns. We then will add stratum1 below
  spread(stratum, density)
## again if b2==3 this fixes errors
if(is.null(above3$stratum2)) above3$stratum2 <- 0
rm(dat3); gc()
write.csv(above3, file = here(wd, "processing", "above3.csv"), row.names = FALSE)

#' This is the end of processing the raw acoustic data. This only reliably goes 
#' to 3m so stratum1 is not reliable. Instead, Nate provided a separate dataset 
#' which comes from his paper. so I replace stratum1 with this data

# SNW: load in data and continue with processing ------------------------------
if(exists("above3") == FALSE) {
  above3 <- read.csv(here(wd, "processing", "above3.csv"))
}
below3 <- readRDS(here(wd, "below3.rds"))  # SNW: from at_read_combine.R

#' All those with NA biomass are zero NASC. Nate confirmed this is true. 
#' Convert to 0 before merging.
# below3 %>% filter(is.na(biomass)) %>% pull(NASC)
below3$biomass[is.na(below3$biomass)] <- 0

#' calculate density= (1000*metric tons)/(area)=kg/nm^2 which matches the above3 
#' data set; and calculate key for merging. kg/nm^2 gets converted to kg/km^2 below.
below3 <- mutate(below3,
                 stratum1 = (1000 * biomass) / area,
                 key = paste(year, round(lat, 3), round(lon, 3))) %>%
  select(stratum1, key)
#' Now merge the two. Stratum1=0.5 to 3m; stratum2=3-b2; stratum3= >b2. Need to 
#' be careful about what to do with missing values. all.x=TRUE means to keep all 
#' values from above3 but drop those from below3. This is needed because some 
#' intervals were dropped from above3 (filtered out above) so won't match. 
#' There's some old code here for exploring these mismatches.
ats.wide <- merge(above3, below3, by = 'key', all.x = TRUE)
ats.wide %>% group_by(year) %>%
  summarize(pct.missing.below3 = mean(is.na(stratum1)))

missing_in_above3 <- filter(ats.wide, is.na(stratum2))
missing_in_below3  <- filter(ats.wide, is.na(stratum1))
write.csv(file = here(wd, "processing", "missing_in_above3.csv"), missing_in_above3)
write.csv(file = here(wd, "processing", "missing_in_below3.csv"), missing_in_below3)
g <- missing_in_below3 %>% 
  ggplot(aes(lon, lat)) +
  facet_wrap("year") + 
  geom_point()
ggsave(here(wd, "processing", "below3_missing_map.png"), g, width = 7, height = 6)
message(paste("Combining above3 and below3.."))
message(paste(nrow(missing_in_above3), " rows missing in above3"))
message(paste(nrow(missing_in_below3), " rows missing in below3"))
#' Nate says regarding mismatch in coordinates: This was something with rounding 
#' errors when re-exporting using our Echoview software, probably due to a newer 
#' version.  This changed the lat/lon values up to 3-4 decimal places. We 
#' decided to simply drop those that don't match since it's a small fraction 
#' (Except for those outside the EBS) and they are randomly distributed.
message("Dropping observations with coordinate mismatch")
ats.wide <- filter(ats.wide, !is.na(stratum1) & !is.na(stratum2) & !is.na(stratum3))
# rm(x2007, x2008, x2009, x2010, x2012, x2014, x2016, x2018)
# rm(above3, below3)

# ats.wide %>% group_by(year) %>% summarize(s1=sum(stratum1),
#                                          s2=sum(stratum2),
#                                           s3=sum(stratum3)) %>%
#   pivot_longer(-year)  %>%
#   ggplot(aes(year, value, color=name)) + geom_line()
# ggplot(ats.wide, aes(log(stratum1+.01), log(.01+stratum3))) + geom_point()
g <- ats.wide %>% 
  ggplot(aes(lon, lat, color = log(stratum1))) +
  facet_wrap("year") + 
  geom_jitter(width = .1, height = .1, size = .5, alpha = .15) +
  scale_color_viridis(option = "magma")
ggsave(here(wd, "processing", "below3_density_map.png"), g, width = 10, height = 7)

# Stratum1 gets added below when creating the final densities

# The structure of the data is done, now we just need to filter "bad" rows.

# Do the final row filtering --------------------------------------------------
#' Nate says to drop the ones with a surface of <25m since there are few and 
#' probably not right anyway and above 450m are where there was no real bottom 
#' detected (dealt with later), and also any negative grounds since they are an error
message("Filtering bad rows...")
message(paste("number of rows before:", nrow(ats.wide)))

# ### Quick checks
# g <- dat0 %>% group_by(key2) %>%
#   summarize(lon=lon[1], lat=lat[1], year=year[1]) %>%
#   ggplot(aes(lon, lat)) + geom_point() + facet_wrap('year')
# ggsave('raw_sampling_locations.png', g, width=9, height=6)

# dat0$hour <- hour(dat0$date) + minute(dat0$date)/60
# dat0$month <- month(dat0$date)
# g <- ggplot(dat0, aes(hour)) + geom_histogram() + facet_grid(.~year)
# ggsave('hour_of_day.png', g, width=9, height=7, dpi=)
# dat0$hour <- dat0$month <- NULL

#' Now check for bad observations (rows). The actual filtering of these happens 
#' at the very end. This helps with merging the below3m data set below, and is 
#' more transparent to group them together. So this section only explores the 
#' weird thing. Based on Nate's suggestion we should remove some of these 
#' extreme distances as they may be real but are strange and a small percentage
write.csv(ats.wide[which(!(ats.wide$dist < 1 & ats.wide$dist > .85)), ], 
          here(wd, "processing", "bad.dist.csv"))
png(here(wd, "processing", "distances.png"), width = 7, height = 5, units = "in", res = 500)
ats.wide %>% 
  group_by(key3) %>% 
  mutate(dist = dist[1]) %>%
  ungroup() %>% 
  pull(dist) %>% 
  hist(breaks = 250) #ecdf() %>% plot()
ats.wide <- ats.wide %>% ungroup()
dev.off()
## There are some weird heights and durations... drop these?
write.csv(file = here(wd, "processing", "weird_heights.csv"), 
          ats.wide[which(ats.wide$bottom < 0), ])
write.csv(file = here(wd, "processing", "big_heights.csv"), 
          ats.wide[which(ats.wide$top > 1000), ])
write.csv(file = here(wd, "processing", "weird_durations.csv"), 
          ats.wide[which(ats.wide$duration < 0), ])
g <- ggplot(ats.wide, aes(log(surface))) + 
  geom_histogram(bins = 100) +
  geom_vline(xintercept = log(450)) + 
  xlab("log surface height")
ggsave(here(wd, "processing", "surface_heights.png"), g, width = 12, height = 6, dpi = 500)
write.csv(ats.wide[which(ats.wide$surface < 16), ], 
          file = here(wd, "processing", "shallow_spots.csv"))
ats.wide <- ats.wide %>%
  filter(dist < 1 & dist > .85 & ground > 0 & ground < 1 & surface < 450)
message(paste("number of rows after:", nrow(ats.wide)))

# SNW: Save the data with three separate strata (0.5 - 3m, 3m - 16m, >16m)
saveRDS(ats.wide, here(wd, "at_3strata.rds"))

## If using <3m add the below3 density to stratum2
message("Converting density to kg/km^2 and calculating AT2 & AT3 ...")

## Convert to kg/km^2 from kg/nm^2
if(strata == 2) {
  message("Combining strata 1 (0.5-3m) and 2 (3-16m)")
  ats.wide$strata2 <- (ats.wide$stratum1 + ats.wide$stratum2) / 1.852^2
} 

if(strata == 3) {
  ats.wide$strata1 <- ats.wide$stratum1 / 1.852^2
  ats.wide$strata2 <- ats.wide$stratum2 / 1.852^2
}
ats.wide$strata3 <- ats.wide$stratum3 / 1.852^2
ats.wide <- select(ats.wide, -stratum1, -stratum2, -stratum3)

message("Saving full data set to ats_full.csv before subsampling..")
if(b2 == 3) {
  write.csv(ats.wide, file = here(wd, "processing", "ats_full_3.csv"))
} else {
  write.csv(ats.wide, file = here(wd, "processing", "ats_full_16.csv"))
}

## Now do the subsampling of data by averaging over multiple intervals to reduce the data set size
ats.wide$transect <- stringr::str_split(ats.wide$key3, "_",
                                        simplify = TRUE)[,2] %>% 
  as.numeric()
idist.fn <- function(lat, lon){
  #' This function calculates the distance of sequential points along a transect 
  #' in kilometers, assuming the first point is 0. This can be then used to 
  #' detect gaps between points for example if there's an island they had to go around
  n <- length(lat)
  idist <- c(0, raster::pointDistance(cbind(lon[-n], lat[-n]),
                                      cbind(lon[-1], lat[-1]),
                                      lonlat = TRUE) / 1000)
  idist
}

#' The tricky part here is calculating transect2. The cumsum(idist>1) is a 
#' cumulative sum over a vector of 0/1 representing whether the distance is too 
#' big (> 1 km). If they are all close this is a vector of zeroes. If not you'll 
#' get two distinct values in transect2, essentially creating two groups. The 
#' [transect + .1] part is to avoid duplicating values so they can cleanly be 
#' converted into a factor and grouped. Think of transect2 as a new ID showing 
#' transect broken into pieces within each one there are no big gaps.
tmp3 <- ats.wide  %>% 
  group_by(year, transect) %>%
  ## Break transects apart if too big of gaps
  mutate(idist = idist.fn(lat, lon),
         transect2 = transect + .1 * cumsum(idist > 1),
         tadd = .1 * cumsum(idist > 1)) %>% 
  ungroup() %>%
  #' For each transect group sequential points into groups of size [n], igroup 
  #' is a factor representing chunks to be averaged together
  group_by(year, transect2) %>%
  #' This is the part where we group into 20 consecutive, this could be a 
  #' different number change it here
  mutate(igroup = cumsum(1:n() %% 20 == 0)) %>% 
  ungroup() %>%
  mutate(igroup = fct_shuffle(factor(igroup))) # shuffle for plotting
## Now for each set of points take averages
ats.wide2 <- tmp3 %>%
  group_by(year, transect2, igroup) %>%
  summarize(ni = n(), 
            lat = mean(lat), 
            lon = mean(lon), 
            transect = transect[1],
            strata1 = mean(strata1),
            strata2 = mean(strata2), 
            strata3 = mean(strata3),
            surface = mean(surface),
            ground = mean(ground)) %>% 
  ungroup()
## ni is the number of points in each average, should be 20 but sometimes fewer and we filter below

stopifnot(all.equal(sum(ats.wide2$ni), nrow(tmp3)))
message("Filtering out interval sets with fewer than 5 intervals")
message(paste("number of intervals before:", sum(ats.wide2$ni)))
ats.wide2 <- ats.wide2 %>% filter(ni >= 5)
message(paste("number of intervals after:", sum(ats.wide2$ni)))
nrow(ats.wide2)
pdf(here(wd, "processing", "subsampled.pdf"), width = 6, height = 7)
for(yy in unique(ats.wide2$year)) {
  g <- ggplot(filter(tmp3, year == yy), aes(lon, lat, color = igroup)) +
    geom_point(size = .7, stroke = 0,shape = 16) +
    geom_point(data = filter(ats.wide2, year == yy), 
               aes(lon, lat, group = transect2, color = igroup),
               size = 1.5) +
    theme(legend.position = "none") +
    labs(title = yy)
  print(g)
}
dev.off()

## used this code to explore the different grouping cutoff
# yy <- 2008
# nrow(filter(ats.wide2, year==yy))
# zzz <- filter(tmp3, year==yy)
# ggplot(zzz, aes(lon, lat, color=igroup))+
#   geom_point(size=.7, stroke=0,shape=16)+
#   geom_point(data=filter(ats.wide2, year==yy), aes(lon, lat, group=transect2, color=igroup),
#              size=1) +
#   theme_bw() + theme(legend.position = "none") +
#   labs(x='Longitude', y='Latitude')
# ggsave('subset_50.png', g, width=7, height=4, dpi=800)


## Melt it for some exploratory ggploting
message("Making exploratory plots...")
ats.long <- ats.wide2 %>% 
  gather(key = strata, value = density, strata1, strata2, strata3) %>% 
  mutate(strata = factor(strata))
ats.long$strata <- fct_recode(ats.long$strata, 
                              "0.5-3m" = "strata1",
                              "3-16m" = "strata2", 
                              "16+" = "strata3")

## Does this match the original data??
g <- ats.long %>%
  ggplot(aes(lon, lat)) + 
  geom_point(size = .2, alpha = .5) + 
  facet_wrap("year")
ggsave(here(wd, "processing", "processed_sampling_locations.png"), g, width = 9, height = 6)


## Some quick exploratory plots -----------------------------------------------
g <- ats.long %>% 
  group_by(year,strata) %>%
  summarize(pct.zero = mean(density == 0), count = length(density)) %>%
  gather(variable, value, -year, -strata) %>%
  ggplot(aes(year, value, color = strata)) + 
  geom_line() +
  facet_wrap("variable", scales = "free")
ggsave(here(wd, "processing", "zeores_by_year.png"), g, width = 7, height = 6, dpi = 500)

g <- ggplot(subset(ats.long, density > 0), aes(log(density), fill = strata)) +
  geom_histogram(bins = 50, position = "identity", alpha = .5) +
  facet_wrap("year", scales = "free_y") 
ggsave(here(wd, "processing", "density_hist_annual.png"), g, width = 9, height = 6, dpi = 500)

g <- ggplot(subset(ats.long, density > 0), aes(log(density), fill = strata)) +
  geom_histogram(bins = 50, position = "identity", alpha = .5) 
ggsave(here(wd, "processing", "density_hist.png"), g, width = 7, height = 3.5, dpi = 500)

jit <- .2
g <- ggplot(subset(ats.long, density>0), aes(lon, lat, color=log(density))) +
  geom_jitter(width = jit, height = jit, alpha = .5, size = .5) + 
  facet_grid(strata ~ year) +
  scale_color_viridis_c()
ggsave(here(wd, "processing", "density_map.png"), g, width = 15, height = 7, dpi = 500)
       
g <- ggplot(subset(ats.long, density == 0), aes(lon, lat)) +
  geom_jitter(width = jit, height = jit, alpha = .25, size = .2) + 
  facet_grid(strata ~ year)
ggsave(here(wd, "processing", "zeroes_map.png"), g, width = 15, height = 7, dpi = 500)

g <- arrange(ats.long, desc(surface)) %>% 
  filter(strata == "16+") %>%
  ggplot(aes(lon, lat, size = surface < 50, color = surface < 50)) +
  geom_point(alpha = .25) + 
  facet_wrap("year")
ggsave(here(wd, "processing", "shallow_map.png"), g, width = 12, height = 6, dpi = 500)

g <- arrange(ats.long, surface) %>% 
  filter(strata == "16+") %>%
  ggplot(aes(lon, lat, size = surface > 300,  color = surface > 300)) +
  geom_point(alpha = .25) + 
  facet_wrap("year") 
ggsave(here(wd, "processing", "deep_map.png"), g, width = 12, height = 6, dpi = 500)

# Write final version of the data
ats.final <- ats.wide2 %>% 
  select(-transect2, -ni, -igroup, -transect, -ground)

if(b2==3){
  message("Writing final output file ats_3.csv to main folder..")
  write.csv(here(wd, "ats_3.csv"), x = ats.final)
} else {
  message("Writing final output file ats_16.csv to main folder..")
  write.csv(here(wd, "ats_16.csv"), x = ats.final)
}
