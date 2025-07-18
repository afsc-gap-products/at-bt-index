#' Script for conducting initial data manipulation steps to get to binned 
#' acoustic data at-depth. These steps are taken from code written by Cole
#' Monnahan.

library(here)
library(dplyr)
library(tidyr)

# Read in data, which was combined & cleaned in at_read_combine.R
dat <- readRDS(here("data", "at", "at_combined.rds"))

## whether to use 0.5-3m from the other data set and add this
## onto the 3-b2 stratum so it goes from 0.5-b2.
use.below3 <- TRUE
b1 <- 3 ## !!do not change!!, this is **NOT** the lower
## breakpoint it has to do with the split AT data sets
b2 <- 16  # SNW: higher breakpoint?

## This is only tested for these two values but theoretically
## should work as long as b2>=0.
stopifnot(b2 %in% c(3,16))
stopifnot(b1==3)

## But some are still duplicates and Nate says to sum across duplicated
## keys. Due to the large size it may be better to break it into two chunks
## just for this calculation. I can't just group by key and sum density b/c
## I need to keep all the column and grouping by the other columns would be
## too slow.
dups <- dat$key[which(duplicated(dat$key))]
tmp <- dat[which(dat$key %in% dups),]
tmp <- tmp[order(tmp$key),]
write.csv(here("data", "at", 'duplicated_keys.csv'), x=tmp)
dups.freq <- group_by(tmp, key) %>%  summarize(count=length(key))
write.csv(here("data", "at", 'duplicated_keys_counts.csv'), x=dups.freq)
table(dups.freq$count)
d1 <- filter(dat, !(key %in% dups))     # unique
d2 <- filter(dat, key %in% dups)        # non unique
d3 <- group_by(d2, key) %>% summarize(year=year[1],
                                      key2=key2[1], key3=key3[1], ground=ground[1], surface=surface[1],
                                      lon=lon[1], lon2=mean(lon),lat=lat[1], pollock=pollock[1],
                                      dist=dist[1], bottom=bottom[1], top=top[1], duration=duration[1],
                                      density=sum(density))
## just check that the longs are the same then delete it
sum(d3$lon != d3$lon2)
d3$lon2 <- NULL
## Now recombine together and check no density was lost
datnew <- bind_rows(d1, d3)
stopifnot(abs(sum(datnew$density)-sum(dat$density))<1e-8)
## should now be no duplicated keys and we can drop the depth layer from
## the key
stopifnot(sum(duplicated(datnew$key))==0)
dat <- datnew
rm(d1,d2,d3, datnew, tmp, dups, dups.freq); gc()

## Create the vertical strata used in model by summing across
## layers.
b <- dat$bottom; t <- dat$top
## with(dat, plot(bottom, top-bottom))
stratum <- rep(NA, len=nrow(dat))
## 0-3m; note this is unused b/c below we use the below3 data
## set. But it is used for some tows that are off the shelf where
## these would then be reliable b/c their height off surface is
## actually >3m.
stratum[b >= 0 & t <= b1] <- 'stratum1'  # 0-3m (unreliable and unused)
stratum[b >= b1 & t <= b2] <- 'stratum2' # 3-EFHm
stratum[b >= b2] <- 'stratum3' # EFH-surface
dat$stratum <- stratum
### None of these are important, not sure why negative but they
## don't have pollock so doesn't matter.
## write.csv(filter(dat, is.na(stratum)), file='bad_strata.csv')
stopifnot(0==mean(is.na(stratum) & dat$density>0))
dat <- filter(dat, !(is.na(stratum))) ## drop the zero densities
## barplot(table(dat$year))
rm(stratum, t, b); gc()

## The real trick here is that we can't simply drop the non-pollock
## and sum across stratum. For one, when there are intervals without
## pollock but with non-pollock, the data has the NP but not the
## pollock, so I need to back-fill those cases as zeroes for
## P. Another weird case is when the hit the shelf and the bottom is
## unknown, so that we have no reference point and the depths are
## incorrect. In this case I need to assume all those fish are in the
## top stratum and backfill zeroes for the bottom two strata. It is
## **very** important to maintain these zeroes as they are real and
## explicitly modeled in VAST.

## There's two cases we should check. First  when the max depth is really
## big, which happens when the survey goes past the slope and cant find the
## bottom and so the software has weird depths. The second is really
## shallow water such that the three depth strata arent even covered.

## So first sum across layers for all unique keys to get the three model
## strata for each location (interval and year) and pollock nonpollock
out <- group_by(dat, key3, stratum, pollock) %>%
  summarize(year=year[1],
            lon=lon[1], lat=lat[1],
            dist=dist[1], surface=surface[1],
            ground=ground[1],  bottom=bottom[1],
            density=sum(density))
## These arent consistent so I will need to fill in zeroes.
table(out$stratum)

## All possible combinations of each stratum for each location have zero
## density.
zeroes <- expand.grid(key4=unique(out$key3), stratum=unique(out$stratum),
                      pollock=c(TRUE,FALSE), density2=0)
zeroes$key4 <- with(zeroes, paste(key4, stratum, pollock, sep='_'))
out$key4 <- with(out, paste(key3, stratum, pollock, sep='_'))
## Merge the real densities into the one populated by zeroes, This is a left join.
tmp <- merge(out[,c('key4', 'density')], zeroes, by='key4', all.y=TRUE)
## Missing values in the real data are now density=NA
stopifnot(nrow(zeroes)-nrow(out) == sum(is.na(tmp$density)))
## Replace NA's with zeroes
tmp$density[is.na(tmp$density)] <- 0
tmp$density2 <- NULL
## check we didn't lose any biomass
stopifnot(isTRUE(all.equal(sum(tmp$density),sum(dat$density))))
## Now tmp has a density for all combinations of strata and locations and
## pollock non pollock.
## We need to merge the location meta data into the new data frame so first
## create a summary DF.
meta <- out[!duplicated(out$key3), c('key3', 'year', 'lon', 'lat',
                                     'dist', 'surface', 'ground')]
tmp$key3 <- plyr::ldply(strsplit(tmp$key4, split='_s'), function(x) x[[1]])[,1]
dat2 <- merge(meta, tmp, by='key3') %>%
  ## Can finally drop the non-pollock stuff
  filter(pollock==TRUE) %>% select(-pollock, -key4)
## consistent now since backfilled with zeroes
x <- as.numeric(table(dat2$stratum))
stopifnot(isTRUE(all.equal(x[1], x[2], x[3])))
## Check we didn't lose any pollock density
stopifnot(isTRUE(all.equal(sum(dat2$density),sum(dat$density[dat$pollock]))))
rm(tmp, zeroes,x); gc()

## Now can deal with issue on the shelf. First make each stratum its own column
s1 <- subset(dat2, surface>450 & stratum=='stratum1')
s2 <- subset(dat2, surface>450 & stratum=='stratum2')
s3 <- subset(dat2, surface>450 & stratum=='stratum3')
## All measured density assumed to be in the top stratum
if(b2==3) {
  ## There is no s2 in this case because s2 represents biomass in 3-3m.
  s3$density <- s1$density+s3$density
  s1$density <- 0
} else {
  s3$density <- s1$density+s2$density+s3$density
  ## so the other two are 0
  s1$density <- s2$density  <-  0
}
## Now bind them back together
dat3 <- rbind(subset(dat2, surface<=450), s1,s2,s3)
## should not have lost anything
stopifnot(nrow(dat2)==nrow(dat3))
stopifnot(sum(dat2$density)== sum(dat3$density))
rm(out, dat, dat2, meta, s3, s2, s1); gc()

## now dat3 is the final data set once we drop the first stratum (0-3m)
## since that is added from a separate source below
above3 <- droplevels(filter(dat3, stratum!='stratum1')) %>%
  ## Add column for merging with below3 data below
  mutate(key=paste(year,round(lat,3), round(lon,3))) %>%
  ## Melt it to have stratum as columns. We then will add stratum1 below
  spread(stratum, density)
## again if b2==3 this fixes errors
if(is.null(above3$stratum2)) above3$stratum2 <- 0
rm(dat3); gc()
write.csv(above3, file = here("data", "at", "above3.csv"), row.names = FALSE)

### This is the end of processing the raw acoustic data. This
### only reliably goes to 3m so stratum1 is not
### reliable. Instead, Nate provided a separate dataset which
### comes from his paper. so I replace stratum1 with this data


