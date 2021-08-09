library(swirl)
#install_course("Getting and Cleaning Data")
#swirl()
mydf <- read.csv(path2csv, stringsAsFactors = FALSE)
dim(mydf)
head(mydf)
library(dplyr)
packageVersion("dplyr")

cran <- tbl_df(mydf)
rm("mydf") 
cran
select(cran, ip_id, package, country)
select(cran, r_arch:country)
select(cran, country:r_arch)
cran
select(cran,-time)
select(cran,-(X:size))
filter(cran,package=="swirl")       
filter(cran, r_version == "3.1.1",country == "US")
filter(cran, r_version <= "3.0.2",country == "IN")
filter(cran, country =="US" | country == "IN") 
filter(cran, size > 100500, r_os == "linux-gnu")

is.na(c(4, 6, NA, 10))
!is.na(c(3, 5, NA, 10))
filter(cran, !is.na(r_version))
cran2 <- select(cran, size:ip_id)
arrange(cran2, ip_id)
arrange(cran2, desc(ip_id))
arrange(cran2, package, ip_id)
arrange(cran2, country,desc(r_version),ip_id)
cran3 <- select(cran,ip_id, package, size)

cran3
mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2 ^10)
mutate(cran3,correct_size = size + 1000)

summarize(cran, avg_bytes = mean(size))


# ---------------------------------------------------------------------
library(dplyr)
cran <- tbl_df(mydf)
rm("mydf")
cran

by_package<- group_by(cran,package)
by_package

summarize(by_package, mean(size))

# Compute four values, in the following order, from
# the grouped data:
#
# 1. count = n()
# 2. unique = n_distinct(ip_id)
# 3. countries = n_distinct(country)
# 4. avg_bytes = mean(size)
#
# A few thing to be careful of:
#
# 1. Separate arguments by commas
# 2. Make sure you have a closing parenthesis
# 3. Check your spelling!
# 4. Store the result in pack_sum (for 'package summary')
#
# You should also take a look at ?n and ?n_distinct, so
# that you really understand what is going on.

pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries =n_distinct(country) ,
                      avg_bytes = mean(size))
quantile(pack_sum$count, probs = 0.99)

top_counts  <- filter(pack_sum, count > 679)
top_counts
View(top_counts)

top_counts_sorted <- arrange(top_counts,desc(count))
View(top_counts_sorted)

quantile(pack_sum$unique, probs = 0.99)
top_unique <- filter(pack_sum, unique > 465)
View(top_unique)


top_unique_sorted <- arrange(top_unique, desc(unique))
View(top_unique_sorted)
#---------------------------------------------------------------------
library(tidyr)
students
gather(students, sex, count, -grade)
students2
res <- gather(students2, sex_class ,count, -grade)
res
separate(res, col = sex_class, into = c("sex", "class"))

students2 %>%
  gather(  sex_class ,count, -grade ) %>%
  separate(col = sex_class, into = c("sex", "class")) %>%
  print

students3

library(readr)
parse_number("class5")
#---------------------------------------------------------------------
library(lubridate)

this_day <- today()
this_day
year(this_day)
wday(this_day)
wday(this_day, label= TRUE)
this_moment <- now()
this_moment
hour(this_moment)

my_date <-ymd("1989-05-17")
my_date
class(my_date)
ymd("1989 May 17")
mdy("March 12, 1975")
dmy(25081985)

ymd("192012") 

ymd("1920-1-2") 

dt1

ymd_hms(dt1)
hms("03:22:14")
dt2
ymd(dt2)

update(this_moment, hours = 8, minutes = 34, seconds = 55)
this_moment

this_moment <- update(this_moment, hours = 10, minutes = 16, seconds = 0)
this_moment

?now

nyc <- now("America/New_York")
nyc

depart <- nyc + days(2)
depart
depart <- update(depart, hours = 17, minutes=34)
depart
arrive <-  depart + hours(15) + minutes(50)

arrive <- with_tz(arrive, tzone = "Asia/Hong_Kong")
arrive

last_time <-mdy("June 17, 2008", tz = "Singapore")
last_time
?interval
how_long <- interval(last_time, arrive)
as.period(how_long)

stopwatch()




