library(curl)
library(readxl)
library(ggplot2)
library(data.table)
library(lubridate)
library(magrittr)

base <- c(2022)

######################################
###### CPIH Annual Inflation #########

### download data direct from ONS

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/l522/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### load in the data and clean

data <- read_excel(temp, range="A9:B2000", col_names = FALSE) %>% setDT()

setnames(data, names(data), c("time","cpih"))

data[, year := as.numeric(substr(time,1,4))]
data[, month := substr(time,6,8)]

### keep only annual data and recode to numeric

data <- data[!(month %in% c(NA,"Q1","Q2","Q3","Q4",
                            "JAN","FEB","MAR","APR","MAY","JUN",
                            "JUL","AUG","SEP","OCT","NOV","DEC")),]

cpih <- data[, c("year","cpih")]


######################################
###### RPI Monthly Inflation #########

### download data direct from ONS

temp <- tempfile()
url <- "https://www.ons.gov.uk/generator?format=xls&uri=/economy/inflationandpriceindices/timeseries/chaw/mm23"
temp <- curl_download(url=url, destfile=temp, quiet=FALSE, mode="wb")

### load in the data and clean

data <- read_excel(temp, range="A9:B2000", col_names = FALSE) %>% setDT()

setnames(data, names(data), c("time","rpi"))

data[, year := as.numeric(substr(time,1,4))]
data[, month := substr(time,6,8)]

### keep only monthly data and recode to numeric

data <- data[!(month %in% c(NA,"Q1","Q2","Q3","Q4",
                            "JAN","FEB","MAR","APR","MAY","JUN",
                            "JUL","AUG","SEP","OCT","NOV","DEC")),]

data <- data[, c("year","rpi")]

rpi <- data[, c("year","rpi")]

#########################################
##### Combine and rebase to Jan 2022 ####

inflation <- merge(rpi, cpih, by = c("year"), all = F)

cpih_base <- as.numeric( inflation[year == base, "cpih"] )
rpi_base  <- as.numeric( inflation[year == base, "rpi"] )

inflation[, cpih := (cpih/cpih_base)*100]
inflation[, rpi := (rpi/rpi_base)*100]

inflation <- inflation[, c("year","rpi","cpih")]

inflation <- melt(inflation,
                  id.vars = c("year"),
                  variable.name = "measure",
                  value.name = "index")

usethis::use_data(inflation, overwrite = TRUE)

#### plot the inflation data


ggplot(inflation) +
  aes(y = index, x = year, color = measure) +
  geom_line() +
  geom_point(size = 1) +
  theme_classic() +
  geom_hline(yintercept = 100) +
  labs(y = "Index (base = 2022)",
       x = "",
       title = "RPI and CPIH Inflation 1998 - 2022")





