############################################################
### Load data first
### Data source readme: http://finmath.stanford.edu/~tibs/tusher/readme
############################################################

### Set datadir
datadir <- '~'

### Pull data
datasource <- 'http://finmath.stanford.edu/~tibs/tusher/normave.txt'
download.file(datasource, file.path(datadir, 'normave.txt'))
colnams <- c('genenames', 'geneid', 'U1A', 'U1B', 'I1A', 'I1B', 'U2A', 'U2B', 'I2A', 'I2B')
rawdat <- read.table(file.path(datadir, 'normave.txt'), col.names = colnams)

### Organize analysis data
dat <- list(
  x = as.matrix(rawdat[, 3:10]),
  y = rep(1:2, each = 4),
  genenames = rawdat$genames,
  geneid = rawdat$geneid,
  logged2 = TRUE
)

############################################################
### Significance analysis of microarrays
############################################################

### Set up
delta <- 0.4
source(file.path(datadir, 'samr_gadget.R'))

### Model 1
modelout <- runmodel(dat)
samr.plot(modelout, delta)

### Model 2
dat$y <- rep(1:2, times = 4)
modelout <- runmodel(dat)
samr.plot(modelout, delta)

### Model 3
dat$y <- rep(1:2, each = 2, times = 2)
modelout <- runmodel(dat)
samr.plot(modelout, delta)


