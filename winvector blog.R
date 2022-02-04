https://win-vector.com/2020/10/28/an-example-of-a-calibrated-model-that-is-not-fully-calibrated/

### CLEAN WORKSPACE AND SOURCE FUNCTIONS ####
# Clear all objects
rm(list=ls())

# Clear all plots
dev.off()

library(wrapr)



d <- build_frame(
  "x1"  , "x2", "y"   |
    1   , 1   , TRUE  |
    1   , 0   , FALSE |
    1   , 0   , TRUE  |
    1   , 1   , FALSE |
    0   , 0   , TRUE  |
    0   , 1   , TRUE  |
    0   , 1   , FALSE |
    0   , 0   , FALSE |
    0   , 0   , TRUE  )
# cat(wrapr::draw_frame(d))

knitr::kable(d)



model <- glm(y ~ x1 + x2, 
             data = d, 
             family = binomial())
summary(model)



d$prediction <- predict(model, 
                        newdata = d, 
                        type = 'response')
knitr::kable(d)



colMeans(d[, qc(y, prediction)]) %.>%
  knitr::kable(.)



for(v in qc(x1, x2)) {
  print(paste0(
    v, ' diff: ', 
    mean(d[[v]] * d$prediction) - mean(d[[v]] * d$y)))
}



cal <- aggregate(y ~ prediction, data = d, FUN = mean)

knitr::kable(cal)



cal_map <- cal$prediction := cal$y
d$calibrated <- cal_map[as.character(d$prediction)]

knitr::kable(d)



colMeans(d[, qc(y, prediction, calibrated)]) %.>%
  knitr::kable(.)






