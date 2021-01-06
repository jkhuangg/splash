library(splash)
library(h2o)

# set current working directory
curr_wd=dirname(rstudioapi::getSourceEditorContext()$path)
setwd(curr_wd)

# create h2o prostate mojo
h2o.init(nthreads = -1)
path <- system.file("extdata", "prostate.csv", package="h2o")
h2o_df <- h2o.importFile(path)
h2o_df$CAPSULE <- as.factor(h2o_df$CAPSULE)
model <- h2o.gbm(y = "CAPSULE",
                 x = c("AGE", "RACE", "PSA", "GLEASON"),
                 training_frame = h2o_df,
                 distribution = "bernoulli",
                 ntrees = 100,
                 max_depth = 4,
                 learn_rate = 0.1)

modelfile <- h2o.download_mojo(model, path= getwd(), get_genmodel_jar=TRUE)

# this is the bare bones version
# splash(modelfile, curr_wd)

# specifying all the parameters
splash(mojo_fn=modelfile,
       path=curr_wd,
       output_fn="index.Rmd",
       title="H2O prostate.csv Demo",
       author="ML4LHS",
       date="01/05/2021",
       output="html_document",
       runtime="shiny"
       )
