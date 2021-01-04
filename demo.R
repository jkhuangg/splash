library(splash)

fn="h2o_lni_glm.zip"
p="Z:/yiju/splash/mojo_files/"

# this is the bare bones version
splash(fn, p)

# specifying all the parameters
splash(mojo_fn=fn,
       path=p,
       output_fn="index.Rmd",
       title="title1",
       author="Jake Huang",
       date="01/03/2021",
       output="html_document",
       runtime="shiny"
       )
