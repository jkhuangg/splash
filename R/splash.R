# this function takes in a column num of an enum col and returns
# a vector of its options
enum2dom = function(colnum, domain_files) {
  idx = toString(colnum)
  domFile = domain_files[[idx]]
  eopt = readLines(domFile)
  eopt
}

# this function takes all the possible inputs for 'Enum'
# and puts them into a string
eopt2str = function(eopt) {
  sz = length(eopt)
  num = 1
  str = ""
  
  for (i in eopt) {
    str = paste0(str, shQuote(i))
    # add comma if not end of vector
    if (num < sz) { str = paste0(str, ", ") }
    num = num + 1
  }
  
  str = trimws(str)
}

#' @export
splash <- function(mojo_fn,
                   path,
                   output_fn="index.Rmd",
                   title=NULL,
                   author=NULL,
                   date=NULL,
                   output=NULL,
                   runtime="shiny") {
  
  # set up work dir, path & output file name
  modelfile = mojo_fn
  fn = output_fn
  setwd(path)

  # Fill in libraries necessary for your h2o code:
  library_input=
  "library(shiny)\nlibrary(h2o)
  "
  ### WRITE YAML TO FILE
  yaml_info=paste(
  "---",
  "\ntitle:", shQuote(title),
  "\nauthor:", shQuote(author),
  "\ndate:", shQuote(date),
  "\noutput:", output,
  "\nruntime:", runtime,
  "\n---")

  write(yaml_info, file=fn)
  # Writing libraries
  write("```{r context='setup',include=FALSE, cache=FALSE}", file=fn, append=TRUE)
  write(library_input, file=fn, append=TRUE)
  write("```", file=fn, append=TRUE)
  # Now is start of r code:
  write("\n```{r echo=FALSE}", file=fn, append=TRUE)

  ### Header
  data =
  "wellPanel(style = \"background: #FFF;\",
    h4('Please enter the following information.'),
    flowLayout("

  write(data, file=fn, append=TRUE)


  ### GETTING NAMES AND TYPES OF EACH H2O OBJ COLUMN
  # YANK the domain info out of model.ini
  unzip(modelfile, files="model.ini")
  f = ini::read.ini("model.ini")
  tot_collen = as.integer(f$info$n_columns) - 1
  tot_domain = as.integer(f$info$n_domains)
  x = readLines("model.ini")

  num = 1
  v = numeric(0)
  colname = c()
  while(!is.na(x[num])) {
    y = x[num]
    if (y == "[columns]") {
      num = num + 1
      for (i in 1:tot_collen) {
        colname=c(colname, x[num])
        num = num + 1
      }
      # minus the extra line here to avoid going one line too far
      # (look at num + 1 at the end of this entire loop)
      num = num - 1
    } else if (y == "[domains]") {
      num = num + 1
      for (i in 1:tot_domain) {
        line = x[num]
        colnum = as.numeric(strsplit(line, split=":")[[1]][1])
        v = c(v, colnum+1)
        num = num + 1
      }
      # minus the extra line here to avoid going one line too far
      # (look at num + 1 at the end of this entire loop)
      num = num - 1
    }
    num = num + 1
  }

  # create list of column types
  coltype = rep("Numeric", tot_collen)
  for (i in v)
    coltype[i] = "Enum"

  # unzip the domain files
  dfl = c()
  en_collen = length(v) - 1
  for (i in 0:en_collen) {
    domain_file=sprintf("domains/d%03d.txt", i)
    dfl=c(dfl, domain_file)
    unzip(modelfile, files=domain_file)
  }

  # maps enum col num. to its domain file
  # maps col name to its type
  names(dfl) = v
  names(coltype) = colname

  # find a way to mark the last column so we don't
  # add  comma at the end of it
  collen = length(colname)

  ### MAIN LOOP
  idx = 1
  for (cn in colname) {
    # get the type of col
    ct = coltype[[cn]]

    # data input
    id = shQuote(cn)
    cn_quoted = shQuote(cn)

    if (ct == "Numeric") {
      # fill in string for numericInput
      data = paste0("\tnumericInput(", id, ", ", cn_quoted, ", value=NA)")
    } else if (ct == "Enum") {
      # stringfy Enum options
      x = enum2dom(idx, dfl)
      opt = eopt2str(x)

      # fill in string for selectInput
      data = paste0("\tselectInput(", id, ", ", cn_quoted, ", ",
                    "\n\t\tc(NA, ", opt, "), ",
                    "\n\t\tselected=NA", ")")
    }

    if (idx < collen) 
      data = paste0(data, ", ")

    # write to file
    write(data, file=fn, append = TRUE)
    idx = idx + 1
  }

  ### End
  data =
  "\t),
  \tbr(),
  \tactionButton('tx_start','Calculate')
  )"

  write(data, file=fn, append=TRUE)

  ### H2O OUTPUT
  data =
  "
  conditionalPanel('input.tx_start>0',
                   wellPanel(id='tx_results',style = \"background: #FFF;\",
                   h3('H2O output: '),
                   h5(dataTableOutput('pred_score'))
                   ))"
  write(data, file=fn, append=TRUE)

  ### H2O CALCULATION
  data =
  "
  # Reactive function to calculate h2o score
  calculate_h2o = reactive({
  \n\t# assign variables"
  write(data, file=fn, append=TRUE)

  # assign variables
  for (cn in colname) {
    data = paste0("\t", cn, " = ")
    ct = coltype[[cn]]
    if (ct == "Numeric") { 
      data = paste0(data, "as.numeric(input$", cn, ")") 
    } else if (ct == "Enum") { 
      data = paste0(data, "input$", cn) 
    }
    write(data, file=fn, append=TRUE)
  }

  data =
  "
  \t# create data frame
  \tx <- data.frame("
  write(data, file=fn, append=TRUE)

  num = 1
  for (cn in colname) {
    sz = length(colname)
    tabs = "\t\t\t"
    if (num < sz) { 
      write(paste0(tabs, cn, ","), file=fn, append=TRUE) 
    } else { 
      write(paste0(tabs, cn, ")"), file=fn, append=TRUE) 
    }
    num = num + 1
  }

  data = paste0(
  "
  \t# call mojo
  \tpred_rf <- h2o.mojo_predict_df(frame=x,
  \t                               mojo_zip_path=", shQuote(modelfile), ",
  \t                               genmodel_jar_path='h2o-genmodel.jar',
  \t                               verbose = F)
  \tpred_rf")

  write(data, file=fn, append=TRUE)
  write("})", file=fn, append=TRUE)

  ### WRITE FUNCTION TO OUTPUT H2O RESULT
  data =
  "
  output$pred_score = renderDataTable(calculate_h2o(),
                                      options=list(paging=FALSE,
                                                   searching=FALSE,
                                                   info=FALSE,
                                                   scrollX=TRUE
                                      ))
  "

  write(data, file=fn, append=TRUE)
  write("```", file=fn, append=TRUE)

  created_jar = FALSE
  # create genmodel_jar, if none
  if (!file.exists("h2o-genmodel.jar")) {
    h2o::h2o.init()
    full_mojo_path = paste0(path, "/", mojo_fn)
    tmp_model = h2o::h2o.import_mojo(full_mojo_path)
    h2o::h2o.download_mojo(model=tmp_model,
                           get_genmodel_jar=TRUE)
    created_jar = TRUE
  }
  
  # some house cleaning
  unlink("domains", recursive=TRUE)
  unlink("model.ini")
  # if we created the tmp file, delete 
  if (created_jar)
    unlink(paste0(tmp_model@model_id, ".zip"))
}
