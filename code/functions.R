# =================== DETERMINING TYPE GROUP========================================== #
cat("Loading functions...")
cat("\n")

# Types Classification
# Type number refers to the 10th base conversion of a strategy
# For instance LLL -> 0 LLM -> 1

class_table <- bind_rows(
    list(type = 0, wide = 'selfish', narrow = 'selfish', wide_o = 'selfish'),
    list(type = 1, wide = 'imp-cond-coop',narrow = 'cond-coop', wide_o = 'imp-cond-coop'),
    list(type = 2, wide = 'imp-cond-coop',narrow = 'cond-coop', wide_o = 'imp-cond-coop'),
    list(type = 3, wide = 'humped',narrow = 'humped', wide_o = 'humped'),
    list(type = 4, wide = 'imp-cond-coop',narrow = 'cond-coop', wide_o = 'imp-cond-coop'),
    list(type = 5, wide = 'perf-cond-coop',narrow = 'cond-coop', wide_o = 'perf-cond-coop'),
    list(type = 6, wide = 'humped',narrow = 'humped', wide_o = 'humped'),
    list(type = 7, wide = 'humped',narrow = 'humped', wide_o = 'humped'),
    list(type = 8, wide = 'imp-cond-coop',narrow = 'cond-coop', wide_o = 'imp-cond-coop'),
    list(type = 9, wide = 'inv-cond-coop',narrow = 'other', wide_o = 'other'),
    list(type = 10, wide = 'inv-humped',narrow = 'other', wide_o = 'other'),
    list(type = 11, wide = 'inv-humped',narrow = 'other', wide_o = 'other'),
    list(type = 12, wide = 'inv-cond-coop',narrow = 'other', wide_o = 'other'),
    list(type = 13, wide = 'uncond',narrow = 'other', wide_o = 'other'),
    list(type = 14, wide = 'imp-cond-coop',narrow = 'cond-coop', wide_o = 'imp-cond-coop'),
    list(type = 15, wide = 'humped',narrow = 'humped', wide_o = 'humped'),
    list(type = 16, wide = 'humped',narrow = 'humped', wide_o = 'humped'),
    list(type = 17, wide = 'imp-cond-coop',narrow = 'cond-coop', wide_o = 'imp-cond-coop'),
    list(type = 18, wide = 'invcond',narrow = 'other', wide_o = 'other'),
    list(type = 19, wide = 'inv-humped',narrow = 'other', wide_o = 'other'),
    list(type = 20, wide = 'inv-humped',narrow = 'other', wide_o = 'other'),
    list(type = 21, wide = 'inv-cond-coop',narrow = 'other', wide_o = 'other'),
    list(type = 22, wide = 'inv-cond-coop',narrow = 'other', wide_o = 'other'),
    list(type = 23, wide = 'inv-humped',narrow = 'other', wide_o = 'other'),
    list(type = 24, wide = 'inv-cond-coop',narrow = 'other', wide_o = 'other'),
    list(type = 25, wide = 'inv-cond-coop',narrow = 'other', wide_o = 'other'),
    list(type = 26, wide = 'uncond',narrow = 'other', wide_o = 'other')
)



# Function to convert conditional response names
#10: 10 base number
#3: 3 base number in vector form
#L: Letter base
cat("convert_type")
cat("\n")

convert_type <- function (type, conv = c("10t3","3t10","10tL","Lt10","3tL","Lt3") ) {
    # Control inputs ------------------------------------------------------------------------
    if(!(length(conv)==1)) {
        stop('provide conversion method')
    }
    if(!(conv %in% c("10t3","3t10","10tL","Lt10","3tL","Lt3"))) {
        stop('conversion method wrong')
    }
    if(conv == "3t10" | conv == "3tL" ) {
        if(any(type > 2) | any(type<0) ){
            stop('wrong type type provided. should  3 items between 0 and 2')
        }
    }
    if(conv == "10t3" | conv == "10tL" ) {
        if(type > 26 | type < 0){
            stop('wrong type number provided. should be between 0 and 26')
        }
    }

    if(conv == "Lt3" | conv == "Lt3" ) {
        stop('conversions from letters are not implemented yet')
        # TO DO: Conversions from L
    }
    #-----------------------------------------------------------------------------
    type_labels <- c("L","M","H")

    if (conv=="3t10") {
        return(digits2number(rev(type),base=3))
    }


    if (conv=="10t3") {
        baseconv<-rev(number2digits(type,base=3))
        if (length(baseconv)==3){
            return(baseconv)
        }
        if (length(baseconv)==2){
            return(c(0,baseconv))
        }
        if (length(baseconv)==1){
            return(c(0,0,baseconv))
        }
        if (length(baseconv)==0){
            return(c(0,0,0))
        }
    }


    if (conv=="3tL") {
        type <- type+1
        return (paste(type_labels[type[1]],type_labels[type[2]],type_labels[type[3]],sep=""))
    }


    if (conv=="10tL") {
        if ("factor" %in% class(type)) {
            type = as.numeric(as.character(type))
        }
        type<-convert_type(type,conv="10t3")
        type<-convert_type(type,conv="3tL")
        return(type)
    }

}


# for bulk conversion
# it would be nicer that above function detects the form and does accordingly
# however it is not trivial as some of the inputs(3,L) are in vector form already
cat("convert_type_list")
cat("\n")

convert_type_list  <- function(types, conv = c("10t3","3t10","10tL","Lt10","3tL","Lt3")) {
    length_types  <- length(types)
    output_list <-  list(rep(NA,length_types))
    for (i in 1:length_types) {
        output_list[[i]] = convert_type(types[[i]],conv)
    }
    return(output_list)
    }

cat("convert_type_vector")
cat("\n")

convert_type_vector  <- function(types, conv = c("10t3","3t10","10tL","Lt10","3tL","Lt3")) {
    length_types  <- length(types)
    output_vector <-  rep(NA,length_types)
    for (i in 1:length_types) {
        output_vector[[i]] = convert_type(types[[i]],conv)
    }
    return(output_vector)
    }


cat("classify_type")
cat("\n")

classify_type<- function(typeno, base3 = FALSE, classification = "wide"){
        if (base3 == TRUE){
        typeno <- convert_type(typeno,"3t10")
    }
        return(filter(class_table, type == typeno)[1,classification][[1]])
        }


cat("classify_type_vector")
cat("\n")

classify_type_vector  <- function(types, classification= "wide") {
# we classify and also convert to factor with a specific order. the mai
# the main reason for that is we need to be consistent about the order
# to have proper graphs

   # base3 not possible here
    length_types  <- length(types)
    output_vector <-  rep(NA,length_types)
    for (i in 1:length_types) {
        output_vector[[i]] = classify_type(types[[i]], base3=FALSE, classification)
    }
    output_vector <- factor(output_vector, levels = get_label_table(classification = classification)[,"shortname"])
    return(output_vector)
    }


cat("get_label_table")
cat("\n")
cat("    classifications: wide, wide_o, narrow")
cat("\n")

get_label_table <- function(classification = "wide") {
  pal_red<-"#C0392B"
  pal_blue<-"#2980B9"
  pal_dblue<-"#1e6391"
  pal_purple<-"#9B59B6"
  pal_green<-"#27AE60"
  pal_dgreen<-"#085b2b"
  pal_yellow<-"#F1C40F"
  pal_orange<-"#E67E22"
  pal_pink <- "#ef39a3"
  pal_gray  <- "#888888"

  if (classification == "wide") {
  label_mat<-rbind(
      c(1,"selfish","Selfish",pal_red),
      c(2,"imp-cond-coop","Imp.Cond.Coop.",pal_green),
      c(3,"perf-cond-coop","Perf.Cond.Coop.",pal_dgreen),
      c(4,"humped","Hump-Shaped",pal_yellow),
      c(5,"inv-humped","Inv. Hump-Shaped",pal_pink),
      c(6,"uncond","Unconditional",pal_blue),
      c(7,"inv-cond-coop","Inv. Cond.Coop.",pal_purple)
  )
  }

if (classification == "wide_o") {
  label_mat<-rbind(
      c(1,"selfish","Selfish",pal_red),
      c(2,"imp-cond-coop","Imp.Cond.Coop.",pal_green),
      c(3,"perf-cond-coop","Perf.Cond.Coop.",pal_dgreen),
      c(4,"humped","Hump-Shaped",pal_yellow),
      c(5,"other","Other",pal_gray)
  )
  }

  
        if (classification == "narrow") {
  label_mat<-rbind(
      c(1,"selfish","Selfish",pal_red),
      c(2,"cond-coop","Cond.Coop.",pal_green),
      c(3,"humped","Hump-Shaped",pal_yellow),
      c(5,"other","Other",pal_gray)
  )
        }


  colnames(label_mat)<-c("code","shortname","longname","color")
  return(label_mat)
}

cat("labelize_class")
cat("\n")

labelize_class  <- function(classnames, output=c("color", "longname"),classification="wide") {

    label_table  <- get_label_table(classification = classification)
    length_classnames  <- length(classnames)
    output_table  <- character(length(classnames))
    
    for (i in 1:length_classnames) {
       output_table[i]  <- label_table[label_table[,'shortname'] == classnames[[i]], output] 
    }
    return(output_table)
    }

    

cat("treatment_convert")
cat("\n")

treatment_convert <- function(TreatmentNo) {
  TreatmentNames <- c("CondInfo","NoCondInfo")
    return(TreatmentNames[TreatmentNo])
}



# Save the last plot as pdf
cat("pdf_last_plot")
cat("\n")

pdf_last_plot <- function(filename, fig_scale = 1, width = 7, height = 6, crop = FALSE) {
  filename_with_path <- file.path(".","figs",paste(filename,".pdf", sep = ""))
  cat("Saving", filename_with_path , "\n")
  ggsave(filename_with_path,
         device = "pdf",
         scale = fig_scale,
         width = width,
         height = height,
         units = "in"
#         dpi = 300
         )
  if (crop == TRUE ){
      system(paste("pdfcrop --margins '0 0 0 0'",filename_with_path,filename_with_path)) # linux only, requires pdfcrop
      }
}

cat("pdf_last_plot")
cat("\n")
png_last_plot <- function(filename, fig_scale = 1, crop = FALSE) {
  filename_with_path <- file.path(".","figs",paste(filename,".png", sep = ""))
  cat("Saving", filename_with_path , "\n")
  ggsave(filename_with_path, device = "png",scale = fig_scale)
  if (crop == TRUE ){
      system(paste("pdfcrop --margins '0 0 0 0'",filename_with_path,filename_with_path)) # linux only, requires pdfcrop
      }
}


generate_switch_vector  <- function(history) {
    length_history  <- length(history)
    switch_vector = numeric(length_history-1)
    for (i in 1:(length_history-1)) {
        switch_vector[[i]]  <- dplyr::if_else(history[[i]] == history[[i+1]],0,1)
    }
    return(switch_vector)
}

cat("get_mode")
cat("\n")

get_mode <- function(x) {
  # credits: @digEmAll
  # https://stackoverflow.com/questions/2547402/is-there-a-built-in-function-for-finding-the-mode
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


cat("get_first")
cat("\n")

get_first  <- function(vector) {
return(vector[[1]])
    }

cat("vectorize_history \n")
cat("    -> used internally. converts history list to a vector")
cat("\n")

vectorize_history <- function(...) {
    input_list <- list(...)
    output <- unlist(input_list, use.names = FALSE)
#    output <- paste(input_list, collapse = '')
    return(output)
}


cat("measure_stability")
cat("\n")

measure_stability  <- function(history, method = "psi") {
   # Measures stability for a given history of
   # methods: psi: psi-stability
   #          andreozzi : andreozzi measure of deviation from expected switches
   #          mode: if mode is equal to most common choice
   #          mode_lasthalf: if mode is equal to the choice that is in last half
          #choiceset <- unique(choiceset) # Not needed. see below.should be unique anyways but just to be sure
    length_history  <- length(history)
    #length_choiceset  <- length(choiceset)  # We don't need it anymore as we use length
    unique_history  <- unique(history) 
    num_unique_history  <- length(unique_history)
    switch_vector  <- generate_switch_vector(history)
    sum_switch  <- sum(switch_vector)
    stability <- NA
    if (method == "psi") {
        stability <- 1 - (num_unique_history/length_history) * (sum_switch / (length_history-1))
    }
      
    if (method == "andreozzi") {
     expected_switches  <- (length_history-1) * (num_unique_history-1)/(num_unique_history)
     stability <- sum_switch - expected_switches
    }
  
    if (method == "mode") {
        first_choice  <-  history[[1]]
        mode_choice  <-  get_mode(history)
        if (length(mode_choice) != 1) {
          stability  <- 0
          }
        else {
          stability  <-  ifelse(first_choice %in% mode_choice,1,0)
        }
    }

    if (method == "mode_lasthalf") {
        first_choice  <-  history[[1]]
        half_index  <- ceiling(length_history/2)
        mode_choice_lasthalf  <-  get_mode(history[half_index:length_history])
        if (length(mode_choice_lasthalf) != 1) {
          stability  <- 0
          }
        else {
        stability  <-  ifelse(first_choice %in% mode_choice_lasthalf,1,0)
        }
    }
    
    return(stability)
}
