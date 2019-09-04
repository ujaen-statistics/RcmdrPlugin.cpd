#' cpd Plug-In Utility Functions
#'
#' @name RcmdrPlugin.Utility
NULL
#> NULL



#Hook function .onAttach is called when attach package.
#' @importFrom Rcmdr putRcmdr getRcmdr closeCommander Commander
.onAttach <- function(libname, pkgname){
   if (!interactive()) return()
   Rcmdr <- options()$Rcmdr
   plugins <- Rcmdr$plugins
   if (!pkgname %in% plugins) {
      Rcmdr$plugins <- c(plugins, pkgname)
      options(Rcmdr=Rcmdr)
      if("package:Rcmdr" %in% search()) {
         if(!getRcmdr("autoRestart")) {
            closeCommander(ask=FALSE, ask.save=TRUE)
            Commander()
         }
      }
      else {
         Commander()
      }
   }
}

#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl
#' @importFrom cpd pcbp
#' @export

cbpProbabilities<-function (){
   initial <- getDialog("cbpProbabilities",
                        defaults = list(
                           initialValues=c(1,4),
                           tail = "lower",
                           values = ""))
   initializeDialog(title = "CBP Probabilities")
   frame <- tkframe(top)
   vVar     <- tclVar(initial$values)
   vEnt     <- ttkentry(frame, width = "30",textvariable = vVar)
   bVar     <-tclVar(initial$initialValues[1])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[2])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   tailVar  <- tclVar(initial$tail)
   buttonFrame <- tkframe(top)
   lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
   upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")

   tkgrid(labelRcmdr(frame, text = "Variable value(s)"), vEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
   tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
   tkgrid(frame, sticky = "w")
   tkgrid(buttonFrame, sticky = "w")
   tkgrid.configure(vEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")

   onOK <- function(){
      closeDialog()
      values <- gsub(",+", ",", gsub(" ",",", tclvalue(vVar)))
      if (values==""){
         errorCondition(recall = cbpProbabilities, message = gettextRcmdr("No values specified."))
         return()
      }
      warn  <- options(warn = -1)
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      tail  <- tclvalue(tailVar)
      options(warn)
      #validattions
      if (is.na(b)){
         errorCondition(recall = cbpProbabilities, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = cbpProbabilities, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = cbpProbabilities, message ="gamma not positive" )
         return()
      }
      #action
      doItAndPrint(paste("pcbp(c(",values, "), ", b,", ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
      putDialog("cbpProbabilities", list(initialValues = c( tclvalue(bVar), tclvalue(gammaVar)), tail = tclvalue(tailVar),
                                     values = tclvalue(vVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "qcbp", reset = "cbpProbabilities", apply = "cbpProbabilities")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = vEnt)
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl
#' @importFrom cpd pctp
#' @export

ctpProbabilities<-function (){
   initial <- getDialog("ctpProbabilities",
                        defaults = list(
                           initialValues=c(0.5,1,3.5),
                           tail = "lower",
                           values = ""))
   initializeDialog(title = "CTP Probabilities")
   frame <- tkframe(top)
   vVar     <- tclVar(initial$values)
   vEnt     <- ttkentry(frame, width = "30",textvariable = vVar)
   aVar     <-tclVar(initial$initialValues[1])
   aEnt     <- ttkentry(frame, width = "6",textvariable = aVar)
   bVar     <-tclVar(initial$initialValues[2])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[3])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   tailVar  <- tclVar(initial$tail)
   buttonFrame <- tkframe(top)
   lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
   upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")

   tkgrid(labelRcmdr(frame, text = "Probabilities"), vEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "a"), aEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
   tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
   tkgrid(frame, sticky = "w")
   tkgrid(buttonFrame, sticky = "w")
   tkgrid.configure(vEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")

   onOK <- function(){
      closeDialog()
      values <- gsub(",+", ",", gsub(" ",",", tclvalue(vVar)))
      if (values==""){
         errorCondition(recall = ctpProbabilities, message = gettextRcmdr("No values specified."))
         return()
      }
      warn  <- options(warn = -1)
      a     <- as.numeric(tclvalue(aVar))
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      tail  <- tclvalue(tailVar)
      options(warn)
      #validattions
      if (is.na(a)){
         errorCondition(recall = ctpProbabilities, message ="a not specified" )
         return()
      }
      if (is.na(b)){
         errorCondition(recall = ctpProbabilities, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = ctpProbabilities, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = ctpProbabilities, message ="gamma not positive" )
         return()
      }
      if (gamma<=2*a+2){
         errorCondition(recall = ctpProbabilities, message ="gamma not greater than 2a" )
         return()
      }
      #action
      doItAndPrint(paste("pctp(c(",values, "), ", a,", ", b,", ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
      putDialog("ctpProbabilities", list(initialValues = c( tclvalue(aVar), tclvalue(bVar), tclvalue(gammaVar)), tail = tclvalue(tailVar),
                                     values = tclvalue(vVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "qctp", reset = "ctpProbabilities", apply = "ctpProbabilities")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = vEnt)
}




#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl
#' @importFrom cpd qcbp
#' @export

cbpQuantiles<-function (){
   initial <- getDialog("cbpQuantiles",
                  defaults = list(
                     initialValues=c(1,4),
                     tail = "lower",
                     quantiles = ""))
   initializeDialog(title = "CBP Quantiles")
   frame <- tkframe(top)
      qVar     <- tclVar(initial$quantiles)
      qEnt     <- ttkentry(frame, width = "30",textvariable = qVar)
      bVar     <-tclVar(initial$initialValues[1])
      bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
      gammaVar <-tclVar(initial$initialValues[2])
      gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
      tailVar  <- tclVar(initial$tail)
      buttonFrame <- tkframe(top)
         lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
         upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")

      tkgrid(labelRcmdr(frame, text = "Probabilities"), qEnt, sticky = "w", padx = 6)
      tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
      tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
      tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
      tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
      tkgrid(frame, sticky = "w")
      tkgrid(buttonFrame, sticky = "w")
      tkgrid.configure(qEnt, sticky = "w")
      tkgrid.configure(bEnt, sticky = "w")
      tkgrid.configure(gammaEnt, sticky = "w")

   onOK <- function(){
      closeDialog()
      quantiles <- gsub(",+", ",", gsub(" ",",", tclvalue(qVar)))
      if (quantiles==""){
          errorCondition(recall = cbpQuantiles, message = gettextRcmdr("No probabilities specified."))
          return()
      }
      warn  <- options(warn = -1)
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      tail  <- tclvalue(tailVar)
      options(warn)
      #validattions
      if (is.na(b)){
         errorCondition(recall = cbpQuantiles, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = cbpQuantiles, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = cbpQuantiles, message ="gamma not positive" )
         return()
      }
      #action
      doItAndPrint(paste("qcbp(c(",quantiles, "), ", b,", ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
      putDialog("cbpQuantiles", list(initialValues = c( tclvalue(bVar), tclvalue(gammaVar)), tail = tclvalue(tailVar),
                            quantiles = tclvalue(qVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "qcbp", reset = "cbpQuantiles", apply = "cbpQuantiles")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = qEnt)
}


#' @rdname RcmdrPlugin.Utility
#' @importFrom Rcmdr ActiveModel getRcmdr .Tcl
#' @importFrom cpd qctp
#' @export

ctpQuantiles<-function (){
   initial <- getDialog("ctpQuantiles",
                        defaults = list(
                           initialValues=c(0.5,1,3.5),
                           tail = "lower",
                           quantiles = ""))
   initializeDialog(title = "CTP Quantiles")
   frame <- tkframe(top)
   qVar     <- tclVar(initial$quantiles)
   qEnt     <- ttkentry(frame, width = "30",textvariable = qVar)
   aVar     <-tclVar(initial$initialValues[1])
   aEnt     <- ttkentry(frame, width = "6",textvariable = aVar)
   bVar     <-tclVar(initial$initialValues[2])
   bEnt     <- ttkentry(frame, width = "6",textvariable = bVar)
   gammaVar <-tclVar(initial$initialValues[3])
   gammaEnt <- ttkentry(frame, width = "6",textvariable = gammaVar)
   tailVar  <- tclVar(initial$tail)
   buttonFrame <- tkframe(top)
   lowerBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "lower")
   upperrBut <- ttkradiobutton(buttonFrame, variable = tailVar, value = "upper")

   tkgrid(labelRcmdr(frame, text = "Probabilities"), qEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "a"), aEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "b"), bEnt, sticky = "w", padx = 6)
   tkgrid(labelRcmdr(frame, text = "gamma"), gammaEnt, sticky = "w", padx = 6)
   tkgrid(lowerBut, labelRcmdr(buttonFrame, text = "Lower tail"), sticky = "w")
   tkgrid(upperrBut, labelRcmdr(buttonFrame, text = "Upper tail"), sticky = "w")
   tkgrid(frame, sticky = "w")
   tkgrid(buttonFrame, sticky = "w")
   tkgrid.configure(qEnt, sticky = "w")
   tkgrid.configure(bEnt, sticky = "w")
   tkgrid.configure(gammaEnt, sticky = "w")

   onOK <- function(){
      closeDialog()
      quantiles <- gsub(",+", ",", gsub(" ",",", tclvalue(qVar)))
      if (quantiles==""){
         errorCondition(recall = ctpQuantiles, message = gettextRcmdr("No probabilities specified."))
         return()
      }
      warn  <- options(warn = -1)
      a     <- as.numeric(tclvalue(aVar))
      b     <- as.numeric(tclvalue(bVar))
      gamma <- as.numeric(tclvalue(gammaVar))
      tail  <- tclvalue(tailVar)
      options(warn)
      #validattions
      if (is.na(a)){
         errorCondition(recall = ctpQuantiles, message ="a not specified" )
         return()
      }
      if (is.na(b)){
         errorCondition(recall = ctpQuantiles, message ="b not specified" )
         return()
      }
      if (is.na(gamma)){
         errorCondition(recall = ctpQuantiles, message ="gamma not specified" )
         return()
      }
      if (gamma<=0){
         errorCondition(recall = ctpQuantiles, message ="gamma not positive" )
         return()
      }
      if (gamma<=2*a+2){
         errorCondition(recall = ctpQuantiles, message ="gamma not greater than 2a" )
         return()
      }
      #action
      doItAndPrint(paste("qctp(c(",quantiles, "), ", a,", ", b,", ",gamma, ", lower.tail=", tail == "lower", ")", sep = ""))
      putDialog("ctpQuantiles", list(initialValues = c( tclvalue(aVar), tclvalue(bVar), tclvalue(gammaVar)), tail = tclvalue(tailVar),
                                     quantiles = tclvalue(qVar)), resettable = FALSE)
      tkfocus(CommanderWindow())
   }
   OKCancelHelp(helpSubject = "qctp", reset = "ctpQuantiles", apply = "ctpQuantiles")
   tkgrid(buttonsFrame, sticky = "ew")
   dialogSuffix(focus = qEnt)
}

