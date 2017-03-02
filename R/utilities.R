#' Create a data structure containing data and sampling rate from the Empatica data.
#'
#' @param data A data frame with the data.
#' @param samplingrate The sampling rate in Hz of the signal, if applicable. Default is \code{NULL}.
#'
#' @return The header information (timestamp).
#'
#' @keywords internal
organise_data <- function(data, samplingrate = NULL) {
    out         <- vector(mode = "list", length = ncol(data))
    if (! is.null(samplingrate))
        time_vector <- seq.int(0, (nrow(data) - 1)) / samplingrate
    else
        time_vector <- NULL

    for (i in seq.int(ncol(data))) {
        signal_name          <- colnames(data)[i]
        out[[i]]$data         <- data[,i]
        out[[i]]$t            <- time_vector
        out[[i]]$samplingrate <- samplingrate
        ## out[[i]]$unit         <- ""
        ## out[[i]]$signaltype   <- ""
    }

    names(out) <- colnames(data)
    out

}


#' Read header information from an Empatica signal file.
#'
#' @param filename The name of an Empatica signal file.
#'
#' @return The header information (timestamp).
#'
#' @keywords internal
read_header <- function(filename) {
    scan(filename, nmax = 1, what = numeric(), nlines = 1, skip = 0, sep = ",", quiet = TRUE)
}


#' Generic function for reading Empatica signal data.
#'
#' @param filename The name of an Empatica signal file.
#' @param signal_names The names to attach to the signals. Default is \code{NULL}.
#'
#' @return A list containing the signal data, a corresponding time vector and the sampling rate.
#'
#' @keywords internal
read_empatica_gen <- function(filename, signal_names = NULL) {
    t_start      <- scan(filename, nmax = 1, what = numeric(), nlines = 1, skip = 0, sep = ",", quiet = TRUE)
    samplingrate <- scan(filename, nmax = 1, what = numeric(), nlines = 1, skip = 1, sep = ",", quiet = TRUE)
    data         <- read.csv(filename, header = FALSE, sep = ",", skip = 2)

    if (! is.null(signal_names))
        colnames(data) <- signal_names

    organise_data(data, samplingrate)
}


#' Read interbeat interval (IBI) data in the empatica format.
#'
#' @param filename The name of the Empatica signal file with IBI data (\code{ibi.csv}).
#' @param signal_names A list containing names for the data. Default is \code{signal_names = c("time", "ibi")}.
#'
#' @return The header information (timestamp).
#'
#' @keywords internal
read_empatica_ibi <- function(filename, signal_names = c("time", "ibi")) {
    t_start <- scan(filename, nmax = 1, what = numeric(), nlines = 1, skip = 0, sep = ",", quiet = TRUE)
    data    <- read.csv(filename, header = FALSE, sep = ",", skip = 1)

    if (! is.null(signal_names))
        colnames(data) <- signal_names

    out                            <- organise_data(data)

    out[["ibi"]][["t"]]  <- out[["time"]][["data"]]
    out[["ibi"]][["samplingrate"]] <- NULL
    out[["time"]]                  <- NULL

    out
}


#' Read Empatica events (button presses)
#'
#' @param f The name of the file containing the events (\code{tags.csv}).
#' 
#' @return A data frame with the events
#'
#' @keywords internal
read_empatica_events <- function(f) {
    tmp    <- scan(f, quiet = TRUE)

    if (length(tmp) > 0)
        events <- data.frame("id"        = seq.int(length(tmp)),
                             "time_raw"  = tmp,
                             "timestamp" = as.POSIXct(tmp, origin="1970-01-01"),
                             "timedelta" = 0)
    else
        events <- data.frame("id"        = numeric(),
                             "time_raw"  = numeric(),
                             "timestamp" = as.POSIXct(character()),
                             "timedelta" = numeric())

    events
}


#' Return and initialise an empty recording structure.
#' The recording structure is a list.
#'
#' @return An empty recording structure.
#'
#' @export
new_recording <- function() {
    ## Create containers
    recording                   <- list()
    recording$properties        <- list()
    recording$signal            <- list()
    recording$events            <- list()
    recording$properties$header <- list()

    recording$properties$time.start.raw <- NA
    recording$properties$time.start     <- NA
    recording$properties$time.stop.raw  <- NA
    recording$properties$time.stop      <- NA

    ## Set subject and casename information
    recording$properties$subject        <- NA

    ## Information on the data format
    recording$properties$format         <- NA
    recording$properties$format.long    <- NA
    recording$properties$device.type    <- NA
    recording$properties$device.version <- NA

    ## The length of the recording in seconds
    recording$properties$length         <- NA

    recording
}

