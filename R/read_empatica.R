#' Read data from an Empatica device.
#'
#' @param filename The name of the zip file containing the Empatica data
#' @param header.only Should only the header information, but no signal data, be read. Boolean. Default is FALSE.
#'
#' @return A recording structure (a list) containing the data.
#'
#' @export
read.empatica <- function(filename, header.only = FALSE) {

    ## Create empty recording
    recording <- new_recording()

    
    ## --------------------------------------------------
    ## Available signals
    ## --------------------------------------------------
    exdir <- tempdir()
    unzip(zipfile = filename, exdir = exdir)

    filelist <- list.files(exdir, pattern = "*.csv", full.names = TRUE)

    out <- list()
    for (f in filelist) {
        signal_name <- tolower(gsub(".csv", "", basename(f)))

        signal_names <- NULL

        if (signal_name == "acc")
            signal_names <- c("acc_x", "acc_y", "acc_z")
        if (signal_name == "bvp")
            signal_names <- c("bvp")
        if (signal_name == "eda")
            signal_names <- c("eda")
        if (signal_name == "hr")
            signal_names <- c("hr")
        if (signal_name == "temp")
            signal_names <- c("temp")

        if (signal_name == "ibi") {
            signal_names <- c("ibi")
            out          <- c(out, read_empatica_ibi(f))
            signal_names <- NULL
        }

        if (signal_name == "tags") {
            recording$events <- read_empatica_events(f)
            signal_names     <- NULL
        }


        if (! is.null(signal_names))
            out <- c(out, read_empatica_gen(f, signal_names))
    }


    header <- read_header(filelist[[1]])

    ## Store signal data
    recording$signal <- out

    ## Store starting and stopping time
    if (length(names(recording$signal)) > 1)
        recording$properties$length <- rev(recording$signal[[names(recording$signal)[1]]]$t)[1]
    else
        recording$properties$length <- 0

    recording$properties$time.start.raw <- header
    recording$properties$time.start     <- as.POSIXct(header, origin="1970-01-01")

    recording$properties$time.stop.raw  <- header + recording$properties$length
    recording$properties$time.stop      <- as.POSIXct(header, origin="1970-01-01") + as.difftime(recording$properties$length, units = "secs")

    ## Handle events
    recording$events$timedelta <- as.numeric(difftime(recording$events$timestamp, recording$properties$time.start, units = "secs"))

    ## Set device information
    recording$properties$device.type    <- "Empatica"
    recording$properties$device.version <- "E4"
    
    ## Return recording
    recording

}
