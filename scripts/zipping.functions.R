uncompress_linux <- function(filename)
{
  print(filename)
  system(sprintf('uncompress %s',filename))
}

# tries to find 7 zip exe
ExecutableFileName7Zip <- function() {
  executableName <- "C:/Program Files/7-Zip/7z.exe"
  
  if(file.exists(executableName)){
    return(executableName)
  }
  
  #other executable file names and ideas go here ...
  stop("Failed to find 7zip. Edit executableName in update_BOM_RainGrids.R to locate 7z.exe.")
}

# simple function to extract 7zip file
# need to have 7zip installed
Decompress7Zip <- function(zipFileName, outputDirectory=getwd(), delete=FALSE)
{
  if (Sys.info()['sysname'] == "Windows") {
    executableName <- ExecutableFileName7Zip()
    arguments <- paste(sep="",
                     "e ",
                     "\"", zipFileName, "\" ",
                     "\"-o", outputDirectory, "\" ",
                     "")
    #print( arguments)
    RunProcess(executableName, arguments)
    
  } else {
    uncompress_linux(zipFileName)
  }
  if(delete){
    unlink(zipFileName);
  }
}

RunProcess = function(executable, arguments)
{
  command = paste(sep="", "\"", executable,  "\" ", arguments);
  
  print (command)
  
  exitCode = system(command, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL
                    , show.output.on.console = TRUE
                    #, minimized = FALSE
                    , invisible = FALSE
  );
  if(exitCode != 0)
  {
    stop("Process returned error");
  }
  return (exitCode)
}
