# MAIN FUNCTION
# Function stmodel is a parser rebuilding the string and sending it trough bash with the system() function.
# Each argument corresponds to the stmodel option which can be displayed with ./build/stmodel -help
# stmodel output will be write at the location refered by the outland argument

stmodel_local <- function(params=params,inland=inland,outland=outland,x=x,y=y,a=a,b=b,clim_file=clim_file,writeStep=1,disturb=0,transProb=1,global=FALSE){


    stmodel <- paste("./model/build/stmodel -x",  x ,"-y",  y ,"-a",  a, "-b",  b, "-s -c", clim_file, "-p", params,"-i",inland, "-t", writeStep, "-d", disturb,"-e",transProb, ">", outland, "2>/dev/null")

    if(global == TRUE){
    stmodel <- paste("./model/build/stmodel -x",  x ,"-y",  y ,"-a",  a, "-b",  b, "-g -s -c", clim_file, "-p", params,"-i",inland, "-t", writeStep, "-d", disturb,"-e",transProb, ">", outland, "2>/dev/null")
    }

    system(stmodel)

}
