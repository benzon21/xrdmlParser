xmlfiles = function(name) {
  x <- read_xml(name)
  
  x_list <- as_list(x)
  
  intensities <- x_list[["xrdMeasurements"]][["xrdMeasurement"]][["scan"]][["dataPoints"]][["intensities"]][[1]]
  
  positions <- x_list[["xrdMeasurements"]][["xrdMeasurement"]][["scan"]][["dataPoints"]][["positions"]][["listPositions"]][[1]]
  
  new_values <- function(x) {
    fact <- strsplit(x, " ")
    for(num in fact){
      numbers = as.numeric(num)
    }
    numbers
  }
  
  counts <- as.vector(new_values(intensities))
  pos <- as.vector(new_values(positions))
  
  new_data <- data.frame(counts=counts,positions = pos)
  
  quarts <- c(quantile(counts,0.25),quantile(counts,0.75))
  
  avg.quart <- mean(quarts)
  
  new.count <- max(counts) - avg.quart
  
  return (new.count)
  }

new.files <- list("20180731.xml","20180731_14.xml","20180731_15.xml","20180731_16.xml",
                  "20180731_17.xml", "20180731_18.xml", "20180731_19.xml","20180731_20.xml", "20180731_21.xml", "20180731_22.xml", "20180731_23.xml")

for (a in new.files){
  datas <- xmlfiles(a)
  print(datas)
}
