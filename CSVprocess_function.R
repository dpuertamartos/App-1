csv_process <- function(path,var_label,csv_name) {
  setwd(path)
  files <- list.files(path)
  
  
  for (i in files){
    
    csv <- paste0(as.character(path),"", as.character(i))
    
    df <- read.csv(csv)
    
    
    frame_delete <- 0
    frame_delete <- df[grepl("[0-9]{4}-0002-[0-9]{4}", df$Label),][1,1]
    print(paste0("First frame deleted: ", frame_delete))
    if( !is.na(frame_delete) ){
      df<- df[-(frame_delete:4000),]
    }
    write.csv(df,i, row.names = FALSE)
  }
  
  ##Aquí se puede comprobar en qué frame se ha dividido la célula, si el número es muy bajo o no corresponde a lo esperado se puede comprobar en la máscara de Fiji por qué ha fallado
  
  ##código para cambiar columnas a nombres que entienda Chromo
  
  setwd(path)
  files <- list.files(path)
  print(files)
  
  sapply(files, function(file){
    
    x <- read.csv(file)
    colnames(x) <- c("frame","particle","area","x","y","XM","YM","perimeter","major","minor","Angle","circularity","frame.1","AR","Roundness","convexity")
    ##write.csv(paste0("NewNames_", file),x)
    
    x$label <- var_label
    
    
    x$particle <- strsplit(as.character(x$particle),"_R3D")[[1]][1]
    
    #x$particle
    
    #  pixel <- 0.066
    # # 
    #  x$x <- (x$x * pixel)^2
    #  x$y <- (x$y * pixel)^2
    
    #x_subset <- x
    
    x_subset = x[,c(1,2,3,4,5,9,10,12,16,17)]
    
    
    write.csv(x_subset, file, row.names=F)
    
    
  })
  
  
  
  ## this scripts merges the multiple .csv in the folder with a common string in their name, by default "Results", this can be changed in the line 76
  ruta <- path
  
  files <- list.files(ruta, pattern = "Results")
  total <- NULL
  
  for (csv in files) {
    
    csv_celula <- paste0(as.character(ruta), as.character(csv))
    #this print the location and name of the merged .csv
    print (paste0(csv_celula," ","merged"))
    df <- read.csv(csv_celula)
    
    #une en la variable total todos los df de los distintos csv
    total <- rbind(total,df)
  }
  return(total)
  ##poner nombre de csv global
  
  
}