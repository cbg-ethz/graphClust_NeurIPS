for(seedy in 1:100){
    filecommand<-paste("\nseednumber<-",seedy,"\nsource(\"cluster_script.R\")\n",sep="")
    write(filecommand,file=paste("euler_scripts/cluster_euler_","seed",seedy,".R",sep=""))
}
