numacraw<-function(get2=FALSE){
  if(require(png) && require(RCurl)){
    set.seed(Sys.time()) 
    path_back<-paste0(tempdir(),"\\back.png")
    download.file("http://pbs.twimg.com/media/B4AEO4LCEAMjpx7.png"
                , destfile = path_back,mode="wb")
    path_front<-paste0(tempdir(),"\\front.png")
    download.file("http://pbs.twimg.com/media/B4AB8oDCIAA_04r.png"
                , destfile = path_front,mode="wb")
    vec<-par()$usr
    SIZE<-runif(min=0.4,max=1, 1)
    X<-runif(min=0, max=1, 1)*(vec[2]-vec[1])
    Y<-runif(min=0, max=1, 1)*(vec[4]-vec[3])
    D<-ifelse(runif(min = 0 ,1)>0.3, path_front, path_back)
    if(!get2){
      rasterImage(readPNG(D)
                  ,X-SIZE*(vec[2]-vec[1])/2
                  ,Y-SIZE*(vec[4]-vec[3])/2
                  ,X+SIZE*(vec[2]-vec[1])/2
                  ,Y+SIZE*(vec[4]-vec[3])/2
      )
    }else{
      rasterImage(readPNG(D)
                  ,X-SIZE*(vec[2]-vec[1])/2
                  ,Y-SIZE*(vec[4]-vec[3])/2
                  ,X+SIZE*(vec[2]-vec[1])/4
                  ,Y+SIZE*(vec[4]-vec[3])/2
      )
      rasterImage(readPNG(D)
                  ,X-SIZE*(vec[2]-vec[1])/4
                  ,Y-SIZE*(vec[4]-vec[3])/2
                  ,X+SIZE*(vec[2]-vec[1])/2
                  ,Y+SIZE*(vec[4]-vec[3])/2
      )
    }
  }else{
    stop("Package png and RCurl are required")
  }
}

plot(1:40)
numacraw()

plot(1:10)
numacraw(get2=TRUE)
