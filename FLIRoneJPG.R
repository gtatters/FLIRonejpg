########### TABLE OF CONTENTS ############################
# 0. Define all libraries and functions
# 1. File Handling - create file loop or manually enter file name
# 2. Import image data to raw and int vectors
# 3. Locate the PNG tag start point.  Derive parameters from PNG Header
# 4. Magicbyte detection of FLIR tags
# 5. Extract Camera calibration values by direct Raw access conversion, using meta tag
# 6. Clean up CamValues, convert timestamp using POSIXlt
# 7. Convert raw from PNG import to temperature using Thermimage 
# 8. Save the temperature data to a PNG file.
# 9. Export plain plot (png file) of the converted thermal image. 
# 10. Composite Plot - if width > height (i.e. landscape style)
# 11, Composite Plot - if width < length (i.e. portrait style)
########### END OF CONTENTS #############################

#0. Define libraries and functions ####
library("hexView")
library("fields")
library("Thermimage")
library("png")
library("R.utils")
library("tools")
flirpalette<-palette.choose("ironbow")
graphics.off()
def.par<-par(no.readonly=TRUE)

#  Functions ###
rawbyte2int<-function(rawbyte, order="big")
  # take a vector byte in character format of raw bytes and convert to integer
{
  if(order=="big")
  {
    msb<-substr(rawbyte, 1, 2) 
    lsb<-substr(rawbyte, 3, 4)
  }
  if(order=="little") 
  {
    msb<-substr(rawbyte, 3, 4)
    lsb<-substr(rawbyte, 1, 2)
  }
  hex.val<-paste("0x", paste(msb, lsb, collapse="", sep=""), sep="")
  unix.val<-as.integer(hex.val)
  unix.val
}

#1.  File Handling - create file loop or manually enter file name  ####
wd<-"~/Dropbox/R/MyProjects/FLIRonejpg"
imgDir<-paste(wd, "/img", sep="")
outDir<-paste(wd, "/out", sep="")
txtDir<-paste(outDir, "/txt", sep="")
pngDir<-paste(outDir, "/png", sep="")
pdfDir<-paste(outDir, "/pdf", sep="")

dir.create(txtDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create(pngDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")
dir.create(pdfDir, showWarnings = TRUE, recursive = FALSE, mode = "0777")

setwd(wd)
camerainfotags<-read.csv("FLIR_Camerainfo_Tags.csv", header=TRUE, stringsAsFactors=FALSE)
caminfo<-as.list(camerainfotags)
setwd(imgDir)
getwd() #show working directory

# Find any files with .jpg or .JPG ending
l.files<-list_files_with_exts(imgDir, full.names=FALSE, c("jpg","JPG","jpeg","JPEG"))
#l.files <- l.files[(toupper(substring(l.files, nchar(l.files) - 3)) == ".JPG" | 
#                 toupper(substring(l.files, nchar(l.files) - 4)) == ".JPEG")]

f<-l.files[1]
#l.files<-l.files[73]
for(f in l.files[c(1)])
{
 
  graphics.off()
  cat('Analysing file:', f)
  cat('\n')
  setwd(imgDir)
  f.ext<-paste(".", file_ext(f), sep="")
  f.root<-substr(f, 1, (nchar(f)-nchar(f.ext)))
  finfo = file.info(f)
  finfo$size  # how many bytes in the file
  f.png<-paste(pngDir, "/", f.root, ".png", sep="")
  f.txt<-paste(txtDir, "/", f.root, "_temperature.csv", sep="")
  f.composite<-paste(pdfDir, "/", f.root, "_composite.pdf", sep="")
  
  # 2. Import image data to raw and int vectors ####
  # Import img data into raw vector and int vector
  # Then use the raw vector to extract PNG data to generate thermal image
  to.read <- file(f, "rb") # set to.read file.  rb means read binary
  byte.size<-1
  rawdata<-readBin(to.read, "raw", n=finfo$size, size=byte.size)
  #intdata<-readBin(to.read, "integer", n=finfo$size, size=2, endian="little", signed=FALSE)
  #names(rawdata)<-paste0("0x", as.hexmode(seq(0, (length(rawdata)-1), 1)))
  close(to.read)
  
  # Use the int vectors to find camera calibration data
  # most FLIR camera tags are stored as unsigned 16bit integers
  # and it is easier to find the tag data by first creating int and real data vectors 
  #  to.read <- file(f, "rb") # set to.read file.  rb means read binary
  #  byte.size<-2
  #  intdatal<-readBin(to.read, "integer", n=finfo$size, size=byte.size, 
  #                  endian = "little", signed=TRUE)
  # names(intdatal)<-paste0("0x", as.hexmode(seq(0, (length(intdatal)*byte.size-1), byte.size)))
  #  close(to.read)
  
  # Use the real vectors to find camera temperature and other calibration data
  # most FLIR calibration and temperature range data are stored as 32bit rationals, 
  # or 'real' numbers in the readBin nomenclature 
  #  to.read <- file(f, "rb") # set to.read file.  rb means read binary
  #  byte.size<-4
  #  realdatal<-readBin(to.read, "double", n=finfo$size, size=byte.size, 
  #                   endian = "little", signed=TRUE)
  # names(realdatal)<-paste0("0x", as.hexmode(seq(0, (length(realdatal)*byte.size-1), byte.size)))
  #  close(to.read)
  
  
  #3. Locate the PNG tag start point.  Derive parameters from PNG Header ####
  # Information on PNG data structure 
  # from: http://www.libpng.org/pub/png/spec/1.2/PNG-Contents.html
  PNG<-locate.fid(c("89","50","4e","47","0d", "0a", "1a", "0a"), rawdata)
  # PNG is the start byte where PNG data structure begins
  # see below PNG tag construction
  IHDR<-locate.fid(c("49", "48", "44", "52"), rawdata)
  IHDR.length<-rawbyte2int(rawdata[seq(IHDR-4,IHDR-1,1)])
  image.width<-rawbyte2int(rawdata[seq(IHDR+4,IHDR+7,1)])
  image.height<-rawbyte2int(rawdata[seq(IHDR+8,IHDR+11,1)])
  bit.depth<-rawbyte2int(rawdata[IHDR+12])
  colour.type<-rawbyte2int(rawdata[IHDR+13])
  compress.method<-rawbyte2int(rawdata[IHDR+14])
  filter.method<-rawbyte2int(rawdata[IHDR+15])
  interlace.method<-rawbyte2int(rawdata[IHDR+16])
  img.size<-image.width*image.height
  scanlines<-seq(1,image.height, 1)
  no.scanlines<-length(scanlines)
  scanline.index<-1+(scanlines-1)*image.width*2+(scanlines-1)
  IDAT<-locate.fid(c("49","44","41","54"), rawdata)
  if(length(IDAT)==1)
  {
    IDAT.length<-rawbyte2int(rawdata[seq(IDAT-4,IDAT-1,1)])
    tempindex<-seq(IDAT+4, IDAT+IDAT.length+4-1, 1)
    rawtemp<-rawdata[tempindex]
    IDAT.data<-rawtemp
    IDAT.data.index<-tempindex
  }
  if(length(IDAT)>1)
  {
    IDAT.length<-NULL
    IDAT.data<-NULL
    IDAT.data.index<-NULL
    for(i in 1:length(IDAT))
    {
      IDAT.length[i]<-rawbyte2int(rawdata[seq(IDAT[i]-4, IDAT[i]-1, 1)])
      tempindex<-seq(IDAT[i]+4, IDAT[i]+IDAT.length[i]+4-1, 1)
      rawtemp<-rawdata[tempindex]
      IDAT.data.index<-c(IDAT.data.index, tempindex)
      IDAT.data<-c(IDAT.data, rawtemp)
    }
  }
  IEND<-locate.fid(c("49", "45", "4e", "44", "ae", "42", "60", "82"), rawdata)
  IEND.length<-8
  # initially I had tried to uncompress the PNG IDAT data, but could not get it to work,
  # so will use the readPNG function from the PNG package
  # and generate the PNG 16bitraw from machine raw data
  # populate entire raw variable with PNG IDATs concatenated together up to the IEND
  
  pngraw<-rawdata[seq(PNG, IEND+IEND.length-1)]
  
  pngError <- tryCatch(pngin<-readPNG(pngraw), error=function(e) e)
  # read in the pngraw vector (eliminates need to save .png to disk)
  if(!inherits(pngError, "error")){
    # if there is no error during readPNG, continue with the script
    # otherwise, it will advance to the next file in the loop
    
    
    pngin<-rotate270.matrix(pngin)
    # rotate matrix to plot in the proper orientation
    image.plot(pngin, col=flirpalette, asp=image.height/image.width, bty="n", xaxt="n", yaxt="n")
    # note how the PNG data saved by FLIR displays incorrectly, due to how they store the 
    # binary pixel data in the wrong endian order before PNG compression
    png16bitraw<-(pngin/256+(floor(pngin*(2^16-1))%%256)/256)*(2^16-1)
    # http://www.imagemagick.org/discourse-server/viewtopic.php?t=24818
    # Not really sure why this math works, but it manages to swap the 16 bit raw to little endian
    image.plot(png16bitraw, asp=image.height/image.width, bty="n",
               xaxt="n", yaxt="n", col=flirpalette)
    
    
    #4. Magicbyte detection of FLIR tags ####
    # Information derived from Phil Harvey's Exiftool program and website:
    # http://www.sno.phy.queensu.ca/~phil/exiftool/
    # then construct the magic byte lookup to allow camera value lookup by recontructing
    # magic sensor fid as a function of "00 02 width height"
    magicbyte<-c("02", "00", as.character(as.hexmode(image.width)), "00",
                 as.character(as.hexmode(image.height)), "00")
    # "02 00" works to find the Flir sensor information.  I have seen it appear 32 bytes in 
    # front of the PNG tag. it also is associated with image size (w x h) and sensor size (w x h)
    # some cases will be pre-pended with "00", thus: "00 02 00" followed by A0 00 78 00
    # The image dimensions, width and height are 2 byte integers in little
    # endian format.  i.e. 640x480 pixels: "80 02 E0 01" = little endian
    # the magicbyte appears to be prepended with "00 02 00" although there may be FLIR files
    # where it is listed in different order due to the different endian storage formats
    
    magicstart<-locate.fid(magicbyte, rawdata)[1] # choose the first tag for reference to the 
    # camerainfo tags from Exiftool
    magicstart0<-magicstart-1 # use this value as offset in readRaw function
    magicstart2<-as.integer((magicstart)/2)+1 
    # add 1 byte to accomodate 'indexing' issue with R not accepting 0 as the first vector element
    magicstart4<-as.integer((magicstart)/4)+1 
    # add 1 byte to accomodate 'indexing' - same as above
    
    FLIR<-locate.fid(c("46", "4c", "49", "52"), rawdata) # FLIR tag
    # "FLIR" is not always a header/tag.  Some times it is tagged as the camera model
    FLIRFFF<-locate.fid(c("46", "4c", "49", "52", "00", "01", "00", "00", 
                          "46", "46", "46"), rawdata) # FLIR....FFF 
    FFF<-locate.fid(c("46", "46", "46"), rawdata)
    # in some FLIR files, the raw data is stored in a TIFF format, so it is useful to 
    # find the FFF tags 
    JFIF<-locate.fid(c("4a", "46", "49", "46"), rawdata)
    # not doing anything with the JFIF tags
    
    #5. Extract Camera calibration values by using meta tags ####
    # offset values obtained from Exiftool's FLIR TAGS information
    # Tags are stored in a csv file largely derived from Exiftool website, with some modifications
    # Tags will be read straight from the file for ease of using the offset values
    # Tag properties and Identities are stored in the List variable caminfo
    # Tag values are stored in the List variable CamValues
    to.read <- file(f, "rb") # set to.read file.  rb means read binary
    ind<-0
    CamValues<-vector("list", length(caminfo$Index))
    for (offset0 in (caminfo$Index))
    {
      ind<-ind+1
      tag.size<-caminfo$TagSize[ind]
      if(caminfo$Type[ind]=="char"){ 
        charval<-readRaw(f, width=tag.size, offset=magicstart0+offset0, nbytes=tag.size, 
                         machine="hex", human="char", size=1, signed=TRUE)
        tmp<-blockString(charval)
      }
      if(caminfo$Type[ind]=="int"){ 
        intval<-readRaw(f, width=tag.size, offset=magicstart0+offset0, nbytes=tag.size,
                        machine="hex", human="int", size=tag.size, endian="little", signed=TRUE)
        tmp<-blockValue(intval)
      }
      if(caminfo$Type[ind]=="real"){ 
        realval<-readRaw(f, width=4, offset=magicstart0+offset0, nbytes=tag.size, machine="hex",
                         human="real", size=tag.size, endian="little", signed=TRUE)
        tmp<-blockValue(realval)
        # Convert temperature variables from Kelvin to Celsius
        if(grepl("Temperature", caminfo$TagName[ind])) 
        {
          tmp<-round(tmp-273.15, 2)
          caminfo$Units[ind]<-"degC"
        }
        if(grepl("Humidity", caminfo$TagName[ind])) 
        {
          tmp<-round(tmp*100)
          caminfo$Units[ind]<-"%"
        }
        
      }
      CamValues[[ind]]<-tmp
    }
    names(CamValues)<-caminfo$TagName
    close(to.read)
    
    
    #6. Clean up CamValues, convert timestamp using POSIXlt ####
    # TimeZone (16 bit integer) is a wrapper for the # of minutes different from UTC
    if(CamValues$TimeZone>32768) CamValues$TimeZone<-(65536-CamValues$TimeZone)
    if(CamValues$TimeZone<32768) CamValues$TimeZone<-(0-CamValues$TimeZone)
    tzh<-(CamValues$TimeZone/60)
    if(CamValues$TimeZone<0) CamValues$TimeZone<-paste(tzh, ":00", sep="")
    if(CamValues$TimeZone>=0) CamValues$TimeZone<-paste(tzh, ":00", sep="")
    # replace the TimeZone integer with a string character indiciating hours different from UTC
    options(digits.secs=3)
    CamValues$DateTimeOriginal<-CamValues$DateTimeOriginal+CamValues$Milliseconds/1000
    CamValues$DateTimeOriginal<-as.POSIXlt(CamValues$DateTimeOriginal, origin="1970-01-01")
    # Convert DateTimeOriginal Unix Time 32-bit integer to POSIX represented time
    # Time zone should be automatically indicated as 3 letter suffix
    CamValues$DateTimeOriginal
    
    
    # 7. Convert raw from PNG import to temperature using Thermimage ####
    attach(CamValues)
    temperature<-raw2temp(png16bitraw,Emissivity,ObjectDistance,ReflectedApparentTemperature,
                          AtmosphericTemperature, IRWindowTemperature,
                          IRWindowTransmission, RelativeHumidity,
                          PlanckR1,PlanckB,PlanckF,PlanckO,PlanckR2)
    detach(CamValues)
    colnames(temperature)<-NULL
    rownames(temperature)<-NULL
    
    t.sum<-summary(as.vector(temperature))
    # Summary temperature data on the entire image
    centre.point<-temperature[floor(image.width/2), floor(image.height/2)]
    # Centre.point is equivalent to the centre spot ROI typical of FLIR images.
    # Its value will be plotted below, but the symbol won't be placed on the graph
    w<-image.width
    h<-image.height
    centre.box<-mean(matrix(temperature[seq(w/2-0.025*w, w/2+0.025*w,1),
                                        seq(h/2-0.025*h,h/2+0.025*h,1)]))
    # Define obj list for calling bquote in plots. 
    # will allow for different font format in plots within the same expression
    obj<-list(fname=f, timestamp=strftime(CamValues$DateTimeOriginal,
                                          origin="1970-01-01", format="%Y-%m-%d %H:%M:%OS", usetz=TRUE), 
              min=round(t.sum[1], 1), max=round(t.sum[6], 1), mean=round(t.sum[4], 1), 
              sd=round(sd(as.vector(temperature)),1), box=round(centre.box, 1),
              spot=round(centre.point, 1))
    
    
    # 8. Save the temperature data to a PNG file ####
    setwd(outDir)
    write.csv(temperature, f.txt, row.names=FALSE)
    
    # 9. Export plain plot (png file) of the converted thermal image. ####
    close.screen(all.screens=TRUE)
    graphics.off()
    #dev.off(4)
    png(filename=f.png)
    split.screen( rbind(c(0,0.8,0,1), c(.8,1,0,1)))
    screen(1)
    image(temperature, add=FALSE, useRaster=TRUE, bty="n", col=flirpalette,
          xlab="", ylab="", xaxt="n", yaxt="n", bty="n",
          zlim=c(min(na.omit(temperature)), max(na.omit(temperature))),
          asp=image.height/image.width)
    screen(2)
    if(w<h) {leg.pos<-c(0.0,0.15,0.2,0.87)
             oC.pos<-c(3.1,-0.02)}
    if(w>h) {leg.pos<-c(0.01,0.15,0.285,0.74)
             oC.pos<-c(3.1, 0.12)}
    image.plot(legend.only=TRUE, col=flirpalette, legend.cex=1, 
               smallplot=leg.pos, legend.mar=1,
               zlim=c(min(na.omit(temperature)), max(na.omit(temperature))))
    text(oC.pos[1], oC.pos[2], bquote("°C"), xpd=TRUE, crt=90)
    close.screen(all.screens=TRUE)
    dev.off(dev.cur())
    
    graphics.off()
    try(par(def.par))
    try(par(bg = "white"))
    
    # 10. Composite Plot - if width > height (i.e. landscape style) ####
    # Optimised plot spacing if final composite image w:h ratio is 1.409
    if(image.width>=image.height)
    {  
      pdf(file=f.composite, width=8, height=8/1.409)
      split.screen(rbind(c(0.01,0.99,0.85,0.99), c(0.01,0.99,0.001,0.84)))
      try(par(mar=c(0,0,0,0)))
      screen(1)
      plot(0,1, type="n", ylab="", bty="n", xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n")
      rect(0,0,1,1, density=0, col="grey")
      text(0.5,0.75, bquote(bold("Filename: ") ~.(obj$fname)))
      text(0.5,0.2,  bquote(bold("Timestamp: ") ~.(obj$timestamp)), cex=0.9)
      split.screen(rbind(c(0.001,0.999,0.335,0.99), c(0.001,0.999,0.01,0.33)), screen=2)
      split.screen(rbind(c(0.15,0.70,0.01,0.99), c(0.701,0.99,0.01,0.99)), screen=3)
      screen(5)
      par(mar=c(0,0,0,0))
      image(temperature, add=FALSE, useRaster=TRUE, bty="n", col=flirpalette,
            xlab="", ylab="", xaxt="n", yaxt="n",
            zlim=c(min(na.omit(temperature)), max(na.omit(temperature))),
            asp=image.height/image.width)
      rect(0,0,1,1)
      screen(6)
      par(mar=c(0,0,2,0))
      image.plot(legend.only=TRUE, col=flirpalette, legend.cex=1, 
                 legend.shrink=1, smallplot=c(0.1,0.2,0,1),
                 zlim=c(min(na.omit(temperature)), max(na.omit(temperature))))
      text(0.87,0.65, bquote(bold("°C")), xpd=TRUE, crt=90)
      split.screen(rbind(c(0.1,0.45,0.01,0.99), c(0.46,0.95,0.01,0.99)), screen=4)
      screen(7)
      par(mar=c(2,2,1,0))
      par(mgp=c(1,0.5,0))
      plot(density(temperature, adjust=0.1), main="", ylab=bquote(bold("No. Pixels")), xlab="", 
           bty="n", yaxt="n", xaxt="n", xlim=c(t.sum[1], t.sum[6]),  col="black")
      axis(side=1, xaxp=c(floor(t.sum[1]), ceiling(t.sum[6]), 2), cex.axis=0.8, pos=0)
      polygon(density(temperature, adjust=0.1), col='black')
      screen(8)
      par(mar=c(1,4,1,1))
      par(mgp=c(1,0,0))
      plot(0,1, type="n", ylab=bquote(bold("Summary\nStatistics")), xlim=c(0,1),
           ylim=c(0,1), xaxt="n", yaxt="n")
      text(0,0.85,   cex=1.2, adj=c(0,NA), bquote(bold("Min:  ") ~.(obj$min)))
      text(0.6,0.85, cex=1.2, adj=c(0,NA), bquote(bold("Max:  ") ~.(obj$max)))
      text(0,0.55,   cex=1.2, adj=c(0,NA), bquote(bold("Mean: ") ~.(obj$mean)))
      text(0.6,0.55, cex=1.2, adj=c(0,NA), bquote(bold("SD:   ") ~.(obj$sd)))
      text(0,0.25,   cex=1.2, adj=c(0,NA), bquote(bold("Box:  ") ~.(obj$box)))
      text(0.6,0.25, cex=1.2, adj=c(0,NA), bquote(bold("Spot: ") ~.(obj$spot)))
      close.screen()
      close.screen(all.screens=TRUE)
      dev.off(dev.cur())
    }
    
    graphics.off()
    try(par(def.par))
    try(par(bg = "white"))
    # 11, Composite Plot - if width < length (i.e. portrait style) ####
    # Optimised plot spacing if final composite image w:h ratio is 1.409
    if(image.width<image.height)
    {
      pdf(file=f.composite, width=8, height=8/1.409)
      split.screen(rbind(c(0.01,0.99,0.85,0.99), c(0.01,0.99,0.1,0.83)))
      par(mar=c(0,0,0,0))
      screen(1)
      plot(0,1, type="n", ylab="", bty="n", xlim=c(0,1), ylim=c(0,1), xaxt="n", yaxt="n")
      rect(0,0,1,1, density=0, col="grey")
      text(0.5,0.75, bquote(bold("Filename: ") ~.(obj$fname)))
      text(0.5,0.2,  bquote(bold("Timestamp: ") ~.(obj$timestamp)), cex=0.9)
      split.screen(rbind(c(0.05,0.3,0.01,0.99), c(0.31,0.7,0.01,0.99), c(0.71,0.95,0.01,0.99)), 
                   screen=2)
      screen(4)
      par(mar=c(0,0,0,0))
      image(temperature, add=FALSE, useRaster=TRUE, bty="n", col=flirpalette,
            xlab="", ylab="", xaxt="n", yaxt="n",
            zlim=c(min(na.omit(temperature)), max(na.omit(temperature))),
            asp=image.height/image.width)
      rect(0,0,1,1)
      screen(5)
      par(mar=c(0,0,2,0))
      image.plot(legend.only=TRUE, col=flirpalette, legend.cex=1, 
                 legend.shrink=1, smallplot=c(0.1,0.3,0,1),
                 zlim=c(min(na.omit(temperature)), max(na.omit(temperature))))
      text(0.9,0.5, bquote(bold("°C")), xpd=TRUE, crt=90)
      screen(3)
      split.screen(rbind(c(0.01,0.99,0.5,0.99), c(0.01,0.99,0.01,0.49)), screen=3)
      screen(6)
      par(mar=c(2,2,0,1))
      par(mgp=c(1,0.5,0))
      plot(density(temperature, adjust=0.1), main="", ylab=bquote(bold("No. Pixels")), xlab="", 
           bty="n", yaxt="n", xaxt="n", xlim=c(t.sum[1], t.sum[6]),  col="black")
      axis(side=1, xaxp=c(floor(t.sum[1]), ceiling(t.sum[6]), 2), cex.axis=0.8, pos=0)
      polygon(density(temperature, adjust=0.1), col='black')
      screen(7)
      plot.new()
      par(mar=c(0,2,0,1))
      par(mgp=c(1,0.5,0))
      plot(0,1, type="n", ylab=bquote(bold("Summary Statistics")), xlim=c(0,1),
           ylim=c(0,1), xaxt="n", yaxt="n")
      text(0.05,0.86, cex=0.9, adj=c(0,NA), bquote(bold("Min:  ") ~.(obj$min)))
      text(0.05,0.72, cex=0.9, adj=c(0,NA), bquote(bold("Max:  ") ~.(obj$max)))
      text(0.05,0.58, cex=0.9, adj=c(0,NA), bquote(bold("Mean: ") ~.(obj$mean)))
      text(0.05,0.44, cex=0.9, adj=c(0,NA), bquote(bold("SD:   ") ~.(obj$sd)))
      text(0.05,0.30, cex=0.9, adj=c(0,NA), bquote(bold("Box:  ") ~.(obj$box)))
      text(0.05,0.16, cex=0.9, adj=c(0,NA), bquote(bold("Spot: ") ~.(obj$spot)))
      close.screen()
      close.screen(all.screens=TRUE)
      dev.off(dev.cur())
    }
  } # loop associated with if statement checking for png import error
} # loop associated with f (files)
