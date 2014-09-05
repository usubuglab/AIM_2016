options(stringsAsFactors=F)
Photos=read.csv("PhotoComment.csv")

phototbl=data.frame(cbind(UID=55555,PHOTO_ID=4444,PHOTO_DESCRIP='Test'))
phototbl=phototbl[-1,]

for (j in 1:nrow(Photos)){
string=Photos$COMMENT[j]
UID=Photos$UID[j]


photosplit=strsplit(string,"PHOTO ID: ")
for (i in 1:length(unlist(photosplit))){
  numloc=regexpr(pattern="[0123456789]+", text=photosplit[[1]][4])#,value=T)
  descrip1=strsplit(x=photosplit[[1]][4],split='LOCATION: ')[[1]][2]
  descrip2=sub(x=descrip1,pattern='DESCRIPTION: ',replacement=' - ')
  if(numloc[1]>0){
    photonum=substring("0063 LOCATION: X-SITE DESCRIPTION: OVERVIEW  ",numloc[1],attr(numloc,"match.length"))
    #append
  }
 phototbl=rbind(phototbl,c(UID,photonum,descrip2)) ;names(phototbl)=c('UID','PHOTO_ID','PHOTO_DESCRIP')
}}

#use files base package to organize
#! rename photos?
setwd('I:\\DCIM\\hawaii')
totalfiles=list.files(getwd())
photoextensionGREP='*.(?:jpe?g|gif|png)'
photofiles=list.files(getwd(),pattern=photoextensionGREP,ignore.case=T)
if(length(totalfiles)>length(photofiles)){
  print('Additional non-photo files found. Confirm the number of photos matches that available in the folder. Add additional file types if needed. The following files were not considered photos:')
  print(setdiff(totalfiles,photofiles))
  }
for (p in 1:length(photofiles)){
  oldname=photofiles[p]
  extension1=regexpr(pattern=photoextensionGREP,oldname,ignore.case=T)#sub(pattern=photoextensionGREP, "\\1", oldname[grepl(pattern=photoextensionGREP, oldname)])
  extension=substr(oldname,extension1[1],extension1[1]+attr(extension1,"match.length"))
  prefix='Test'#<Trip-Location><Owner>#will be tricky to separate some bob and em oregeon dscn (em's should all have parantheses if same number)...best bet is temporarily relocating them in batches (which is probably how to do them all)
  newname=sprintf('%s%s%s',prefix,sub(extension,'',oldname),extension)
  file.rename(oldname, newname)
}
#! shuffle photos between folders by FO
#file.copy();file.remove()