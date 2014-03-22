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