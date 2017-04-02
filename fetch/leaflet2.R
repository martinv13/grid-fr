
load("carte_rte.RData")

unlist(lapply(layers, function(x){x[[1]]$geometryType }))

# extraction des postes
points <- lapply(c(1:3,7:10), function(i){
  lapply(layers[[i]], function(ch){
    temp <- as.list(ch$features)
    temp <- cbind(temp$attributes, temp$geometry)
    setDT(temp)
    temp
  }) %>% rbindlist
})

# for (i in 1:7) {
# write.table(points[[i]], file=paste0(i,".csv"), sep=";", dec=",", row.names = FALSE)
# }

postes <- rbindlist(points[1:3])[U_MAX>5,]
postes <- unique(postes, by="CODNAT")

# extraction des lignes
lines <- lapply(4:6, function(i){
  lapply(layers[[i]], function(ch){
    temp <- as.list(ch$features)
    temp <- cbind(temp$attributes, temp$geometry)
    setDT(temp)
    temp
  }) %>% rbindlist
})

lines <- rbindlist(lines, idcol="layer")

lines[,ID_OBJECT:=paste(ID, OBJECTID, sep="_")]
lines <- unique(lines, by="ID_OBJECT")[U_MAX>5 & ETAT=="E" & layer==1 ,]
lines <- melt(lines, measure.vars = 9:18)

lines[,varn:=as.numeric(substr(variable,9,9))]
lines[,variable:=substr(variable,1,7)]
lines <- spread(lines, variable, value)

lines <- lines[!is.na(IDR_LIT)]
lines[,ID_OBJECT:=paste(ID_OBJECT,varn,sep="_")]

lines[, c("x_start","y_start","x_end","y_end"):={
  xs <- .SD$paths[[1]][,,1]
  ys <- .SD$paths[[1]][,,2]
  .(xs[1],ys[1],tail(xs,n=1),tail(ys,n=1))
}, by=ID_OBJECT]

lines <- unique(lines, by=c("IDR_LIT","x_start","y_start","x_end","y_end","U_MAX"))

# extraire les noeuds
ends <- melt(lines[,.(ID_OBJECT, U_MAX, x_start,x_end,y_start,y_end)],
             id.vars = c("ID_OBJECT","U_MAX"))
ends[,c("coord","side"):=.(substr(variable,1,1),substr(variable,3,10))]
ends <- spread(ends[,.(ID_OBJECT,U_MAX,side, coord,value)], coord, value)
setDT(ends)

ends[,node:=.GRP, by=.(x,y)]
ends[,nb:=.N,by=node]

lines <- ends[side=="end",
              .(ID_OBJECT,node1=node,nb1=nb)][lines, on="ID_OBJECT", mult="first"]
lines <- ends[side=="start",
              .(ID_OBJECT,node2=node,nb2=nb)][lines, on="ID_OBJECT", mult="first"]
setcolorder(lines, c("node1","node2","nb1","nb2","ID_OBJECT",colnames(lines)[6:dim(lines)[2]]))

# étude des lignes d'un même IDR_LIT
linesf <- lines[!(ID_OBJECT %in% 
                    c("77798188_14134_1","71594843_19859_1","71594743_7376_1",
                      "77008104_13844_1","71945077_19397_1","71289107_33506_1",
                      "77200185_3022_1","77200085_6597_1","77983288_7575_1",
                      "77578704_32613_1","77578404_32423_1","77578004_26658_1",
                      "77328083_29478_1","77561325_14117_1","74355167_33545_1",
                      "74355067_30585_1","76136688_29524_1","77983188_32817_1",
                      "77903128_3496_1","78013904_21762_1","77881928_21178_1",
                      "77561425_32566_1","75130489_9956_1","75137589_1415_1"))]

selpaths <- linesf[,{
  nbs <- table(c(node1,node2))
  .("ID_OBJECT"=ID_OBJECT,
    "nbp1"=c(nbs[match(node1, names(nbs))]),
    x_start=x_start,
    y_start=y_start,
    "nbp2"=c(nbs[match(node2, names(nbs))]),
    x_end=x_end,
    y_end=y_end,
    "nbp_ends"=sum(nbs==1))
}, by=IDR_LIT]

# paths qui ne sont pas des lignes
non_lin <- selpaths[nbp1>2 | nbp2>2 | nbp_ends != 2, IDR_LIT]

crd <-  selpaths[IDR_LIT=="VLEJUL62ZROB5",]
View(data.matrix(dist(cbind(c(crd$x_start[crd$nbp1==1],
                              crd$x_end[crd$nbp2==1]),
                            c(crd$y_start[crd$nbp1==1],
                              crd$y_end[crd$nbp2==1])))))

# fusion des paths disjoints


# fusion des paths

merged <- linesf[,.(path2 = {
        sp <- mapply(function(p,i){
                    Lines(Line(cbind(p[,,1],p[,,2])),ID=i)
        },paths,seq_along(paths))
        sp <- SpatialLines(sp,proj4string=CRS("+init=epsg:2154"))
        merged <- gLineMerge(sp)
        x <- merged@lines[[1]]@Lines[[1]]@coords[,1]
        y <- merged@lines[[1]]@Lines[[1]]@coords[,2]
        a <- array(NA, dim = c(1,length(x),2))
        a[,,1] <- x
        a[,,2] <- y
        list(a)
      }), by=ID_PATH]

# join infos 
merged <- lines[,.(IDR_LIT_1,ADR_LIT_1,IDR_LIT_2,ADR_LIT_2,
                   IDR_LIT_3,ADR_LIT_3,IDR_LIT_4,ADR_LIT_4,
                   IDR_LIT_5,ADR_LIT_5,ID_PATH,CONFIG,ETAT,U_MAX)][
                     merged, on="ID_PATH", mult="first"]



spl_lines <- apply(lines, 1, function(ligne){
  ligne <- as.list(ligne)
  Lines(Line(cbind(ligne$paths[,,1],ligne$paths[,,2])),
        ID=ligne$ID_OBJECT)
}) %>% SpatialLines(proj4string=CRS("+init=epsg:2154"))
setDF(lines)
row.names(lines) <- lines$ID_OBJECT
spl_lines <- SpatialLinesDataFrame(spl_lines, lines)
setDT(lines)

spl_lines <- spTransform(spl_lines, "+init=epsg:4326")

spl_sub <- subset(spl_lines, spl_lines$IDR_LIT%in% non_lin )

colrs <- ifelse(spl_lines$ID_PATH %in% lines_car$ID_PATH, "#ff0000", "#0000ff" )
colrs <- ifelse(spl_lines$IDR_LIT %in% non_lin, "#ff0000", "#0000ff" )
colrs <- ifelse(spl_lines$group_line != 1, "#ff0000", "#0000ff" )
#colrs <- ifelse(spl_lines$group_line!=1 , "#ff0000", "#0000ff" )

spl_postes <- SpatialPointsDataFrame(cbind(postes$x,postes$y),postes,proj4string=CRS("+init=epsg:2154"))
spl_postes <- spTransform(spl_postes, "+init=epsg:4326")


leaflet() %>%
  addTiles() %>%
#  addCircleMarkers(data=spl_postes) %>%
  addPolylines(data=spl_sub,
    popup=~paste0("<h3>",IDR_LIT ,"</h3>",
                  "ID : ", ID_OBJECT, "<br>",
                             "ADR : ", ADR_LIT, "<br>",
                            "ETAT : ", ETAT, "<br>",
                            "U : ", U_MAX, "<br>"),
#               color=colrs,
              weight=~ifelse(U_MAX==7,6,3)
    )







lines[,start:=substr(IDR_LIT, 1, 5)]
lines[,u_code:=as.numeric(substr(IDR_LIT, 7, 7))]
lines[,circn:=as.numeric(substr(IDR_LIT, 8, 8))]
lines[,end:=substr(IDR_LIT, 9, 13)]

# ordonner les postes
lines[!is.na(start) & start>end, c("start","end"):=.(end,start)]

lines <- unique(lines[!is.na(start) & u_code>5], by=c("start","circn","end"))

# étiquettes des groupes pour les lignes
lines[,grp:=as.numeric(rep(NA, length(OBJECTID)))]
igrp <- 1
while (lines[is.na(grp),.N]>0) {
  id <- lines[match(TRUE,is.na(lines$grp)),ID_OBJECT]
  lines[ID_OBJECT==id, grp:=igrp]
  while (lines[(IDR_LIT %in% lines[grp==igrp,IDR_LIT]) & is.na(grp),.N]>0) {
    lines[IDR_LIT %in% lines[grp==igrp,IDR_LIT], grp:=igrp]
  }
  igrp <- igrp+1
}

# recherche des postes correspondant
lines <- lines[, code_start := postes$CODNAT[match(start, postes$CODNAT)]]
# codes avec des espaces
lines[is.na(code_start), 
      code_start := postes$CODNAT[match(gsub(" ","",start), postes$CODNAT)]]


lines <- lines[, code_end := postes$CODNAT[match(end, postes$CODNAT)]]
lines[is.na(code_end), 
      code_end := postes$CODNAT[match(gsub(" ","",end), postes$CODNAT)]]
corres <- postes[,.(CODNAT, end=paste0("Z",substr(CODNAT,1,4)))][
  lines[is.na(code_end),], on="end"]
corres <- unique(corres, by=c("end","CODNAT"))[
  ,occurences:=.N, by=end][
    occurences==1]
lines <- lines[is.na(code_end), code_end := corres$CODNAT[match(end, corres$end)]]

# on enlève les piquages et les interconnexions
lines <- lines[(!is.na(code_start)&!is.na(code_end)) | 
                 !(grepl("PIQUAGE", ADR_LIT) | substr(start,1,1)==".")]
# combien en reste-t-il ?
lines[is.na(code_start)|is.na(code_end),.N]

# on enlève les lignes non rattachées
lines <- lines[!is.na(code_start)&!is.na(code_end)]

lines <- postes[,.(code_end=CODNAT,x_end=x,y_end=y)][lines, on="code_end"]
lines <- postes[,.(code_start=CODNAT,x_start=x,y_start=y)][lines, on="code_start"]

lines <- unique(lines, by=c("code_start", "code_end", "circn"))





fwrite(lines[is.na(code_start)|is.na(code_end),.N],file="unmatch.csv",sep=";",dec=",")
fwrite(postes,file="postes.csv",sep=";",dec=",")

View(lines[is.na(x_start),])


lignes_elec <- fread("data/lines_rte.csv")
lignes_elec[is.na(match(lignes_elec$`Identifiant géographique / Asset location`,
      c(lines$ADR_LIT_1,lines$ADR_LIT_2,lines$ADR_LIT_3,lines$ADR_LIT_4,lines$ADR_LIT_5)))]


test <- lines[!is.na(IDR_LIT_4),]
test <- lines[i==1491,]

ptest <- lapply(test$paths, function(pt){
  data.table(x=pt[,,1],x=pt[,,2])
}) %>% rbindlist()
dtest <- as.matrix(dist(ptest, diag=TRUE, upper = TRUE))
diag(dtest) <- 100000000000
dtest <- apply(dtest, 1, which.min)
str(dtest)
dim(dtest)
strsplit(test$IDR_LIT_1[1],"L61")

points[[1]][CODNAT=="P.JER",]

# extraction des points 
lines[,path_grp:=.GRP, by=.(IDR_LIT_1,ADR_LIT_1,IDR_LIT_2,ADR_LIT_2,
                            IDR_LIT_3,ADR_LIT_3,IDR_LIT_4,ADR_LIT_4,
                            IDR_LIT_5,ADR_LIT_5)]

pt <- lines[path_grp==22,paths ]

spl_lines <- apply(lines, 1, function(ligne){
  ligne <- as.list(ligne)
  Lines(Line(cbind(as.numeric(c(ligne$x_start, ligne$x_end)),
                   as.numeric(c(ligne$y_start, ligne$y_end)))), 
        ID=paste(ligne$code_start,ligne$code_end,ligne$circn,sep="_"))
}) %>% SpatialLines(proj4string=CRS("+init=epsg:2154"))



lines_df <- lines[,-c("paths"),with=FALSE]
setDF(lines_df)
row.names(lines_df) <- lines_df$ID_OBJECT

spl_lines <- SpatialLinesDataFrame(spl_lines, lines_df)



test2 <- subset(test, test$MAJ_GEO==max(test$MAJ_GEO))

gLineMerge()

long <- gLength(spl_lines, byid=TRUE)
