
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

lines <- lines[!is.na(IDR_LIT) & (grepl("400",ADR_LIT)|grepl("225",ADR_LIT))]
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

lines <- ends[,.(node2=node,nb2=nb,x_end=x,y_end=y)][lines, on=c("x_end","y_end"), mult="first"]
lines <- ends[,.(node1=node,nb1=nb,x_start=x,y_start=y)][lines, on=c("x_start","y_start"), mult="first"]

# 
# setcolorder(lines, c("node1","node2","nb1","nb2","ID_OBJECT",colnames(lines)[6:dim(lines)[2]]))

# étude des lignes d'un même IDR_LIT
lines <- lines[!(ID_OBJECT %in% 
                    c("77798188_14134_1","71594843_19859_1","71594743_7376_1",
                      "77008104_13844_1","71945077_19397_1","71289107_33506_1",
                      "77200185_3022_1","77200085_6597_1","77983288_7575_1",
                      "77578704_32613_1","77578404_32423_1","77578004_26658_1",
                      "77328083_29478_1","77561325_14117_1","74355167_33545_1",
                      "74355067_30585_1","76136688_29524_1","77983188_32817_1",
                      "77903128_3496_1","78013904_21762_1","77881928_21178_1",
                      "77561425_32566_1","75130489_9956_1","75137589_1415_1",
                      "77008204_21540_1","78013804_28649_1"))]

selpaths <- lines[,{
  nbs <- table(c(node1,node2))
  .("ID_OBJECT"=ID_OBJECT,
    "nbp1"=c(nbs[match(node1, names(nbs))]),
    node1=node1,
    x_start=x_start,
    y_start=y_start,
    "nbp2"=c(nbs[match(node2, names(nbs))]),
    node2=node2,
    x_end=x_end,
    y_end=y_end,
    "nbp_ends"=sum(nbs==1))
}, by=IDR_LIT]

# paths qui ne sont pas des lignes
non_lin <- selpaths[nbp1>2 | nbp2>2 | nbp_ends != 2,IDR_LIT]

for (id in unique(non_lin)) {
  crd <-  selpaths[IDR_LIT==id,]
  r1 <- crd$nbp1 == 1
  r2 <- crd$nbp2 == 1
  leafs <- cbind(c(crd$x_start[r1],
                   crd$x_end[r2]),
                 c(crd$y_start[r1],
                   crd$y_end[r2]))
  row.names(leafs) <- paste(c(crd$ID_OBJECT[r1],crd$ID_OBJECT[r2]),
                            c(rep("start",sum(r1)),rep("end",sum(r2))))
  dists <- as.matrix(dist(leafs))
  dists[upper.tri(dists, diag=TRUE)] <- NA
  dists <- data.table(melt(dists))
  dists[,c("Var1","end1"):=tstrsplit(Var1," ")]
  dists[,c("Var2","end2"):=tstrsplit(Var2," ")]
  
  corrig <- dists[!is.na(value) & Var1!=Var2,
        .(Var2=Var2[which.min(value)],
          value=min(value),
          end1=end1[which.min(value)],
          end2=end2[which.min(value)]),
        by=Var1]
  corrig <- corrig[,.(Var1=Var1[which.min(value)],
                    value=min(value),
                    end1=end1[which.min(value)],
                    end2=end2[which.min(value)]),
                  by=Var2]
  if (dim(corrig)[1]>1) corrig <- corrig[value!=max(value)]
  
  if (dim(corrig)[1]>0) {
    for (i in 1:length(corrig$Var1)) {
      id1 <- match(corrig$Var1[i],lines$ID_OBJECT)
      id2 <- match(corrig$Var2[i],lines$ID_OBJECT)
      if (corrig$end1[i]=="start") {
        l1 <- 1
      } else {
        l1 <- dim(lines$paths[[id1]])[2]
      }
      if (corrig$end2[i]=="start") {
        l2 <- 1
      } else {
        l2 <- dim(lines$paths[[id2]])[2]
      }
      x_mean <- (lines$paths[[id1]][,l1,1]+lines$paths[[id2]][,l2,1])/2
      y_mean <- (lines$paths[[id1]][,l1,2]+lines$paths[[id2]][,l2,2])/2
      lines$paths[[id1]][,l1,1] <- x_mean
      lines$paths[[id2]][,l2,1] <- x_mean
      lines$paths[[id1]][,l1,2] <- y_mean
      lines$paths[[id2]][,l2,2] <- y_mean
    }
  }
}

lines <- lines[,-c("node1","nb1","node2","nb2"),with=FALSE]


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





spl_sub <- subset(spl_lines, spl_lines$IDR_LIT %in% non_lin )

colrs <- ifelse(spl_lines$ID_PATH %in% lines_car$ID_PATH, "#ff0000", "#0000ff" )
colrs <- ifelse(spl_lines$IDR_LIT %in% non_lin, "#ff0000", "#0000ff" )
colrs <- ifelse(spl_lines$group_line != 1, "#ff0000", "#0000ff" )
#colrs <- ifelse(spl_lines$group_line!=1 , "#ff0000", "#0000ff" )

spl_postes <- SpatialPointsDataFrame(cbind(postes$x,postes$y),postes,proj4string=CRS("+init=epsg:2154"))
spl_postes <- spTransform(spl_postes, "+init=epsg:4326")


leaflet() %>%
  addTiles() %>%
#  addCircleMarkers(data=spl_postes) %>%
  addPolylines(data=spl_merged,
    popup=~paste0("<h3>",IDR_LIT ,"</h3>",
#                  "ID : ", ID_OBJECT, "<br>",
                             "ADR : ", ADR_LIT, "<br>",
                            "ETAT : ", ETAT, "<br>",
                            "U : ", U_MAX, "<br>"),
#               color=colrs,
              weight=~ifelse(U_MAX==7,6,3)
    )

save(lines,file="lines_paths_clean.RData")

load(file="lines_paths_clean.RData")

merged <- lines[,.(paths = {
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
}), by=IDR_LIT ]

# join infos 
merged <- lines[,.(CONFIG,ETAT,U_MAX,IDR_LIT,ADR_LIT)][
  merged, on="IDR_LIT", mult="first"]


merged[, c("x_start","y_start","x_end","y_end"):={
  xs <- .SD$paths[[1]][,,1]
  ys <- .SD$paths[[1]][,,2]
  .(xs[1],ys[1],tail(xs,n=1),tail(ys,n=1))
}, by=IDR_LIT]

spl_merged <- apply(merged, 1, function(ligne){
  ligne <- as.list(ligne)
  Lines(Line(cbind(ligne$paths[,,1],ligne$paths[,,2])),
        ID=ligne$IDR_LIT)
}) %>% SpatialLines(proj4string=CRS("+init=epsg:2154"))
setDF(merged)
row.names(merged) <- merged$IDR_LIT
spl_merged <- SpatialLinesDataFrame(spl_merged, merged)
setDT(merged)

merged$long <- gLength(spl_merged,byid = TRUE)

spl_merged <- spTransform(spl_merged, "+init=epsg:4326")

merged[,dir_long:=sqrt((x_start-x_end)^2+(y_start-y_end)^2)]
# vérification distance vol d'oiseau / distance géométrique
plot(merged$dir_long,merged$long)

#####
# association lignes / postes

merged[,start:=substr(IDR_LIT, 1, 5)]
merged[,u_code:=as.numeric(substr(IDR_LIT, 7, 7))]
merged[,circn:=as.numeric(substr(IDR_LIT, 8, 8))]
merged[,end:=substr(IDR_LIT, 9, 13)]

# ordonner les postes
merged[!is.na(start) & start>end, c("start","end"):=.(end,start)]

merged <- unique(merged[!is.na(start) & u_code>5], by=c("start","circn","end","u_code"))

# recherche des postes correspondant et inversion si nécessaire
merged[, code_start := postes$CODNAT[match(start, postes$CODNAT)]]
merged[, code_end := postes$CODNAT[match(end, postes$CODNAT)]]
merged[, "pres":={
  id <- match(code_start,postes$CODNAT)
  sqrt((postes$x[id]-x_start)^2+(postes$y[id]-y_start)^2)-
    sqrt((postes$x[id]-x_end)^2+(postes$y[id]-y_end)^2)
}]

merged[, code_temp := code_start]
merged[pres>0, code_start := code_end]
merged[pres>0, code_end := code_temp]
merged <- merged[,-c("pres","code_temp"),with=FALSE]

# recherche des postes proches
merged <- merged[,c("code_end","dist_end"):={
                 dists <- crossdist(.SD$x_end,.SD$y_end, 
                                    postes$x,postes$y)
                 dists[outer(.SD$u_code,postes$U_MAX,`>`)]<-100000
                 disti <- apply(dists, 1, which.min)
                 dists <- apply(dists, 1, min)
                 .(code_end=postes$CODNAT[disti],dist_end=dists)}]
merged <- merged[,c("code_start","dist_start"):={
                 dists <- crossdist(.SD$x_start,.SD$y_start, 
                                    postes$x,postes$y)
                 dists[outer(.SD$u_code,postes$U_MAX,`>`)]<-100000
                 disti <- apply(dists, 1, which.min)
                 dists <- apply(dists, 1, min)
                 .(code_start=postes$CODNAT[disti],dist_start=dists)}]
merged[is.na(code_start)|(dist_start>1500), code_start:=NA]
merged[is.na(code_end)|(dist_end>1500), code_end:=NA]

# noeuds restant
ends <- melt(merged[is.na(code_end)|is.na(code_start),
                    .(IDR_LIT, U_MAX, x_start,x_end,y_start,y_end,code_end,code_start)],
             id.vars = c("IDR_LIT","U_MAX","code_end","code_start"))
ends[,c("coord","side"):=.(substr(variable,1,1),substr(variable,3,10))]
ends <- distinct(ends[(is.na(code_start)&side=="start")|(is.na(code_end)&side=="end")])
ends <- spread(ends[,.(IDR_LIT,U_MAX,side, coord,value)], coord, value)
setDT(ends)
ends <- ends[,.(node=paste0("NOEUD_",.GRP),
                U_MAX=max(U_MAX), 
                IDR_LIT=IDR_LIT[which.max(U_MAX)]), 
             by=.(x,y)]

merged <- ends[,.(node_start=node,x_start=x,y_start=y)][
  merged, 
  on=c("x_start","y_start"),
  mult="first"]
merged <- ends[,.(node_end=node,x_end=x,y_end=y)][
  merged, 
  on=c("x_end","y_end"),
  mult="first"]

merged[!is.na(node_start), code_start:=node_start]
merged[!is.na(node_end), code_end:=node_end]
merged <- merged[,-c("node_start","node_end"), with=FALSE]


# sous-graphes connectés ?
merged <- merged[code_start!=code_end,]
graph <- graph_from_data_frame(merged[,.(code_start=code_start,
                                         code_end=code_end)],directed=FALSE)
cl <- clusters(graph)
isole <- names(cl$membership)[cl$membership>1]

merged <- merged[!((code_start %in% isole) | (code_end %in% isole)),]
deg <- degree(graph)
leaves <- names(deg)[deg==1]

# test des lignes N qui ne sont pas redondantes
merged[,redundant:=NA]
for (i in seq_along(merged$code_start)) {
  if (i %% 10 == 0) print(i)
  graph <- graph_from_data_frame(merged[-i,.(code_start=code_start,
                                           code_end=code_end)],directed=FALSE)
  cl <- clusters(graph)
  merged$redundant[i] <-  (cl$no == 1)
}
merged[code_start %in% leaves, redundant:=FALSE]
merged[code_end %in% leaves, redundant:=FALSE]

graph <- graph_from_data_frame(merged[,.(code_start=code_start,
                                         code_end=code_end)],directed=FALSE)
# on ne garde que les postes utilisés
postes2 <- rbind(postes,ends[,.(U_MAX=U_MAX,x=x,y=y,CODNAT=node)], fill=TRUE)
postes2 <- postes2[CODNAT %in% names(V(graph)),]

spl_merged <- apply(merged, 1, function(ligne){
  ligne <- as.list(ligne)
  Lines(Line(cbind(ligne$paths[,,1],ligne$paths[,,2])),
        ID=ligne$IDR_LIT)
}) %>% SpatialLines(proj4string=CRS("+init=epsg:2154"))
setDF(merged)
row.names(merged) <- merged$IDR_LIT
spl_merged <- SpatialLinesDataFrame(spl_merged, merged)
setDT(merged)
spl_merged <- spTransform(spl_merged, "+init=epsg:4326")


# colrs <- ifelse(spl_merged$code_start %in% isole | 
#                   spl_merged$code_end %in% isole   , "#ff0000", "#0000ff" )
colrs <- ifelse(!spl_merged$redundant , "#ff0000", "#0000ff" )
# colrs <- ifelse(is.na(spl_merged$code_start) | 
#                   is.na(spl_merged$code_end)   , "#ff0000", "#0000ff" )

spl_postes <- SpatialPointsDataFrame(cbind(postes2$x,postes2$y),postes2,proj4string=CRS("+init=epsg:2154"))
spl_postes <- spTransform(spl_postes, "+init=epsg:4326")
colrs_postes <- ifelse(substr(spl_postes$CODNAT,1,5)=="NOEUD", "#ff0000", "#0000ff" )


leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data=spl_postes,radius=5,popup=~CODNAT, color=colrs_postes) %>%
  addPolylines(data=spl_merged,
               popup=~paste0("<h3>",IDR_LIT ,"</h3>",
                             #                  "ID : ", ID_OBJECT, "<br>",
                             "ADR : ", ADR_LIT, "<br>",
                             "ETAT : ", ETAT, "<br>",
                             "U : ", U_MAX, "<br>",
                             "start : ", code_start, "<br>",
                             "end : ", code_end, "<br>"),
                              color=colrs,
               weight=~ifelse(U_MAX==7,6,3)
  )


# correspondance des transfos
postes_400 <- unique(as.vector(unlist(merged[U_MAX==7, .(code_start,code_end)])))
postes_225 <- unique(as.vector(unlist(merged[U_MAX==6, .(code_start,code_end)])))

postes2[,l400:=CODNAT %in% postes_400]
postes2[,l225:=CODNAT %in% postes_225]

# postes 225 avec des lignes 400
postes2[l400 & U_MAX==6]
# postes 400 sans lignes 400
postes2[!l400 & U_MAX==7]


reseau <- list(lignes=merged,
               postes=postes2)

save(reseau, file="topologie.RData")

