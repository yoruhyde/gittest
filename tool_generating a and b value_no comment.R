##########################################################################################
# SETUP
##########################################################################################
library(data.table);library(bit64)
setwd("C:\\Users\\yuemeng1\\Desktop\\optm comcast_gai\\")
ex.curve=fread("opt_modelinput_curve.csv")
ex.dim=fread("adm_dim.csv")

##########################################################################################
# Code part
##########################################################################################

# check sumup dim
bdgt_dim=ex.dim$bdgt_dim[ex.dim$bdgt_dim!=0]
dim=ex.dim$dim[ex.dim$dim!=0]
if(sum(dim %in% bdgt_dim)==0) bdgt_dim1="all" else bdgt_dim1=dim[dim %in% bdgt_dim]

setkey(ex.curve,bdgt_id)
for.sp=unique(ex.curve)

# for.sp=curve[!duplicated(curve[,c(bdgt_dim),with=F]),]
sp_current=for.sp[,list(sp_current=sum(sp_current)),by=c(bdgt_dim1)]


# generate spend points
curve_fit=ex.curve
percent=c(seq(0,8,by=.1),9:100)
percent_mat=matrix(rep(percent,nrow(curve_fit)),nr=nrow(curve_fit),byrow=T)
sp_mat=curve_fit$sp_current*percent_mat
colnames(sp_mat)=paste("sp_",1:length(percent),sep="")


calc_npv=function(x){
    
    npv=curve_fit$a*(1-exp(-curve_fit$b*x))+rnorm(nrow(curve_fit),rep(0,nrow(curve_fit)),rep(1e-5,nrow(curve_fit)))
    npv
  }


npv_mat=apply(sp_mat,2,calc_npv)
colnames(npv_mat)=paste("npv_",1:length(percent),sep="")

# fit curves
npv_mat=data.table(cbind(curve_fit[,c(dim),with=F],npv_mat))
sp_mat=data.table(cbind(curve_fit[,c(bdgt_dim),with=F],sp_mat))

sp_fit=sp_mat[!duplicated(sp_mat[,c(bdgt_dim),with=F]),]
sp_fit=sp_fit[,lapply(.SD,sum,na.rm=T),by=c(bdgt_dim1)]

npv_fit=npv_mat[,lapply(.SD,sum,na.rm=T),by=c(dim)]
curve_fit_final=merge(npv_fit,sp_fit,by=c(bdgt_dim1),all.x=T)

col_npv=which(names(curve_fit_final) %in% paste("npv_",1:length(percent),sep="")) 
col_sp=which(names(curve_fit_final) %in% paste("sp_",1:length(percent),sep="")) 

curve_fit_final=curve_fit_final[order(npv_11),]


learn.rate.start=1e-9
fit_curve=function(x,b.start){
  #x=curve_fit_final[12,]
  dataset=data.frame(d=x[col_npv],id=x[col_sp])
  a.start <- max(dataset$d)
  b.start <- learn.rate.start
  control1 <- nls.control(maxiter= 1000000, minFactor= 1e-40, warnOnly= TRUE,tol=1e-5)
  nl.reg <- nls(d ~ a * (1-exp(-b * id)),data=dataset,start= list(a=a.start,b=b.start),
                control= control1)
  b <- coef(nl.reg)[2]
  a <- coef(nl.reg)[1]
  res <- sum(resid(nl.reg)^2)
  return(list(a,b,res))
}

a=rep(0,nrow(curve_fit_final))
b=rep(0,nrow(curve_fit_final))

if(round(nrow(curve_fit_final)/20)==0) int=5 else int=round(nrow(curve_fit_final)/20)

while (T) {
  learn.rate.start=learn.rate.start*10
  col_zero=which(a %in% 0)
  if (sum(col_zero)==0) {
    break
  } else{
    for(i in col_zero) {
      if (i%%int==0) print(paste("Curves: ",(round(i/nrow(curve_fit_final),digit=2) * 100),  "% Complete ", sep="",Sys.time()))
      tryCatch({
        fit=fit_curve(x=as.vector(as.matrix(curve_fit_final[i,])),b.start=learn.rate.start)
        a[i]=fit[[1]]
        b[i]=fit[[2]]
      },error=function(e) {
        print(i)
      },finally=next
      ) 
    }
  }
}


final=data.frame(curve_fit_final[,dim,with=F],a=a,b=b)
ex.curve$npv=ex.curve$a*(1-exp(-ex.curve$b*ex.curve$sp_current))

# compute current spend and npv
npv_current=ex.curve[,list(npv=sum(npv)),by=c(dim)]

curve_current=merge(npv_current,sp_current,by=c(bdgt_dim1),all.x=T)
curve_current=curve_current[curve_current$npv!=0,]

# a adjustment
final=merge(final,curve_current,by=c(dim),all.x=T)
final$npv_p=final$a*(1-exp(-final$b*final$sp_current))
final$a=final$a*final$npv/final$npv_p


# output
final$bdgt_id=do.call(paste, c(final[bdgt_dim1], sep = "_"))
match=ex.curve[!duplicated(ex.curve[,c(dim),with=F]),]
match=match[,c(dim,as.vector(do.call(cbind,as.list(gsub("_id","_name",dim))))),with=F]
final=merge(final,match,by=c(dim),all.x=T)


# export files
write.csv(final,"sim_output_curve_ab.csv",row.names=F)
save.image("sim_curve.RData")
