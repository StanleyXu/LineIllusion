plotPoly=function(x,y){
  l=length(x)
  for(i in 1:(l-1)){
    segments(x[i],y[i],x[i+1],y[i+1])
  }
  segments(x[l],y[l],x[1],y[1])
}

recPoly=function(x,y,p=0.1){
  l=length(x)
  d=sqrt((x[1]-x[l])^2+(y[1]-y[l])^2)
  #message(d)
  if(d>0.0001){
    x1.n=(x[2]-x[1])*p+x[1]
    y1.n=(y[2]-y[1])*p+y[1]
    segments(x[l],y[l],x1.n, y1.n)
    x.new=c(x[2:l],x1.n)
    y.new=c(y[2:l],y1.n)
    recPoly(x.new, y.new, p)
  }
}


n=6
require(deldir)
x=runif(n)
y=runif(n)
plot(x,y, xlab='',ylab='', xaxt='n', yaxt='n', type='n')
dd=deldir(x,y)
tl=tile.list(dd)
l=length(tl)
rv=rbinom(l,1,0.5)
for(i in 1:l){
  tl.i=tl[[i]]
  tx=tl.i$x; ty=tl.i$y
  if(rv[i]==1){
    tx=rev(tx); ty=rev(ty);
  }
  edge=length(tx)
  if(edge<5){
    plotPoly(tx,ty)
    recPoly(tx,ty,p=0.1)
  }else{
    x1=tx[1:3]; y1=ty[1:3]
    x2=tx[c(1,3:edge)]; y2=ty[c(1,3:edge)]
    plotPoly(x1,y1)
    recPoly(x1,y1,p=0.1)
    plotPoly(x2,y2)
    recPoly(x2,y2,p=0.1)    
  }
}