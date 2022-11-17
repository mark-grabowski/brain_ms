#From Liam Revell's web page
#http://blog.phytools.org/2012/01/function-to-get-descendant-node-numbers.html

library(phytools)

getDescendants<-function(tree,node,curr=NULL){
  #plot(tree,font=0.25); nodelabels(bg="white")
  if(is.null(curr)) curr<-vector()
  daughters<-tree$edge[which(tree$edge[,1]==node),2]
  curr<-c(curr,daughters)
  w<-which(daughters>=length(tree$tip))
  if(length(w)>0) for(i in 1:length(w)) 
    curr<-getDescendants(tree,daughters[w[i]],curr)
  return(curr)
}


#Example of how it works
#tree<-pbtree(n=10)
#tree<-reorder(tree,"postorder")
#tree$tip.label<-1:10
# this lines up tip numbers & labels for simplicity
#par(mar=c(1,1,1,1)) # set margins
#plot(tree,font=1); nodelabels(bg="white")
#getDescendants(tree,node=17)
#[1] 18 6 7 8




bayou2slouch<-function(trdata,bayou.shifts){
  #returns a new column of the dataset - bayou.shifts
  #also returns internal node assignment
  #Make sure you send a merged tree and data file using bayou setup
  
  #Change color for Plots
  library(ggplot2)
  #source("/Users/markgrabowski/Google Drive/Shared with Mac Pro/Brain Size Diet + Social System/Programs/getDescendants.R")
  
  #Get ggplot colors used for plot to make on tree
  gg_color_hue <- function(n) {
    hues = seq(15, 375, length=n+1)
    hcl(h=hues, l=65, c=100)[1:n]
  }
  
  n.tips<-length(trdata$phy$tip.label)
  num.internal.nodes<-n.tips+(1:trdata$phy$Nnode)
  n.internal.nodes<-length(num.internal.nodes)
  
  #First set all exant tips to OU1
  trdata$dat$bayou.shifts<-"OU1"
  #Assign all internal nodes to OU1
  internal.nodes.bayou.shifts<-rep("OU1",n.internal.nodes)
  
  #Shifts from Bayou Species + BoM AAPA 03202019 Code - saved on Google Drive
  #OU2
  for(i in 2:(length(bayou.shifts))){
    print(i)
    print(bayou.shifts[i])
    if(bayou.shifts[i]<=n.tips)#shift on single branch
    {
      #Assign external nodes/tips to OUi
      trdata$dat$bayou.shifts[bayou.shifts[i]]<-paste("OU",sep="",i)
      #trdata$phy$tiplabel
      
    }
    else{
      saved.decendants<-getDescendants(trdata$phy,node=bayou.shifts[i])
      external.nodes<-saved.decendants[saved.decendants<=n.tips]
      internal.nodes<-saved.decendants[saved.decendants>n.tips]
    
      #Assign external nodes/tips to OUi
      trdata$dat$bayou.shifts[external.nodes]<-paste("OU",sep="",i)
    
      #Assign internal nodes to OUi
      internal.nodes.bayou.shifts[internal.nodes-n.tips]<-paste("OU",sep="",i)
      
      #Assign node where shift occurs to OUi
      internal.nodes.bayou.shifts[bayou.shifts[i]-n.tips]<-paste("OU",sep="",i)
    }
  }  
  #Make colors for regimes
  reg.colors<-gg_color_hue(length(bayou.shifts))
  
  #Combine external coding and internal coding to plot tree with colored shifts
  bayou.shifts.total<-c(trdata$dat$bayou.shifts,internal.nodes.bayou.shifts)
  edge.regimes <- factor(bayou.shifts.total[trdata$phy$edge[,2]])
  print(edge.regimes)
  print(reg.colors)
  plot(trdata$phy,edge.color = reg.colors[edge.regimes], edge.width = 1, cex = 0.2)
  return(list(trdata,internal.nodes.bayou.shifts))
}
  