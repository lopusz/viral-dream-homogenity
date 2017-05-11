rm(list = ls(all = TRUE))

library(methods)
library(utils)
library(grDevices)
library(tidyr)
library(ggplot2)

read_data<-function(fname, embargo) {
  stopifnot(embargo==0 || embargo==24)
  df<-read.table(fname,header=TRUE,sep=",")
  subject_id<-df$SUBJECTID
  ans<-df[[2]]
  col_pattern<-sprintf("pred.hour%d.",embargo)
  col_mask<-grepl(col_pattern,colnames(df))
  df<-df[,col_mask]
  colnames(df)<-gsub(col_pattern, "", colnames(df))
  rownames(df)<-subject_id
  df<-data.frame(t(df))
  return(list(pred_prob=df,ans=ans))
}

calc_accuracy_count<-function(pred,ans) {
  return(sum(1*(pred==ans)))
}

calc_accuracy_score<-function(pred,ans) {
  return(sum(1*(pred==ans))/length(ans))
}

calc_f1_score<-function(pred,ans) {
  tp<-sum((pred==1)*(ans==1))
  fn<-sum((pred==0)*(ans==1))
  fp<-sum((pred==1)*(ans==0))
  return(2.0*tp/(2.0*tp+fn+fp))
}

calc_matthews_score<-function(pred,ans) {
  tp<-sum((pred==1)*(ans==1))*1.0
  tn<-sum((pred==0)*(ans==0))*1.0
  fn<-sum((pred==0)*(ans==1))*1.0
  fp<-sum((pred==1)*(ans==0))*1.0
  return((tp*tn-fp*fn)/sqrt((tp+fp)*(tp+fn)*(tn+fp)*(tn+fn)))
}

calc_class_card_score<-function(pred,ans) {
  ans_card<-sum(ans)
  pred_card<-sum(pred)
  return(-abs(ans_card-pred_card))
}

is_0_1<-function(pred_prob_unique) {
  return((length(pred_prob_unique)==1 && (1.0*pred_prob_unique==1.0 || 1.0*pred_prob_unique==0.0)) ||
         (length(pred_prob_unique)==2 && all(1.0*pred_prob_unique==c(0.0,1.0))))
}

find_opt_pred<-function(pred_prob, ans, score_f) {
  res<-pred_prob
  u_pred_prob_plain<-sort(unique(pred_prob))

  # If there are only predictions (i.e. 1 and 0 only), skip  adjusting

  if (!is_0_1(u_pred_prob_plain)) {
    u_pred_prob<-c(u_pred_prob_plain,Inf)
    df<-data.frame(prob=u_pred_prob,score=rep(0.0,length(u_pred_prob)))
    for(i in 1:length(u_pred_prob)) {
      p<-u_pred_prob[i]
      df$score[i]<-score_f(1*(pred_prob<p),ans)
    }
    j<-which.max(df$score)
    res<-1*(pred_prob<df$prob[j])
    #cat(df$score,"\n")
    #cat(j,df$prob[j],"\n")
  }
  return(res)
}

conv_prob_to_pred<-function(pred_prob,ans) {
  res<-data.frame(t(apply(pred_prob,1,
                   function(x) find_opt_pred(x,ans,calc_class_card_score))))
  return(res)
}

conv_pred_to_class<-function(pred,ans) {
  res<-data.frame(t(apply(pred,1,function(x) ans*2+x)))
  res<-data.frame(lapply(res,function(x) factor(x,levels=0:3)))
  colnames(res)<-colnames(pred)
  rownames(res)<-rownames(pred)
  return(res)
}

get_sorted_teams_df<-function(pred,ans,score_f) {
  pred_correct<-t(ans %*% t(rep(1,nrow(pred))))
  score<-apply(pred,1,function(x) score_f(x,ans))
  df<-data.frame(Score=score,Team=names(score),row.names=NULL)
  df<-df[order(df$Score),]
  return(df)
}

get_sorted_subject_ids_df<-function(pred,ans) {
  pred_correct<-t(ans %*% t(rep(1,nrow(pred))))

  score<-apply(pred==pred_correct,2,function(x) sum(x))
  df<-data.frame(Score=score,SubjectID=names(score),row.names=NULL)
  df0<-df[ans==0,]
  df0<-df0[order(-df0$Score),]
  df1<-df[ans==1,]
  df1<-df1[order(-df1$Score),]
  return(rbind(df1,df0))
}

plot_class<-function(class,sorted_subject_ids_df,sorted_teams_df,out_plot_fname) {

  df_tmp<-class
  df_tmp$Team<-rownames(class)
  df<-gather(df_tmp,Team)
  df_tmp<-NULL

  colnames(df)[2]<-"SubjectID"
  colnames(df)[3]<-"Class"

  df$SubjectID<-factor(df$SubjectID,levels=sorted_subject_ids_df$SubjectID)
    levels(df$SubjectID)<-sprintf("[%2d] %s", sorted_subject_ids_df$Score, sorted_subject_ids_df$SubjectID)

  df$Team<-factor(df$Team,levels=sorted_teams_df$Team)
  levels(df$Team)<-sprintf("%s [%2.0f]", sorted_teams_df$Team, sorted_teams_df$Score)

  class_level_names<-c("0, pred right",
                       "0, pred wrong",
                       "1, pred wrong",
                       "1, pred right")

  df$Class[df$Class=="0"]<-class_level_names[1]
  df$Class[df$Class=="1"]<-class_level_names[2]
  df$Class[df$Class=="2"]<-class_level_names[3]
  df$Class[df$Class=="3"]<-class_level_names[4]
  df$Class<-factor(df$Class,levels=class_level_names)

  class_colors<-c("#d7191c","#fdae61","#a6d96a","#1a9641")
  names(class_colors)<-class_level_names

  title<-gsub("\\.[^\\.]*$","",basename(out_plot_fname))

  pdf(file=out_plot_fname)
  p<-ggplot()+
     geom_raster(data=df,aes(x=SubjectID,y=Team,fill=Class))+
     scale_fill_manual(values = class_colors)+
     ggtitle(title)+
     theme_bw()+
     theme(axis.text.x = element_text(angle = 315, hjust = 0))
  show(p)
  dev.off()
  return(df)
}

main<-function(inp_data_fname,out_plot_fname) {
  tmp<-read_data(inp_data_fname,24)
  pred_prob<-tmp$pred_prob
  ans<-tmp$ans
  tmp<-NULL

  pred<-conv_prob_to_pred(pred_prob,ans)

  # Fake data for test purposes
  # pred<-data.frame(P1=c(1,0,0,1),P2=c(0,1,0,1),P3=c(1,1,1,1),P4=c(0,0,0,1),row.names=c("A","B","C","D"))
  # ans<-c(1,0,1,0)

  sorted_subject_ids_df<-get_sorted_subject_ids_df(pred,ans)
  sorted_teams_df<-get_sorted_teams_df(pred,ans,calc_accuracy_count)

  class<-conv_pred_to_class(pred,ans)

  z<-plot_class(class,sorted_subject_ids_df,sorted_teams_df,out_plot_fname)
  return(list(class=class,pred_prob=pred_prob,pred=pred,ans=ans,z=z))
}

args<-commandArgs(trailingOnly = TRUE)

if (length(args)>1) {
  res<-main(args[[1]],args[[2]])
}
