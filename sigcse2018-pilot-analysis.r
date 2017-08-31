library(psych)
setwd("/Users/kevin/Documents/work/Research/Testoscope/IntentionalFailurePilotExperiment-Spring2017/analysis")
everything <- read.csv(file="pilot-metadata.csv", header=T, sep=",")
everything[everything==""] <- NA

cat("\n\n*****Initial descriptive info")
describeBy(everything$initial_tests, everything$group)
describeBy(everything$initial_solution_conditionals, everything$group)
table(everything$initial_builds)

cat("\n\n*****Initial assignment LINE COVERAGE")
describeBy(everything$initial_coverage_line, everything$group)
shapiro.test(everything$initial_coverage_line)
wilcox.test(everything$initial_coverage_line[everything$group == "Correct"],
            everything$initial_coverage_line[everything$group == "Buggy"])

cat("\n\n*****Initial assignment CONDITIONAL COVERAGE")
describeBy(everything$initial_coverage_conditional, everything$group)
shapiro.test(everything$initial_coverage_conditional)
wilcox.test(everything$initial_coverage_conditional[everything$group == "Correct"],
            everything$initial_coverage_conditional[everything$group == "Buggy"])

cat("\n\n*****Initial assignment REFERENCE SOLUTION CONDITIONAL COVERAGE")
describeBy(everything$initial_ref_solution_coverage_conditional, everything$group)
shapiro.test(everything$initial_ref_solution_coverage_conditional)
wilcox.test(everything$initial_ref_solution_coverage_conditional[everything$group == "Correct"],
            everything$initial_ref_solution_coverage_conditional[everything$group == "Buggy"])

cat("\n\n*****Initial assignment CORRECTNESS")
describeBy(everything$initial_ref_tests_passed, everything$group)
shapiro.test(everything$initial_ref_tests_passed)
wilcox.test(everything$initial_ref_tests_passed[everything$group == "Correct"],
            everything$initial_ref_tests_passed[everything$group == "Buggy"])

cat("\n\n*****REVISED ASSIGNMENT*****")
cat("\n\n*****Revised assignment LINE COVERAGE")
describeBy(everything$revised_coverage_line, everything$group)
shapiro.test(everything$revised_coverage_line)
wilcox.test(
  everything$revised_coverage_line[everything$group == "Correct"],
  everything$revised_coverage_line[everything$group == "Buggy"])

cat("\n\n*****Revised assignment CONDITIONAL COVERAGE")
describeBy(everything$revised_coverage_conditional, everything$group)
shapiro.test(everything$revised_coverage_conditional)
wilcox.test(
  everything$revised_coverage_conditional[everything$group == "Correct"],
  everything$revised_coverage_conditional[everything$group == "Buggy"])


cat("\n\n*****Revised SOLUTION CORRECTNESS")
describeBy(everything$revised_ref_tests_passed, everything$group)
shapiro.test(everything$revised_ref_tests_passed)
wilcox.test(everything$revised_ref_tests_passed[everything$group == "Correct"],
            everything$revised_ref_tests_passed[everything$group == "Buggy"])


cat("\n\n*****Revised TESTING TRUE POSITIVES")
describeBy(everything$revised_peer_correct_pass, everything$group)
shapiro.test(everything$revised_peer_correct_pass)
wilcox.test(everything$revised_peer_correct_pass[everything$group == "Correct"],
            everything$revised_peer_correct_pass[everything$group == "Buggy"])

cat("\n\n*****Revised TESTING TRUE NEGATIVES")
describeBy(everything$revised_peer_incorrect_fail, everything$group)
shapiro.test(everything$revised_peer_incorrect_fail)
wilcox.test(everything$revised_peer_incorrect_fail[everything$group == "Correct"],
            everything$revised_peer_incorrect_fail[everything$group == "Buggy"])

cat("\n\n*****QUIZ TESTING TRUE POSITIVES")
describeBy(everything$quiz_peer_correct_pass, everything$group)
shapiro.test(everything$quiz_peer_correct_pass)
wilcox.test(everything$quiz_peer_correct_pass[everything$group == "Correct"],
            everything$quiz_peer_correct_pass[everything$group == "Buggy"])

cat("\n\n*****QUIZ TESTING TRUE NEGATIVES")
describeBy(everything$quiz_peer_incorrect_fail, everything$group)
shapiro.test(everything$quiz_peer_incorrect_fail)
wilcox.test(everything$quiz_peer_incorrect_fail[everything$group == "Correct"],
            everything$quiz_peer_incorrect_fail[everything$group == "Buggy"])


# everything$quiz_test_fully_true = everything$quiz_peer_correct_pass == 1 & everything$quiz_peer_incorrect_fail == 1
# everything$quiz_test_trueness = everything$quiz_peer_correct_pass - everything$quiz_peer_incorrect_pass
# describeBy(everything$quiz_test_trueness, everything$group)
# shapiro.test(everything$quiz_test_trueness)
# wilcox.test(everything$quiz_test_trueness[everything$group == "Correct"],
#             everything$quiz_test_trueness[everything$group == "Buggy"])
# everything$quiz_test_fail_accuracy = everything$quiz_peer_incorrect_fail - everything$quiz_peer_correct_fail
# describeBy(everything$quiz_test_fail_accuracy, everything$group)
# shapiro.test(everything$quiz_test_fail_accuracy)
# wilcox.test(everything$quiz_test_fail_accuracy[everything$group == "Correct"],
#             everything$quiz_test_fail_accuracy[everything$group == "Buggy"])

# plot(quiz_peer_correct_pass~quiz_peer_incorrect_fail, data=everything)

# describe(everything$quiz_peer_correct_pass)
# quantile(everything$quiz_peer_correct_pass, na.rm=TRUE)
# iqr = IQR(everything$quiz_peer_correct_pass, na.rm=TRUE)
# upperq = quantile(everything$quiz_peer_correct_pass, na.rm=TRUE)[4]
# lowerq = quantile(everything$quiz_peer_correct_pass, na.rm=TRUE)[2]
# upperthresh = iqr * 1.5 + upperq
# lowerthresh = lowerq - iqr * 1.5
# 
# describe(everything$quiz_peer_incorrect_fail)
# quantile(everything$quiz_peer_incorrect_fail, na.rm=TRUE)
# iqr = IQR(everything$quiz_peer_incorrect_fail, na.rm=TRUE)
# upperq = quantile(everything$quiz_peer_incorrect_fail, na.rm=TRUE)[4]
# lowerq = quantile(everything$quiz_peer_incorrect_fail, na.rm=TRUE)[2]
# upperthresh = iqr * 1.5 + upperq
# lowerthresh = lowerq - iqr * 1.5


# describe(everything$quiz_peer_incorrect_fail)
# quantile(everything$quiz_peer_incorrect_fail, na.rm=TRUE)
# 
# plot(everything$revised_peer_correct_pass~everything$revised_peer_incorrect_fail, 
#      main="Exercise Testing Outcomes", ylab="Correct Passed", xlab="Buggy Failed")



cor.test(everything$revision_peer_correct_pass,everything$revision_peer_incorrect_fail,method="spearman",conf.level = 0.05)

cor.test(everything$quiz_peer_correct_pass,everything$quiz_peer_incorrect_fail,method="spearman",conf.level = 0.05)

library(ggplot2); 
qplot(revised_peer_incorrect_fail,
      revised_peer_correct_pass,
      color=group, 
      data=everything,
      main="Exercise Testing Outcomes", 
      ylab="Positive Passed", 
      xlab="Negative Failed"
      ) + theme_bw() + scale_color_manual(breaks = c("Buggy", "Correct"), values=c("#000000", "#AAAAAA"))

qplot(quiz_peer_incorrect_fail,
      quiz_peer_correct_pass,
      color=group, 
      data=everything,
      main="Quiz Testing Outcomes", 
      ylab="Positive Passed", 
      xlab="Negative Failed"
      ) + theme_bw() + scale_color_manual(breaks = c("Buggy", "Correct"), values=c("#000000", "#AAAAAA"))

cat("\n\n*****REVISION CORRELATION")
cor.test(everything$revised_peer_correct_pass,everything$revised_peer_incorrect_fail,method="spearman",conf.level = 0.05)

cat("\n\n*****QUIZ CORRELATION")
cor.test(everything$quiz_peer_correct_pass,everything$quiz_peer_incorrect_fail,method="spearman",conf.level = 0.05)


#combine from different assignments stacked in same columns...
quiz <- everything[c("id", "group", "quiz_ref_tests_passed", "quiz_peer_correct_pass", "quiz_peer_incorrect_fail")]
colnames(quiz) <- c("id", "group", "ref_tests_passed", "peer_correct_pass", "peer_incorrect_fail")
quiz$assignment = "quiz"
exercise <- everything[c("id", "group", "revised_ref_tests_passed", "revised_peer_correct_pass", "revised_peer_incorrect_fail")]
colnames(exercise) <- c("id", "group", "ref_tests_passed", "peer_correct_pass", "peer_incorrect_fail")
exercise$assignment = "exercise"
results = rbind(quiz,exercise) 
results <- results[complete.cases(results),]

set.seed(430) #static seeding for clustering
results.fit = kmeans(results[4:5], 3) #find 3 clusters from peer_correct_pass and peer_incorrect_fail
results$cluster[results$cluster==1] = "A"
results$cluster[results$cluster==2] = "B"
results$cluster[results$cluster==3] = "C"
aggregate(results,by=list(results.fit$cluster),FUN=mean)
aggregate(results,by=list(results.fit$cluster),FUN=sd)

# qplot(peer_incorrect_fail,
#       peer_correct_pass,
#       group=cluster, 
#       data=results,
#       main="Clustered Testing Outcomes", 
#       ylab="Correct Passed", 
#       xlab="Incorrect Failed"
# ) + theme_bw()

ggplot(results, aes(peer_incorrect_fail, peer_correct_pass, group=cluster)) +
  ggtitle("Clustered Testing Outcomes") +
  xlab("Negative Failed") +
  ylab("Positive Passed") +
  geom_point(aes(shape=results$cluster, color=results$cluster)) +
  scale_colour_manual(breaks=c("A","B","C"),values=c('A'="#000000", 'B'="#444444", 'C'="#999999"),guide=F) +
  scale_shape_manual(breaks=c("A","B","C"),values=c('A'=0, 'B'=1, 'C'=2),guide=F) +
  theme_light()
  

describeBy(results$ref_tests_passed,results$cluster)
shapiro.test(results$ref_tests_passed)
wilcox.test(results$ref_tests_passed[results$cluster == 1],
            results$ref_tests_passed[results$cluster == 2])
wilcox.test(results$ref_tests_passed[results$cluster == 1],
            results$ref_tests_passed[results$cluster == 3])
wilcox.test(results$ref_tests_passed[results$cluster == 2],
            results$ref_tests_passed[results$cluster == 3])

