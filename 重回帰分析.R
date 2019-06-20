

data = read.csv("C:/cygwin64/home/kaito/Program/R_PCA/SSTave.csv")


# データフレームの各列をベクトルに代入

for(i in 1:22){
  nam <- paste("var_",i,sep="")
  assign(nam,c(data[,i]))
  
}


lm1 <- lm(var_1 ~
            var_2 + var_3 + var_4 + var_5 + var_6 + var_7 + var_8 + var_9 + var_10 + var_11 + var_12 + var_13 + var_14 + var_15 + var_16 + var_17 + var_18 + var_19 + var_20 + var_21 + var_22 ,
          data=data)

summary(lm1)                # 分析結果を表示
round(coefficients(lm1), 2) # 係数を表示


# 分析のもとになったデータと予測値、残差を一覧にする
exp1 <- predict(lm1)         # 元データに対する予測値
res1 <- residuals(lm1)       # データと予測値の残差
view_lm1<- data.frame(data[1], exp1, res1) #

write.csv(view_lm1,file = "Mr1.csv")

##################################################
# 増減法で説明変数を減らして重回帰分析
##################################################
# 説明変数を減らしたAIC値を求める

stp<-step(lm1)

# 不要な説明変数を外して線形単回帰分析を実行
lm2 <- lm(var_1 ~
            var_4 + var_6 + var_8 + var_9 + var_12 + var_14 + var_16 + var_17 + var_21 ,
          data=data)
summary(lm2)                # 分析結果を表示
round(coefficients(lm2), 2) # 係数を表示

# 分析のもとになったデータと予測値、残差を一覧にする
exp2 <- predict(lm2)         # 元データに対する予測値
res2 <- residuals(lm2)       # データと予測値の残差
view_lm2 <- data.frame(data[1], exp2, res2) # データフレームにまとめる

write.csv(view_lm2,file = "Mr2.csv")

hist(var_2)


################################################
# 不要な相互作用を除いて重回帰分析
################################################
# 相互作用を考慮した重回帰分析を実行
lm3 <- lm(var_1 ~
            (var_2 + var_3 + var_4 + var_5 + var_6 + var_7 + var_8 + var_9 + var_10 + var_11 + var_12 + var_13 + var_14 + var_15 + var_16 + var_17 + var_18 + var_19 + var_20 + var_21 + var_22) ^2,
          data=data)
summary(lm3)

# 相互作用を1つずつ外してAIC値を求める
lm4 <- step(lm3)

# 分析のもとになったデータと予測値、残差を一覧にする
exp <- predict(lm4)         # 元データに対する予測値
res <- residuals(lm4)       # データと予測値の残差
view_lm4 <- data.frame(data[1], exp, res) # データフレームにまとめる

# 係数を表示
round(coefficients(lm4), 2)