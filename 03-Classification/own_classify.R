library(tm)
library(ggplot2)


# 分類器の訓練用
spam.path <- "data/spam/"
easyham.path <- "data/easy_ham/"
hardham.path <- "data/hard_ham/"

# 分類器のテスト用
spam2.path <- "data/spam_2/"
easyham2.path <- "data/easy_ham_2/"
hardham2.path <- "data/hard_ham_2/"

# 各ファイルを開いて最初の空行を見つけ その下のテキストを単一のテキスト要素を含む文字ベクトルとして返す
get.msg <- function(path) {
  # 読み込みモードrt で開く
  con <- file(path, open="rt", encoding="latin1")
  text <- readLines(con)

  msg <- text[seq(which(text=="")[1] + 1, length(text), 1)]
  close(con)

  #pasteをつかってベクトルを単一の文字要素にする collapesを指定して終了を定義
  return(paste(msg,collapes="\n"))
}

# 分類器訓練  すべての電子メールからメッセージを取り出す必要
# その方法の１つとして すべてのメッセージを含み、 ベクトルの各要素が単一の電子メールとなるようなベクトルを作る
# ソレを行う方法として get.msgを applyと一緒に使う

spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]

# get.msgをすべてのスパムファイルメインい対して適用する
# ファイルの内容が戻り値として買えるので、ソレを用いてメッセージベクトルを構築する
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path, p, sep ='/')))

get.tdm <- function(doc.vec) {
  doc.corpus <- Corpus(VectorSource(doc.vec))

  # どのようにテキストを精製するかオプションを指定
  # コーパスに２回以上出現する単語のみがTDMに現れるようにする
  control <- list(stopwords=TRUE, removePunctuation=TRUE, removeNumbers=TRUE, minDocFreq=2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}

# spamのdtmを取得
spam.dtm <- get.tdm(all.spam)

# ここまでで構築したTDMを利用して スパムの訓練データを構築できるようになった

# ある単語が観察されたときに、その電子メールがスパムである確率がわかるように分類器を訓練する

# as.matrixを使ってTDMオブジェクトを通常のR行列に変換する
spam.matrix <- as.matrix(spam.dtm)

# rowSumsによって 各単語の全文暑中の合計頻度を含むベクトルを作る
spam.counts <- rowSums(spam.matrix)

# data.frameによって 文字ベクトルと数値ベクトルを結合
# 頻度カウントが文字列に変換されないように stringsAsFactors falseにする
spam.df <- data.frame(cbind(names(spam.counts), as.numeric(spam.counts)), stringsAsFactors=FALSE)

# 訓練データを生成する
# まず ある単語が出現した文書の割合を計算する
# そのためにはsapplyを使ってすべての行を匿名関数に渡して正の数を持つ要素数を数え、
# 次のその合計をTDMの列の数 すなわちスパムコーパスの文書数で割ることにより実現できる
names(spam.df) <- c("term", "frequency")
spam.df$frequency <- as.numeric(spam.df$frequency)

spam.occurrence <- sapply(1:nrow(spam.matrix), function(i) {
  length(which(spam.matrix[i,] > 0)) / ncol(spam.matrix)
})

# 各単語のコーパス全体での頻度を計算する
spam.density <- spam.df$frequency / sum(spam.df$frequency)

# spam.occurrenceとspam.densityをデータフレームに追加する これでスパム分類の教師データの生成完了
spam.df <- transform(spam.df, density=spam.density, occurrence=spam.occurrence)


#print(head(spam.df[with(spam.df, order(-occurrence)),], 1000))

#print(head(spam.df[with(spam.df, order(-frequency)),], 1000))



# 分類器訓練 非スパム(easy)を読み出す
# spamと同様の処理を行っていく
easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs!="cmds")]

all.easyham <- sapply(easyham.docs[1:length(spam.docs)],
                      function(p) get.msg(file.path(easyham.path, p)))

#print(head(spam.df[with(spam.df, order(-occurrence)),], 1000))

#print(head(spam.df[with(spam.df, order(-frequency)),], 1000))

# easyhamのdtmを取得
easyham.dtm <- get.tdm(all.easyham)
easyham.matrix <- as.matrix(easyham.dtm)

easyham.counts <- rowSums(easyham.matrix)
easyham.df <- data.frame(cbind(names(easyham.counts), as.numeric(easyham.counts)), stringsAsFactors=FALSE)
names(easyham.df) <- c("term", "frequency")
easyham.df$frequency <- as.numeric(easyham.df$frequency)

easyham.occurrence <- sapply(1:nrow(easyham.matrix), function(i) {
  length(which(easyham.matrix[i,] > 0)) / ncol(easyham.matrix)
})

easyham.density <- easyham.df$frequency / sum(easyham.df$frequency)
easyham.df <- transform(easyham.df, density=easyham.density, occurrence=easyham.occurrence)


print(head(easyham.df[with(easyham.df, order(-occurrence)),], 1000))
