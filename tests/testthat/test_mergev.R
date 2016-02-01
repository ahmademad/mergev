dfa <- data.frame(key=letters[1:10],
                  key2=LETTERS[1:10],
	                cola=paste("a",letters[1:10], sep=":"))
lt  <- c("a", "b", "b", "c","c","d", "d", "e","f","f")
lt2 <- c("A", "B", "Z", "C","Z","A", "D", "L","F","Z")
dfb <- data.frame(key=lt,
                  key2=lt2,
	                colb=paste("b",lt, lt2, sep=":"))
lt <- rep(letters[10],10)
lt2 <- rep(c(LETTERS[10],"A"),each=5)
dfq <- data.frame(key=lt,
                  key2=lt2,
	                colq=paste("q",lt,lt2, sep=":"))

test_that("mergev can merge",{
	mgv <- mergev(dfa, dfb, by="key", all=TRUE, verbose=FALSE, )
	expect_that(mgv, is_a("data.frame"))

	mgv <- mergev(dfa, dfq, by="key", all=TRUE, verbose=FALSE)
	expect_that(mgv, is_a("data.frame"))

	mgv <- mergev(dfa, dfb, by="key", all=FALSE, verbose=FALSE)
	expect_that(mgv, is_a("data.frame"))

	mgv <- mergev(dfa, dfq, by="key", all=FALSE, verbose=FALSE)
	expect_that(mgv, is_a("data.frame"))
})

dfl <- list(dfa, dfb, dfq)
byl <- list("key",0,c("key","key2"),c(0,1,2),c(1,0,2), c(1,2,0),"key2")
test_that("mergev agrees with merge",{
  dfi1 <- 1
  dfi2 <- 2
  while(! (dfi1 == (length(dfl)-1) & dfi2==length(dfl))) {
    dfi2 <- dfi2 + 2
    if(dfi2 > length(dfl)) {
      dfi1 <- dfi1 + 1
      dfi2 <- dfi1 + 1
    }
    df1 <- dfl[[dfi1]]
    df2 <- dfl[[dfi2]]
    for(byi in 1:length(byl)) {
      thisby <- byl[[byi]]
      mg <- merge(df1, df2, by=thisby, all=TRUE)
      mgv <- mergev(df1, df2, by=thisby, all=TRUE, verbose=FALSE)
      mgv$merge.type <- NULL
      expect_that(mg, equals(mgv))

      mg <- merge(df1, df2, by=thisby, all=FALSE)
      mgv <- mergev(df1, df2, by=thisby, all=FALSE, verbose=FALSE)
      mgv$merge.type <- NULL
      expect_that(mg, equals(mgv))

      mg <- merge(df1, df2, by=thisby, all.x=FALSE, all.y=TRUE)
      mgv <- mergev(df1, df2, by=thisby, all.x=FALSE, all.y=TRUE, verbose=FALSE)
      mgv$merge.type <- NULL
      expect_that(mg, equals(mgv))
      
      mg <- merge(df1, df2, by=thisby, all.x=TRUE, all.y=FALSE)
      mgv <- mergev(df1, df2, by=thisby, all.x=TRUE, all.y=FALSE, verbose=FALSE)
      mgv$merge.type <- NULL
      expect_that(mg, equals(mgv))
      
    }
  }    

})

test_that("characteristics correct",{
  dfi1 <- 1
  dfi2 <- 2
  while(! (dfi1 == (length(dfl)-1) & dfi2==length(dfl))) {
    dfi2 <- dfi2 + 2
    if(dfi2 > length(dfl)) {
      dfi1 <- dfi1 + 1
      dfi2 <- dfi1 + 1
    }
    df1 <- dfl[[dfi1]]
    df2 <- dfl[[dfi2]]
    for(byi in 1:length(byl)) {
      thisby <- byl[[byi]]
      mgv <- mergev(df1, df2, by=thisby, all=TRUE, verbose=FALSE, return.list=TRUE)
      # test characteristics of the output
      
    }
  }    

})

