
test_that("not really proper tests of mids functions", {
   if(require(mice)){
      # test mids_subset
      df <- data.frame(
         x =c(1,1,3,NA),
         y = c("a", "a", "a", "b"),
         z = c(1, 0, 0, 0),
         u = c(3.1, 2.2, 3.3, 2.7)
      )
      df2 <- df
      row.names(df) <- c("Ulrika", "Johan", "Niclas", "Henrik")

      object <- mice(data = df, m=5) # char
      row.names(object$data)
      row.names(object$imp$x)
      sob <- mids_subset(object, c(4,1))
      row.names(sob$data)
      row.names(sob$imp$x)

      object2 <- mice(data=df2, m=5) # int
      row.names(object2$data)
      row.names(object2$imp$x)
      sob2 <- mids_subset(object2, c(4,1))
      row.names(sob2$data)
      row.names(sob2$imp$x)

      # test mids_subset_2
      df <- data.frame(
         x =c(1,1,3,NA),
         y = c("a", "a", "a", "b"),
         z = c(1, 0, 0, 0),
         u = c(3.1, 2.2, 3.3, 2.7)
      )
      df2 <- df
      row.names(df) <- c("Ulrika", "Johan", "Niclas", "Henrik")

      object <- mice(data = df, m=5) # char
      row.names(object$data)
      row.names(object$imp$x)
      sob <- mids_subset_2(object, c(4,1,4,4))
      row.names(sob$data)
      row.names(sob$imp$x)

      object2 <- mice(data=df2, m=5) # int
      row.names(object2$data)
      row.names(object2$imp$x)
      sob2 <- mids_subset_2(object2, c(4,4,4,1,4,4,4,4,4,4,4,4,4))
      row.names(sob2$data)
      row.names(sob2$imp$x)

      # predict functions
      df <- gimme_some_data(1000)
      mdf <- mice(df)
      df2 <- gimme_some_data(100)
      mdf2 <- mice(df2)
      cbind(
         mids_predict_logreg_2(mdf, "y~x+z", mdf2),
         mids_predict_logreg(mdf, "y~x+z", df2)
      )

      # dunno
      df <- gimme_some_data(50)
      object <- mice(df)
      mids_describe(object)

      mids_get(object, 1)
      index <- 1:25
      sobject <- mids_subset(object, index)
      class(sobject)
      str(sobject$data)
      rownames(sobject$imp$x)
      rownames(object$imp$x)
      rownames(sobject$imp$z)
      rownames(object$imp$z)

   }
})
