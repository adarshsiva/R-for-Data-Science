#Predicting house prices
    # Create a tibble with n_convenience column from zero to ten
        explanatory_data <- tibble(n_convenience=0:10)
    # From previous step
    explanatory_data <- tibble(
    n_convenience = 0:10
    )
    # Use mdl_price_vs_conv to predict with explanatory_data
        predict(mdl_price_vs_conv, explanatory_data)
    # From previous steps
        explanatory_data <- tibble(
        n_convenience = 0:10
        )

    # Edit this, so predictions are stored in prediction_data
        prediction_data <- explanatory_data %>%
        mutate(
        predict(mdl_price_vs_conv, explanatory_data)
        ) 

    # See the result
        prediction_data
#Visualizing predictions
    # Add to the plot
        ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE) +
    # Add a point layer of prediction data, colored yellow
        geom_point(data = prediction_data, color = "yellow")
#The limits of prediction
    # Define a tibble where n_convenience is -1
        minus_one <- tibble (n_convenience = -1)

    # Define a tibble where n_convenience is 2.5
        two_pt_five <- tibble(n_convenience = 2.5)
#Try making predictions on your two impossible cases. What happens?
    The model successfully gives a prediction about cases that are impossible in real life.
#Extracting model elements
    # Get the model coefficients of mdl_price_vs_conv
        coefficients(mdl_price_vs_conv)
    # Get the fitted values of mdl_price_vs_conv
        fitted(mdl_price_vs_conv)
    # Get the residuals of mdl_price_vs_conv
        residuals(mdl_price_vs_conv)
    # Print a summary of mdl_price_vs_conv
        summary(mdl_price_vs_conv)
# Get the coefficients of mdl_price_vs_conv
    coeffs <- coefficients(mdl_price_vs_conv)
# Get the intercept
    intercept <- coeffs[1]
# Get the slope
    slope <- coeffs[2]
    explanatory_data %>% 
     mutate(
     # Manually calculate the predictions
     price_twd_msq = intercept + (slope*n_convenience)
    )
# Compare to the results from predict()
    predict(mdl_price_vs_conv, explanatory_data)
# Get the coefficient-level elements of the model
    tidy(mdl_price_vs_conv)
# Get the observation-level elements of the model
    augment(mdl_price_vs_conv)
# Get the model-level elements of the model
    glance(mdl_price_vs_conv)
#Home run!
    Someone who hit 40 home runs in 2017 is predicted to hit 10 fewer home runs the next year because regression to the mean states that, on average, extremely high values are not sustained.
# Using sp500_yearly_returns, plot return_2019 vs. return_2018
    ggplot(sp500_yearly_returns,aes(return_2018,return_2019)) +
  # Make it a scatter plot
     geom_point() +
  # Add a line at y = x, colored green, size 1
    geom_abline(color = "green",size=1) +
  # Add a linear regression trend line, no std. error ribbon
    geom_smooth(method = "lm", se = FALSE) +
  # Fix the coordinate ratio
    coord_fixed(ratio = 1)
# From previous step
    mdl_returns <- lm(
    return_2019 ~ return_2018, 
    data = sp500_yearly_returns
    )
# Create a data frame with return_2018 at -1, 0, and 1 
    explanatory_data <- tibble(return_2018=-1:1)
# Use mdl_returns to predict with explanatory_data
    predict(mdl_returns, explanatory_data)
#Transforming the explanatory variable
    # Run the code to see the plot
    # Edit so x-axis is square root of dist_to_mrt_m
        ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq)) +
        geom_point() +
        geom_smooth(method = "lm", se = FALSE)
    # Run a linear regression of price_twd_msq vs. 
    # square root of dist_to_mrt_m using taiwan_real_estate
        mdl_price_vs_dist <- lm(price_twd_msq ~ sqrt(dist_to_mrt_m), taiwan_real_estate)
    # See the result
        mdl_price_vs_dist
    # From previous step
        mdl_price_vs_dist <- lm(
        price_twd_msq ~ sqrt(dist_to_mrt_m), 
        data = taiwan_real_estate
        )
    # Use this explanatory data
        explanatory_data <- tibble(
        dist_to_mrt_m = seq(0, 80, 10) ^ 2
        )
    # Use mdl_price_vs_dist to predict explanatory_data
        prediction_data <- explanatory_data %>%
        mutate(predict(mdl_price_vs_dist, explanatory_data))
    # See the result
        prediction_data
    # From previous steps
        mdl_price_vs_dist <- lm(
        price_twd_msq ~ sqrt(dist_to_mrt_m), 
        data = taiwan_real_estate
        )
        explanatory_data <- tibble(
        dist_to_mrt_m = seq(0, 80, 10) ^ 2
        )
        prediction_data <- explanatory_data %>% 
        mutate(
         price_twd_msq = predict(mdl_price_vs_dist, explanatory_data)
        )

        ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq)) +
         geom_point() +
         geom_smooth(method = "lm", se = FALSE) +
     # Add points from prediction_data, colored green, size 5
         geom_point(color="green",size=5,data=prediction_data)
#Transforming the response variable too
    # Run the code to see the plot
    # Edit to raise x, y aesthetics to power 0.25
        ggplot(ad_conversion, aes(n_impressions^0.25, n_clicks^0.25)) +
         geom_point() +
         geom_smooth(method = "lm", se = FALSE)
         # Run a linear regression of n_clicks to the power 0.25 vs. 
     # n_impressions to the power 0.25 using ad_conversion
        mdl_click_vs_impression <- lm(formula= I(n_clicks^0.25) ~ I(n_impressions^0.25),data=ad_conversion)
      # From previous step
        mdl_click_vs_impression <- lm(
        I(n_clicks ^ 0.25) ~ I(n_impressions ^ 0.25),
        data = ad_conversion
        )

    # Use this explanatory data
        explanatory_data <- tibble(
          n_impressions = seq(0, 3e6, 5e5)
        )

        prediction_data <- explanatory_data %>% 
         mutate(
      # Use mdl_click_vs_impression to predict n_clicks ^ 0.25
          n_clicks_025 = predict(mdl_click_vs_impression, explanatory_data),
     # Back transform to get n_clicks
         n_clicks = n_clicks_025 ^ 4
         )
    # Add points from prediction_data, colored green
        geom_point(color="green",data=prediction_data)
#Coefficient of determination
    # Print a summary of mdl_click_vs_impression_orig
        summary(mdl_click_vs_impression_orig)
    # Print a summary of mdl_click_vs_impression_trans
        summary(mdl_click_vs_impression_trans)
    # Get coeff of determination for mdl_click_vs_impression_orig
        mdl_click_vs_impression_orig %>% 
    # Get the model-level details
        glance(mdl_click_vs_impression_orig) %>% 
    # Pull out r.squared
        pull(r.squared)

    # Do the same for the transformed model
        mdl_click_vs_impression_trans %>%  glance(mdl_click_vs_impression_trans) %>% 
    # Pull out r.squared
        pull(r.squared)
#mdl_click_vs_impression_orig has a coefficient of determination of 0.89. Which statement about the model is true?
    The number of impressions explains 89% of the variability in the number of clicks.
#Which model does the coefficient of determination suggest gives a better fit?
    The transformed model, mdl_click_vs_impression_trans
#Residual standard error
    # Get RSE for mdl_click_vs_impression_orig
        mdl_click_vs_impression_orig %>% 
     # Get the model-level details
        glance(mdl_click_vs_impression_orig) %>% 
    # Pull out sigma
        pull(sigma)

    # Do the same for the transformed model
        mdl_click_vs_impression_trans %>% 
    # Get the model-level details
        glance(mdl_click_vs_impression_trans) %>% 
    # Pull out sigma
        pull(sigma)
    #mdl_click_vs_impression_orig has an RSE of 20. Which statement is true?
        The typical difference between observed number of clicks and predicted number of clicks is 20.
    #Which model does the RSE suggest gives more accurate predictions?
        The transformed model, mdl_click_vs_impression_trans.
#Look at the numbers on the y-axis scales, and how well the trend lines follow the  line. Which statement is true?
    The residuals track the " equals 0" line more closely in the transformed model compared to the original model, indicating that the transformed model is a better fit for the data.
#Look at how well the points track the "normality" line. Which statement is true?
    The residuals track the "normality" line more closely in the transformed model compared to the original model, indicating that the transformed model is a better fit for the data.
#Look at the numbers on the y-axis and the slope of the trend line. Which statement is true?
    The size of the standardized residuals is more consistent in the transformed model compared to the original model, indicating that the transformed model is a better fit for the data.
#Drawing diagnostic plots
    # Plot the three diagnostics for mdl_price_vs_conv
     library(ggplot2)
        library(ggfortify)
        autoplot(mdl_price_vs_conv, which=1:3,nrow=3,ncol=1)
#Leverage
    #Guess which observations you think will have a high leverage, then move the slider to find out.
    #Which statement is true?
        Observations with a large distance to the nearest MRT station have the highest leverage, because most of the observations have a short distance, so long distances are more extreme.
#Influence
    #Guess which observations you think will have a high influence, then move the slider to find out.
    #Which statement is true?
        Observations with predictions far away from the trend line have high influence, because they have large residuals and are far away from other observations.
#Extracting leverage and influence
         mdl_price_vs_dist %>% 
    # Augment the model
         augment() %>% 
    # Arrange rows by descending leverage
        arrange(desc(.hat)) %>% 
    # Get the head of the dataset
        head()
    mdl_price_vs_dist %>% 
    # Augment the model
        augment() %>% 
    # Arrange rows by descending Cook's distance
        arrange(desc(.cooksd)) %>% 
    # Get the head of the dataset
        head()
    # Plot the three outlier diagnostics for mdl_price_vs_conv
        autoplot(
         mdl_price_vs_dist,
         which = 4:6,
         nrow=3,
         ncol=1
        )
# Using churn, plot time_since_last_purchase
        ggplot(
        churn,
        aes(time_since_last_purchase)
        ) +
     # as a histogram with binwidth 0.25
         geom_histogram(binwidth=0.25)+
    # faceted in a grid with has_churned on each row
        facet_grid(rows=vars(has_churned))
    # Redraw the plot with time_since_first_purchase
        ggplot(
        churn,
        aes(time_since_first_purchase)
        ) + geom_histogram(binwidth=0.25)+
         facet_grid(rows=vars(has_churned))
#Visualizing linear and logistic models
    # Using churn plot has_churned vs. time_since_first_purchase
        ggplot(
        churn,
        aes(time_since_first_purchase,has_churned)
        ) +
         # Make it a scatter plot
        geom_point() +
        # Add an lm trend line, no std error ribbon, colored red
        geom_smooth(method="lm",se=FALSE,color="red")
         # Add a glm trend line, no std error ribbon, binomial family
        geom_smooth(method = "glm", se = FALSE, color = "red",    method.args=list(family=binomial))
#Logistic regression with glm()
    mdl_churn_vs_relationship <- glm(formula=has_churned~time_since_first_purchase,data=churn,family=binomial)
#Probabilities
    # Make a data frame of predicted probabilities
        prediction_data <- explanatory_data %>% 
        mutate(
        has_churned=predict(mdl_churn_vs_relationship,explanatory_data,
        type="response")
        )
    # Add points from prediction_data, colored yellow, size 2
        geom_point(data=prediction_data,color="yellow",size=2)
#Most likely outcome
    # Update the data frame
        prediction_data <- explanatory_data %>% 
        mutate(   
         has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    # Add the most likely churn outcome
          most_likely_outcome =round(has_churned)
        )

    # See the result
        prediction_data
     # Add most likely outcome points from prediction_data, 
     # colored yellow, size 2
        geom_point(aes(y=most_likely_outcome),data=prediction_data,color="yellow",size=2)
#odds Ratio
    # Add the odds ratio
         odds_ratio = has_churned/(1-has_churned)
    # Using prediction_data, plot odds_ratio vs. time_since_first_purchase
        ggplot(prediction_data,aes(time_since_first_purchase,odds_ratio)) +
      # Make it a line plot
         geom_line()  +
     # Add a dotted horizontal line at y = 1
         geom_hline(yintercept=1,linetype="dotted")
#Log odds ratio  
      # Add the log odds ratio from odds_ratio
            log_odds_ratio = log(odds_ratio),
        # Add the log odds ratio using predict()
            log_odds_ratio2 = predict(mdl_churn_vs_relationship,explanatory_data)
         )
     # Use a logarithmic y-scale
        scale_y_log10()
#Calculating the confusion matrix
    # Get the actual responses from the dataset
        actual_response <- churn$has_churned

    # Get the "most likely" responses from the model
        predicted_response <- round(fitted(mdl_churn_vs_relationship))

    # Create a table of counts
        outcomes <- table(predicted_response,actual_response)

    #   See the result
        outcomes
#Measuring logistic model performance
    # Convert outcomes to a yardstick confusion matrix
        confusion <- conf_mat(outcomes)

    # Plot the confusion matrix

        autoplot(confusion)

    # Get performance metrics for the confusion matrix
        summary(confusion,event_level="second")
