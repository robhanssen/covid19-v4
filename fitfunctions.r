#
# fitfunctions.r
#
# definitions of functions to help with the curve fitting of exponential growth curves 
#


exponential_fit <- function(df, location, time_start, time_stop)
{
    fit = lm(log10(df$count[df$location==location & df$time > time_start & df$time < time_stop]) ~ df$time[df$location==location & df$time > time_start & df$time < time_stop])

    return(fit)
}

exponential_fit_prediction <- function(df, fit, location, time_start, time_stop)
{
    time = df$time[df$location==location & df$time > time_start & df$time < time_stop]
    prediction <- 10^(predict(fit, list=time))
    pred = tibble(time)
    pred$count = prediction
    pred$location = "Prediction"

    return(pred)
}

exponential_fit_rate <- function(fit)
{
    B = fit$coefficients[2]
    rate = round(log10(2)/B,2)
    
    return(rate)
}

fitline <- function(fit, time_start, time_stop)
{
    A0 = fit$coefficients[1]
    B0 = fit$coefficients[2]

    ts = time_start + 21
    time = time_start:ts
    count = 10^(A0+B0*time)
    df = tibble(time, count)
    df$location="zPrediction"

    return(df)
}