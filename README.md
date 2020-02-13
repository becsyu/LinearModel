# Linear Regression, Random Effects and Hierarchical Linear Analysis
In marketing we often encouter sales data that want to account for the promotion, competition and holiday effect on sales. Finance folks might also come across customer retension/activation problems where we try to understand how to best offer our performance, to the right customer, at the right time. Perhaps the best and the easiest place to start is a linear model, with a log transformation. 

Here I will show 2 exercises I did using linear models, from the most basic linear models to mixed effects, and how to preliminarily select the interactions, how to evaluate model efficiency and decide which model is better for particular situations. Data is made-up, courtesy of McCombs Business School, department of Marketing.

## Sales Data
We will use a simple dataset to evaluate the impact of the opening of a new Walmart on the sales a local grocery store. Suppose that you have been hired as a consultant for the local grocery store. Store management is worried since Wal-Mart has entered the market by opening a "Wal-Mart Super-center" only 3 miles away. The management is interested in analyzing the impact on store sales after Wal-Mart's entry. 

For the analysis, management has given you access to 50 weeks of sales data before the entry of Walmart and 50 weeks after. 

## Credit Card Data
In this exercise, we will use hierarchical linear models and regressions with random effects for an analytics problem from a credit card company. The credit card company would like to figure out whether offering more promotions (for example, gasoline rebates and coupons for using the credit card) to their existing customers can increase the share-of-wallet of the credit card (that is, the share of a consumer's monthly spending using the credit card in her total spending). The company would also like to figure out what customer characteristics make them more responsive to promotions. 

The company conducted a field experiment by randomly selecting 300 customers and offering them different monthly promotions for 12 months. The share-of-wallet data were recorded in each month for every customer. The data set also included some consumer characteristics. 

## Contact
Please contact me at sijia.yu@utexas.edu if you have further questions. 
