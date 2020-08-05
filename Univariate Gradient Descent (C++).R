library(Rcpp)
library(inline)

# This is a univariate Gradient Descent algorithm, written in 
# C++, with the help of the Rcpp and inline packages, and 
# compiled via cxxfunction.

# body_GradDesc takes in an initial theta value, alpha value and tolerance level. Alpha is the learning rate for the function, and tolerance
# is the value at which the algorithm will be judged to have reached a minimum.

# It will take in two functions, arbitrarily given below in character strings as F1 and F1_prime.
# The function F1_prime is just the derivative of F1, which is needed to calculate the steps which the algorithm will take.

# F1 and F1_prime are called into cxxfunction via the Includes parameter, and can be changed and reloaded, depending on the 
# function desired. 

# The functions which we will use for F1 and F1_prime are given below:


F1 <- '

double f1( const double &theta )
{
  
  double f_theta = 1+4*(pow( ( theta + 4 ), 2 ) );
  
  return f_theta;
  
}

'


F1_prime <- '

  double f1_prime( const double &theta )
{

  double f_prime_theta = 8 * ( theta + 4 );
  
  return f_prime_theta;

}

'

body_GradDesc <- '

  
  double theta0 = as<double>(theta);                  // Takes in theta as theta0.
  
  double alpha0 = as<double>(alpha);                  // Takes in alpha as alpha0. Alpha is the learning rate.
  
  double tol0   = as<double>(tol);                    // Takes in tolerance as tol0. Tolerance is when the algorithm reaches convergence.
  
  double f_theta_new = f1( theta0 );                  // Assigns our first f(theta)
                                                      // value to a variable called
                                                      // f_theta_new.
  
  std::vector<double> theta_vec;                      // Calls a vector from the STL.
  
  theta_vec.push_back(theta0);                        // Assigns our input theta as the
                                                      // first value in our vector.
  
  double rel_ch = 1;                                  // Initialises relative change
                                                      // to be equal to 1.
  
  int i = 0;                                          // Initialises counter i to be 
                                                      // equal to 0.
  
  while(rel_ch > tol0){
  
    i = i + 1;                                        // Increments i by 1.
  
    double theta_new = theta_vec[i - 1] - (alpha0 * f1_prime( theta_vec[ i - 1 ] ) );
    // Above assigns a new theta value for each iteration, 
    // using formula NextTheta = PreviousTheta - ( alpha )( f_prime( PreviousTheta ) ).
    
    theta_vec.push_back(theta_new);
    // Above adds the new theta value to the end of our theta vector theta_vec.
    
    
    double f_theta_old = f_theta_new;                // Stores the old value of the
                                                     // target function.          
    
    f_theta_new = f1( theta_vec[ i ] );          // Stores new value of target
                                                     // function for i_th iteration.
    
    
    rel_ch = fabs( ( f_theta_new - f_theta_old ) / f_theta_old );
    // Calculates absolute value of Relative change for i_th iteration.
  
  }
  
  return( wrap( 
    List::create(
      _["Optimal_Theta_Value"] = theta_vec[i],
      _["Theta_Values_Vector"] = theta_vec,
      _["Iteration"]           = i,
      _["Minimum_Target_Function_Value"] = f_theta_new
      ) ) );
  
  // Above returns a list of Optimal Theta Value, returning the final theta value at the
  // i_th iteration of the while loop, the entire vector of theta values, the number of
  // the iteration, and the minimum of the target function.
  
'


# Below uses cxxfunction to generate the gradient descent function, called GradDesc.
# The function uses includes my_objfun ( a random f(theta) function ) and my_derivfun,
# the derivative of that f(theta) function.

GradDesc <- cxxfunction(signature(theta = 'numeric',
                                alpha = 'numeric',
                                tol = 'numeric'),
                      body = body_GradDesc,
                      include = c(F1, F1_prime),
                      plugin = 'Rcpp')


GradDesc(theta = as.numeric(10), alpha = as.numeric(0.01), tol = as.numeric(0.00000001))
# Test: returns optimal theta value of -3.99989, on 141st iteration.



# Now a version with Bold Driver Method, i.e. a learning rate which can change.


body_GradDescBoldDriver <- '

  double theta0 = as<double>(theta);
  double alpha0 = as<double>(alpha);
  double tol0   = as<double>(tol);
  
  double f_theta_new = f1( theta0 );
  
  std::vector<double> theta_vec;
  theta_vec.push_back(theta0);
  
  std::vector<double> alpha_vec;    // Instantiate STL vector to store alpha learning rates,
                                    // just for reference.
  
  alpha_vec.push_back(alpha0);      // Assign inputted alpha to be first element of alpha vector.
  
  double rel_ch = 1;
  int i = 0;
  
  while(rel_ch > tol0){
  
    i = i + 1;
  
    double theta_new = theta_vec[i - 1] - (alpha0 * f1_prime( theta_vec[ i - 1 ] ) );
    
    theta_vec.push_back(theta_new);
    
    double f_theta_old = f_theta_new;
    f_theta_new = f1( theta_vec[ i ] );
    
    
    rel_ch = fabs( ( f_theta_new - f_theta_old ) / f_theta_old );
  
    // Below initiates an if statement. 
    // If we continue to decrease (ie f(NextTheta) - f(PreviousTheta) < 0),
    // then we increase learning rate alpha0 by 1.05.
    // Else, if we begin to increase, ie we have overshot our minimum, then we 
    // decrease learning rate alpha0 by half.
    
    
    if(f_theta_new - f_theta_old < 0){
      
      alpha0 *= 1.05;
      
    }else{
      
      alpha0 = alpha0 / 2;    
      
    }
    
    // We then store our alpha values in a vector, just for reference.
    
    alpha_vec.push_back(alpha0);
  
  }
  
  return( wrap( 
    List::create(
      _["Optimal_Theta_Value"]    = theta_vec[i],
      _["Theta_Values_Vector"]    = theta_vec,
      _["Iteration"]              = i,
      _["Minimum_Target_Function_Value"] = f_theta_new,
      _["Alpha_Values_Vector"]    = alpha_vec
      ) ) );
      
  // Output is identical to above, but also includes the vector of alpha values.
  
'

GradDescBoldDriver <- cxxfunction(signature(theta = 'numeric',
                                            alpha = 'numeric',
                                            tol = 'numeric'),
                                  body = body_GradDescBoldDriver,
                                  include = c(F1, F1_prime),
                                  plugin = 'Rcpp')


GradDescBoldDriver(theta = as.numeric(10), alpha = as.numeric(0.01), tol = as.numeric(0.00000001))

# Test: returns optimal theta value of -3.99998, on 42nd iteration, with alpha of 0.07761588.
