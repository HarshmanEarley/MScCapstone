inc_gdcFloat <- '
  // Recursive function to return gcd of a and b 
  double gcd(double a, double b){
    //Invert if a < b
    if (a < b){
        return gcd(b, a); 
    }
    // base case 
    if (fabs(b) < 0.0000001){
        return a;
    }
        
    return(gcd(b, a - floor(a / b) * b));
  }
'

body_gdcFloat <- '
  // CPP code for finding the GCD of two floating 
  // numbers. 
  //#include <bits/stdc++.h> 
  using namespace std; 
  double a = as<double>(arg_a);
  double b = as<double>(arg_b);
  
  return(wrap(gcd(a,b)));
'

ccp_gdcFloat <- cxxfunction(signature(arg_a= "double", arg_b= "double"),
                            body = body_gdcFloat,
                            includes = inc_gdcFloat,
                            plugin = "Rcpp")

# for 1:lseed iterations, call RandomWalk with specified seed for reproducability
ccpwrap_gdcFloat = function(a,b){
  ccp_gdcFloat(a,b)
}

